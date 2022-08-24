#![allow(warnings)]

use cosmic_api::command::request::create::{PointFactory, PointFactoryU64, PointSegTemplate};
use cosmic_api::error::MsgErr;
use cosmic_api::frame::frame::PrimitiveFrame;
use cosmic_api::id::id::{Layer, Point, Port, ToPoint, ToPort, Version};
use cosmic_api::log::{PointLogger, RootLogger};
use cosmic_api::msg::MsgMethod;
use cosmic_api::particle::particle::Status;
use cosmic_api::substance::substance::{Errors, Substance, SubstanceKind, Token};
use cosmic_api::sys::{Greet, InterchangeKind, Knock, Sys};
use cosmic_api::util::uuid;
use cosmic_api::wave::{
    Agent, DirectedKind, DirectedProto, Exchanger, Handling, HyperWave, Method, Ping, Pong,
    ProtoTransmitter, ProtoTransmitterBuilder, Reflectable, ReflectedKind, ReflectedProto,
    ReflectedWave, Router, SetStrategy, SysMethod, TxRouter, UltraWave, Wave, WaveId, WaveKind,
};
use cosmic_api::VERSION;
use dashmap::DashMap;
use futures::future::select_all;
use futures::FutureExt;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use tokio::io::AsyncWriteExt;
use tokio::select;
use tokio::sync::mpsc::error::{SendError, SendTimeoutError, TrySendError};
use tokio::sync::mpsc::Receiver;
use tokio::sync::oneshot::Sender;
use tokio::sync::{broadcast, mpsc, oneshot, watch, Mutex, RwLock};
use cosmic_api::quota::Timeouts;

#[macro_use]
extern crate async_trait;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    pub static ref LOCAL_CLIENT: Point = Point::from_str("LOCAL::client").expect("point");
    pub static ref LOCAL_CLIENT_RUNNER: Point =
        Point::from_str("LOCAL::client:runner").expect("point");
}

pub enum HyperwayKind {
    Mount,
    Ephemeral,
}

pub struct Hyperway {
    pub remote: Port,
    outbound: Hyperlane,
    inbound: Hyperlane,

    #[cfg(test)]
    pub diagnostic: HyperwayDiagnostic,
}

impl Hyperway {
    pub fn new(remote: Port, agent: Agent) -> Self {
        let mut inbound = Hyperlane::new("inbound");
        inbound
            .tx
            .try_send(HyperlaneCall::Transform(Box::new(FromTransform::new(
                remote.clone(),
            ))));
        inbound
            .tx
            .try_send(HyperlaneCall::Transform(Box::new(AgentTransform::new(
                agent,
            ))));
        Self {
            remote,
            outbound: Hyperlane::new("outbound"),
            inbound,
            #[cfg(test)]
            diagnostic: HyperwayDiagnostic::new(),
        }
    }

    pub fn transform_inbound(&self, transform: Box<dyn HyperTransform>) {
        self.inbound
            .tx
            .try_send(HyperlaneCall::Transform(transform));
    }

    pub fn transform_to(&self, to: Port) {
        self.inbound
            .tx
            .try_send(HyperlaneCall::Transform(Box::new(ToTransform::new(to))));
    }

    pub async fn mount(&self, init_wave: Option<UltraWave>) -> HyperwayExt {
        let drop_tx = None;

        HyperwayExt {
            tx: self.inbound.tx(),
            rx: self.outbound.rx(init_wave).await,
            drop_tx,
        }
    }

    pub async fn ephemeral(
        &self,
        drop_tx: oneshot::Sender<()>,
        init_wave: Option<UltraWave>,
    ) -> HyperwayExt {
        let drop_tx = Some(drop_tx);

        HyperwayExt {
            tx: self.outbound.tx(),
            rx: self.inbound.rx(init_wave).await,
            drop_tx,
        }
    }

    pub async fn channel(&self) -> (mpsc::Sender<UltraWave>, mpsc::Receiver<UltraWave>) {
        (self.outbound.tx(), self.outbound.rx(None).await)
    }
}

#[cfg(test)]
pub struct HyperwayDiagnostic {
    pub replaced_ext: broadcast::Sender<Result<(), MsgErr>>,
}

#[cfg(test)]
impl HyperwayDiagnostic {
    pub fn new() -> Self {
        let (replaced_ext, _) = broadcast::channel(128);
        Self { replaced_ext }
    }
}

pub struct HyperwayExt {
    drop_tx: Option<oneshot::Sender<()>>,
    pub tx: mpsc::Sender<UltraWave>,
    pub rx: mpsc::Receiver<UltraWave>,
}

impl HyperwayExt {
    pub fn new(tx: mpsc::Sender<UltraWave>, rx: mpsc::Receiver<UltraWave>) -> Self {
        let drop_tx = None;
        Self { tx, rx, drop_tx }
    }

    pub fn new_with_drop(
        tx: mpsc::Sender<UltraWave>,
        rx: mpsc::Receiver<UltraWave>,
        drop_tx: oneshot::Sender<()>,
    ) -> Self {
        let drop_tx = Some(drop_tx);
        Self { tx, rx, drop_tx }
    }

    pub fn add_drop_tx(&mut self, drop_tx: oneshot::Sender<()>) {
        self.drop_tx.replace(drop_tx);
    }

    pub fn router(&self) -> TxRouter {
        TxRouter::new(self.tx.clone())
    }
}

impl Drop for HyperwayExt {
    fn drop(&mut self) {
        match self.drop_tx.take() {
            None => {}
            Some(drop_tx) => {
                drop_tx.send(());
            }
        }
    }
}

#[derive(Clone)]
pub struct HyperwayStub {
    pub agent: Agent,
    pub remote: Port,
}

impl From<Greet> for HyperwayStub {
    fn from(greet: Greet) -> Self {
        Self {
            agent: greet.agent,
            remote: greet.port,
        }
    }
}

impl HyperwayStub {
    pub fn from_port(remote: Port) -> Self {
        Self {
            agent: remote.to_agent(),
            remote,
        }
    }

    pub fn new(remote: Port, agent: Agent) -> Self {
        Self { agent, remote }
    }
}

pub enum HyperwayInterchangeCall {
    Wave(UltraWave),
    Internal(Hyperway),
    Remove(Port),
    Mount {
        stub: HyperwayStub,
        init_wave: UltraWave,
        rtn: oneshot::Sender<Result<HyperwayExt, MsgErr>>,
    },
}

pub enum HyperlaneCall {
    Drain,
    Ext(mpsc::Sender<UltraWave>),
    ResetExt,
    Wave(UltraWave),
    Transform(Box<dyn HyperTransform>),
}

pub trait HyperTransform: Send + Sync {
    fn filter(&self, wave: UltraWave) -> UltraWave;
}

#[derive(Clone)]
pub struct AgentTransform {
    agent: Agent,
}


impl AgentTransform {
    pub fn new(agent: Agent) -> Self {
        Self { agent }
    }
}

impl HyperTransform for AgentTransform {
    fn filter(&self, mut wave: UltraWave) -> UltraWave {
        wave.set_agent(self.agent.clone());
        wave
    }
}

#[derive(Clone)]
pub struct LayerTransform {
    layer: Layer,
}

impl LayerTransform {
    pub fn new(layer: Layer) -> Self {
        Self { layer }
    }
}

impl HyperTransform for LayerTransform {
    fn filter(&self, mut wave: UltraWave) -> UltraWave {
        let to = wave
            .to()
            .clone()
            .to_single()
            .unwrap()
            .with_layer(self.layer.clone());
        wave.set_to(to);
        wave
    }
}

#[derive(Clone)]
pub struct TransportTransform {
    transport_to: Port
}

impl TransportTransform {
    pub fn new(transport_to: Port) -> Self {
        Self{
            transport_to
        }
    }
}

impl HyperTransform for TransportTransform {
    fn filter(&self, wave: UltraWave) -> UltraWave {
        let from = wave.from().clone();
        let transport = wave.wrap_in_transport(from,self.transport_to.clone());
        let wave = transport.build().unwrap();
        wave.to_ultra()
    }
}

#[derive(Clone)]
pub struct HopTransform {
    hop_to: Port
}

impl HopTransform {
    pub fn new(hop_to: Port) -> Self {
        Self{
            hop_to
        }
    }
}

impl HyperTransform for HopTransform {
    fn filter(&self, wave: UltraWave) -> UltraWave {
        let signal = wave.to_signal().unwrap();
        let from = signal.from.clone();
        let wave = signal.wrap_in_hop(from,self.hop_to.clone());
        let wave = wave.build().unwrap();
        wave.to_ultra()
    }
}

pub struct ToTransform {
    to: Port,
}

impl ToTransform {
    pub fn new(to: Port) -> Self {
        Self { to }
    }
}

impl HyperTransform for ToTransform {
    fn filter(&self, mut wave: UltraWave) -> UltraWave {
        wave.set_to(self.to.clone());
        wave
    }
}

pub struct FromTransform {
    from: Port,
}

impl FromTransform {
    pub fn new(from: Port) -> Self {
        Self { from }
    }
}

impl HyperTransform for FromTransform {
    fn filter(&self, mut wave: UltraWave) -> UltraWave {
        wave.set_from(self.from.clone());
        wave
    }
}
#[derive(Clone)]
pub struct Hyperlane {
    tx: mpsc::Sender<HyperlaneCall>,
    #[cfg(test)]
    eavesdrop_tx: broadcast::Sender<UltraWave>,
    label: &'static str,
}

impl Hyperlane {
    pub fn new(label: &'static str) -> Self {
        #[cfg(test)]
        let (eavesdrop_tx, _) = broadcast::channel(16);

        let (tx, mut rx) = mpsc::channel(1024);
        {
            let tx = tx.clone();
            #[cfg(test)]
            let eavesdrop_tx = eavesdrop_tx.clone();

            tokio::spawn(async move {
                let mut ext = None;
                let mut queue = vec![];
                let mut transforms = vec![];
                while let Some(call) = rx.recv().await {
                    match call {
                        HyperlaneCall::Ext(new) => {
                            ext.replace(new);
                        }
                        HyperlaneCall::Transform(filter) => {
                            transforms.push(filter);
                        }
                        HyperlaneCall::Wave(mut wave) => {
                            while queue.len() > 1024 {
                                // start dropping the oldest messages
                                queue.remove(0);
                            }
                            for transform in transforms.iter() {
                                wave = transform.filter(wave);
                            }
                            queue.push(wave);
                        }
                        HyperlaneCall::Drain => {
                            // just drains the queue later if there is a listener
                        }
                        HyperlaneCall::ResetExt => {
                            ext = None;
                        }
                    }
                    if let Some(ext_tx) = ext.as_mut() {
                        for wave in queue.drain(..) {
                            #[cfg(test)]
                            let wave_cp = wave.clone();
                            match ext_tx.send(wave).await {
                                Ok(_) => {
                                    #[cfg(test)]
                                    eavesdrop_tx.send(wave_cp);
                                }
                                Err(err) => {
                                    tx.send(HyperlaneCall::ResetExt).await;
                                    tx.try_send(HyperlaneCall::Wave(err.0));
                                }
                            }
                        }
                    } else {
                    }
                }
            });
        }

        Self {
            tx,
            label,
            #[cfg(test)]
            eavesdrop_tx,
        }
    }

    #[cfg(test)]
    pub fn eavesdrop(&self) -> broadcast::Receiver<UltraWave> {
        self.eavesdrop_tx.subscribe()
    }

    pub async fn send(&self, wave: UltraWave) -> Result<(), MsgErr> {
        Ok(self
            .tx
            .send_timeout(HyperlaneCall::Wave(wave), Duration::from_secs(5))
            .await?)
    }

    pub fn tx(&self) -> mpsc::Sender<UltraWave> {
        let (tx, mut rx) = mpsc::channel(1024);
        let call_tx = self.tx.clone();
        tokio::spawn(async move {
            while let Some(wave) = rx.recv().await {
                call_tx.send(HyperlaneCall::Wave(wave)).await;
            }
        });
        tx
    }

    pub async fn rx(&self, init_wave: Option<UltraWave>) -> mpsc::Receiver<UltraWave> {
        let (tx, rx) = mpsc::channel(1024);
        if let Some(init_wave) = init_wave {
            tx.send(init_wave).await;
        }
        self.tx.send(HyperlaneCall::Ext(tx)).await;
        rx
    }
}

pub struct HyperwayInterchange {
    call_tx: mpsc::Sender<HyperwayInterchangeCall>,
    logger: PointLogger,
    singular_to: Option<Port>
}

impl HyperwayInterchange {
    pub fn new(logger: PointLogger) -> Self {
        let (call_tx, mut call_rx) = mpsc::channel(1024);

        {
            let call_tx = call_tx.clone();
            let logger = logger.clone();
            tokio::spawn(async move {
                let mut hyperways = HashMap::new();
                while let Some(call) = call_rx.recv().await {
                    match call {
                        HyperwayInterchangeCall::Internal(hyperway) => {
                            let mut rx = hyperway.inbound.rx(None).await;
                            hyperways.insert(hyperway.remote.clone(), hyperway);
                            let call_tx = call_tx.clone();
                            tokio::spawn(async move {
                                while let Some(wave) = rx.recv().await {
                                    call_tx
                                        .send_timeout(
                                            HyperwayInterchangeCall::Wave(wave),
                                            Duration::from_secs(60u64),
                                        )
                                        .await;
                                }
                            });
                        }
                        HyperwayInterchangeCall::Remove(point) => {
                            hyperways.remove(&point);
                        }
                        HyperwayInterchangeCall::Wave(wave) => match wave.to().single_or() {
                            Ok(to) => match hyperways.get(&to) {
                                None => {}
                                Some(hyperway) => {
                                    hyperway.outbound.send(wave).await;
                                }
                            },
                            Err(_) => {
                                logger.warn("Hyperway Interchange cannot route Ripples, instead wrap in a Hop or Transport");
                            }
                        },
                        HyperwayInterchangeCall::Mount {
                            stub,
                            init_wave,
                            rtn,
                        } => {

                            match hyperways.get(&stub.remote) {
                                None => {
                                    logger.error(format!(
                                        "hyperway {} not found",
                                        stub.remote.to_string()
                                    ));
                                    rtn.send(Err(format!(
                                        "hyperway {} not found",
                                        stub.remote.to_string()
                                    )
                                    .into()));
                                }
                                Some(hyperway) => {
                                    rtn.send(Ok(hyperway.mount(Some(init_wave)).await));
                                }
                            }
                        }
                    }
                }
            });
        }

        Self {
            call_tx,
            logger,
            singular_to: None
        }
    }

    pub fn router(&self) -> Box<dyn Router> {
        Box::new(OutboundRouter::new(self.call_tx.clone()))
    }

    pub fn point(&self) -> &Point {
        &self.logger.point
    }

    pub async fn mount(
        &self,
        stub: HyperwayStub,
        init_wave: UltraWave,
    ) -> Result<HyperwayExt, MsgErr> {
        let call_tx = self.call_tx.clone();
        let (tx, rx) = oneshot::channel();
        call_tx
            .send(HyperwayInterchangeCall::Mount {
                stub: stub.clone(),
                init_wave,
                rtn: tx,
            })
            .await;
        rx.await?
    }

    pub fn singular_to( &mut self, to: Port ) {
        self.singular_to.replace(to);
    }

    pub async fn add(&self, mut hyperway: Hyperway) {
        if let Some(to) = self.singular_to.as_ref() {
            hyperway.transform_to(to.clone());
        }
        self.call_tx
            .send(HyperwayInterchangeCall::Internal(hyperway))
            .await;
    }

    pub fn remove(&self, hyperway: Port) {
        let call_tx = self.call_tx.clone();
        tokio::spawn(async move {
            call_tx
                .send(HyperwayInterchangeCall::Remove(hyperway))
                .await;
        });
    }

    pub async fn route(&self, wave: UltraWave) {
        self.call_tx.send(HyperwayInterchangeCall::Wave(wave)).await;
    }
}

#[async_trait]
pub trait HyperRouter: Send + Sync {
    async fn route(&self, wave: HyperWave);
}

pub struct OutboundRouter {
    pub call_tx: mpsc::Sender<HyperwayInterchangeCall>,
}

impl OutboundRouter {
    pub fn new(call_tx: mpsc::Sender<HyperwayInterchangeCall>) -> Self {
        Self { call_tx }
    }
}

#[async_trait]
impl Router for OutboundRouter {
    async fn route(&self, wave: UltraWave) {
        self.call_tx.send(HyperwayInterchangeCall::Wave(wave)).await;
    }

    fn route_sync(&self, wave: UltraWave) {
        self.call_tx.try_send(HyperwayInterchangeCall::Wave(wave));
    }
}

#[async_trait]
pub trait HyperGreeter: Send + Sync + Clone + Sized {
    async fn greet(&self, stub: HyperwayStub) -> Result<Greet, MsgErr>;
}

#[derive(Clone)]
pub struct SimpleGreeter {
    hop: Port,
    transport: Port,
}

impl SimpleGreeter {
    pub fn new(hop: Port, transport: Port) -> Self {
        Self { hop, transport }
    }
}

#[async_trait]
impl HyperGreeter for SimpleGreeter {
    async fn greet(&self, stub: HyperwayStub) -> Result<Greet, MsgErr> {
        Ok(Greet {
            port: stub.remote,
            agent: stub.agent,
            hop: self.hop.clone(),
            transport: self.transport.clone(),
        })
    }
}

#[async_trait]
pub trait HyperAuthenticator: Send + Sync + Clone + Sized {
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, HyperConnectionErr>;
}

#[derive(Clone)]
pub struct TokenAuthenticator {
    pub token: Token,
    pub agent: Agent,
}

impl TokenAuthenticator {
    pub fn new(agent: Agent, token: Token) -> Self {
        Self { agent, token }
    }
}

#[async_trait]
impl HyperAuthenticator for TokenAuthenticator {
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, HyperConnectionErr> {
        if let Substance::Token(token) = &*knock.auth {
            if *token == self.token {
                Ok(HyperwayStub {
                    agent: self.agent.clone(),
                    remote: knock
                        .remote
                        .ok_or::<MsgErr>("expected a remote entry selection".into())?,
                })
            } else {
                Err(HyperConnectionErr::Fatal("invalid token".to_string()))
            }
        } else {
            Err(HyperConnectionErr::Fatal(
                "expected Subtance: Token".to_string(),
            ))
        }
    }
}

#[derive(Clone)]
pub struct AnonHyperAuthenticator;

impl AnonHyperAuthenticator {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Clone)]
pub struct TokenAuthenticatorWithRemoteWhitelist {
    pub token: Token,
    pub agent: Agent,
    pub whitelist: HashSet<Point>,
}

impl TokenAuthenticatorWithRemoteWhitelist {
    pub fn new(agent: Agent, token: Token, whitelist: HashSet<Point>) -> Self {
        Self {
            agent,
            token,
            whitelist,
        }
    }
}

#[async_trait]
impl HyperAuthenticator for TokenAuthenticatorWithRemoteWhitelist {
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, HyperConnectionErr> {
        if let Substance::Token(token) = &*knock.auth {
            if *token == self.token {
                let remote = knock.remote.ok_or(HyperConnectionErr::Fatal(
                    "expected a remote entry selection".to_string(),
                ))?;
                if self.whitelist.contains(&remote) {
                    Ok(HyperwayStub {
                        agent: self.agent.clone(),
                        remote,
                    })
                } else {
                    Err(HyperConnectionErr::Fatal(
                        "remote is not part of the whitelist".to_string(),
                    ))
                }
            } else {
                Err(HyperConnectionErr::Fatal("invalid token".to_string()))
            }
        } else {
            Err(HyperConnectionErr::Fatal(
                "expecting Substance: Token".to_string(),
            ))
        }
    }
}

#[async_trait]
impl HyperAuthenticator for AnonHyperAuthenticator {
    async fn auth(&self, req: Knock) -> Result<HyperwayStub, HyperConnectionErr> {
        let remote = req.remote.ok_or(HyperConnectionErr::Fatal(
            "required remote point request".to_string(),
        ))?;

        Ok(HyperwayStub {
            agent: Agent::Anonymous,
            remote,
        })
    }
}

#[derive(Clone)]
pub struct AnonHyperAuthenticatorAssignEndPoint {
    pub logger: PointLogger,
    pub remote_point_factory: Arc<dyn PointFactory>,
}

impl AnonHyperAuthenticatorAssignEndPoint {
    pub fn new(remote_point_factory: Arc<dyn PointFactory>, logger: PointLogger) -> Self {
        Self {
            remote_point_factory,
            logger,
        }
    }
}

#[async_trait]
impl HyperAuthenticator for AnonHyperAuthenticatorAssignEndPoint {
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, HyperConnectionErr> {
        let remote = self
            .logger
            .result(self.remote_point_factory.create().await)?
            .to_port();
        Ok(HyperwayStub {
            agent: Agent::Anonymous,
            remote,
        })
    }
}

#[derive(Clone)]
pub struct TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    pub logger: RootLogger,
    pub tokens: Arc<DashMap<Token, HyperwayStub>>,
}

impl TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    pub fn new(tokens: Arc<DashMap<Token, HyperwayStub>>, logger: RootLogger) -> Self {
        Self { logger, tokens }
    }
}

#[async_trait]
impl HyperAuthenticator for TokensFromHeavenHyperAuthenticatorAssignEndPoint {
    async fn auth(&self, auth_req: Knock) -> Result<HyperwayStub, HyperConnectionErr> {
        match &*auth_req.auth {
            Substance::Token(token) => {
                if let Some((_, stub)) = self.tokens.remove(token) {
                    return Ok(stub);
                } else {
                    return Err(HyperConnectionErr::Fatal("invalid token".to_string()));
                }
            }
            _ => {
                return Err(HyperConnectionErr::Fatal(
                    "expected Substance: Token".to_string(),
                ));
            }
        }
    }
}

pub struct TokenDispensingHyperwayInterchange {
    pub agent: Agent,
    pub logger: PointLogger,
    pub tokens: Arc<DashMap<Token, HyperwayStub>>,
    pub lane_point_factory: Box<dyn PointFactory>,
    pub remote_point_factory: Box<dyn PointFactory>,
    pub interchange: HyperwayInterchange,
}

impl TokenDispensingHyperwayInterchange {
    pub fn new(
        agent: Agent,
        router: Box<dyn HyperRouter>,
        lane_point_factory: Box<dyn PointFactory>,
        end_point_factory: Box<dyn PointFactory>,
        logger: PointLogger,
    ) -> Self {
        let tokens = Arc::new(DashMap::new());
        let authenticator = Box::new(TokensFromHeavenHyperAuthenticatorAssignEndPoint::new(
            tokens.clone(),
            logger.logger.clone(),
        ));
        let interchange = HyperwayInterchange::new(logger.clone());
        Self {
            agent,
            tokens,
            logger,
            lane_point_factory,
            remote_point_factory: end_point_factory,
            interchange,
        }
    }

    pub async fn dispense(&self) -> Result<(Token, HyperwayStub), MsgErr> {
        let token = Token::new_uuid();
        let remote_point = self.remote_point_factory.create().await?.to_port();
        let lane_point = self.lane_point_factory.create().await?;
        let logger = self.logger.point(lane_point);
        let stub = HyperwayStub {
            agent: self.agent.clone(),
            remote: remote_point,
        };
        self.tokens.insert(token.clone(), stub.clone());
        Ok((token, stub))
    }
}

impl Deref for TokenDispensingHyperwayInterchange {
    type Target = HyperwayInterchange;

    fn deref(&self) -> &Self::Target {
        &self.interchange
    }
}

impl DerefMut for TokenDispensingHyperwayInterchange {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.interchange
    }
}

pub struct VersionGate {
    router: HyperGateSelector,
}

impl VersionGate {
    pub async fn unlock(&self, version: semver::Version) -> Result<HyperGateSelector, String> {
        if version == *VERSION {
            Ok(self.router.clone())
        } else {
            Err("version mismatch".to_string())
        }
    }
}

#[async_trait]
pub trait HyperGate: Send + Sync {
    async fn knock(&self, knock: Knock) -> Result<HyperwayExt, HyperConnectionErr>;

    async fn jump(
        &self,
        kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayExt, HyperConnectionErr>;
}

pub struct HopRouter {
    greet: Greet,
    tx: mpsc::Sender<UltraWave>,
}

impl HopRouter {
    fn to_hop(&self, mut wave: UltraWave) -> Result<UltraWave, MsgErr> {
        wave.set_agent(self.greet.agent.clone());
        let mut transport = wave
            .wrap_in_transport(self.greet.port.clone(), self.greet.transport.clone())
            .build()?
            .to_signal()?;
        let hop = transport
            .wrap_in_hop(Point::local_portal().to_port(), self.greet.hop.clone())
            .build()?
            .to_ultra();
        Ok(hop)
    }
}

#[async_trait]
impl Router for HopRouter {
    async fn route(&self, wave: UltraWave) {
        match self.to_hop(wave) {
            Ok(hop) => {
                self.tx.send(hop).await.unwrap_or_default();
            }
            Err(err) => {
                println!("{}", err.to_string());
            }
        }
    }

    fn route_sync(&self, wave: UltraWave) {
        match self.to_hop(wave) {
            Ok(hop) => {
                self.tx.try_send(hop).unwrap_or_default();
            }
            Err(err) => {
                println!("{}", err.to_string());
            }
        }
    }
}

pub struct HyperApi {
    greet: Greet,
    hyperway: HyperwayExt,
    exchanger: Exchanger,
}

impl HyperApi {
    pub fn new(hyperway: HyperwayExt, greet: Greet) -> Self {
        let exchanger = Exchanger::new(greet.port.clone(), Default::default());
        Self {
            greet,
            hyperway,
            exchanger,
        }
    }

    pub fn router(&self) -> HopRouter {
        HopRouter {
            greet: self.greet.clone(),
            tx: self.hyperway.tx.clone(),
        }
    }

    pub fn transmitter(&self) -> ProtoTransmitter {
        let mut builder =
            ProtoTransmitterBuilder::new(Arc::new(self.router()), self.exchanger.clone());
        builder.agent = SetStrategy::Override(self.greet.agent.clone());
        builder.build()
    }
}

#[derive(Clone)]
pub struct HyperGateSelector {
    map: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>>,
}

impl HyperGateSelector {
    pub fn new(map: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>>) -> Self {
        Self { map }
    }

    pub fn add(&self, kind: InterchangeKind, gate: Arc<dyn HyperGate>) -> Result<(), MsgErr> {
        if self.map.contains_key(&kind) {
            Err(format!("already have an interchange of kind: {}", kind.to_string()).into())
        } else {
            self.map.insert(kind, gate);
            Ok(())
        }
    }
}

#[async_trait]
impl HyperGate for HyperGateSelector {
    async fn knock(&self, knock: Knock) -> Result<HyperwayExt, HyperConnectionErr> {
        if let Some(gate) = self.map.get(&knock.kind) {
            gate.value().knock(knock).await
        } else {
            Err(HyperConnectionErr::Fatal(format!(
                "interchange not available: {}",
                knock.kind.to_string()
            )))
        }
    }

    async fn jump(
        &self,
        kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayExt, HyperConnectionErr> {
        self.map
            .get(&kind)
            .ok_or(HyperConnectionErr::Fatal(format!(
                "interchange kind not available: {}",
                kind.to_string()
            )))?
            .value()
            .jump(kind, stub)
            .await
    }
}

pub trait HyperwayConfigurator:  Send + Sync {
    fn config(&self, greet: &Greet, hyperway: &mut Hyperway);
}

pub struct DefaultHyperwayConfigurator;

impl HyperwayConfigurator for DefaultHyperwayConfigurator {
    fn config(&self, greet: &Greet, hyperway: &mut Hyperway) {

    }
}

#[derive(Clone)]
pub struct InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator
{
    logger: PointLogger,
    auth: A,
    greeter: G,
    interchange: Arc<HyperwayInterchange>,
    configurator: C
}
impl<A, G, C> InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator
{
    pub fn new(
        auth: A,
        greeter: G,
        configurator: C,
        interchange: Arc<HyperwayInterchange>,
        logger: PointLogger,
    ) -> Self {
        Self {
            auth,
            greeter,
            configurator,
            interchange,
            logger,
        }
    }
}

impl<A, G, C> InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator
{
    async fn enter(&self, greet: Greet) -> Result<HyperwayExt, HyperConnectionErr> {
        let mut hyperway = Hyperway::new(greet.port.clone(), greet.agent.clone());
        self.configurator.config(&greet, & mut hyperway );

        self.interchange.add(hyperway).await;

        let port = greet.port.clone();
        let stub = HyperwayStub {
            agent: greet.agent.clone(),
            remote: greet.port.clone(),
        };

        let mut ext = self.logger.result_ctx(
            "InterchangeGate.enter",
            self.interchange.mount(stub, greet.into()).await,
        )?;

        let (drop_tx, drop_rx) = oneshot::channel();
        ext.drop_tx = Some(drop_tx);

        let interchange = self.interchange.clone();
        tokio::spawn(async move {
            drop_rx.await;
            interchange.remove(port);
        });

        Ok(ext)
    }
}

#[async_trait]
impl<A, G, C> HyperGate for InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator,
{
    async fn knock(&self, knock: Knock) -> Result<HyperwayExt, HyperConnectionErr> {
        let stub = self.auth.auth(knock).await?;
        let greet = self.greeter.greet(stub).await?;
        self.enter(greet).await
    }

    async fn jump(
        &self,
        _kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayExt, HyperConnectionErr> {
        let greet = self.greeter.greet(stub).await?;
        self.enter(greet).await
    }
}

#[derive(Clone)]
pub struct MountInterchangeGate<A, G>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
{
    logger: PointLogger,
    auth: A,
    greeter: G,
    interchange: Arc<HyperwayInterchange>,
}

impl<A, G> MountInterchangeGate<A, G>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
{
    pub fn new(
        auth: A,
        greeter: G,
        interchange: Arc<HyperwayInterchange>,
        logger: PointLogger,
    ) -> Self {
        Self {
            auth,
            greeter,
            interchange,
            logger,
        }
    }

    async fn enter(&self, greet: Greet) -> Result<HyperwayExt, HyperConnectionErr> {
        let stub = HyperwayStub::new(greet.port.clone(), greet.agent.clone());
        let ext = self.interchange.mount(stub, greet.into()).await?;

        Ok(ext)
    }
}

#[async_trait]
impl<A, G> HyperGate for MountInterchangeGate<A, G>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
{
    async fn knock(&self, knock: Knock) -> Result<HyperwayExt, HyperConnectionErr> {
        let stub = self.auth.auth(knock).await?;
        let greet = self.greeter.greet(stub).await?;
        let ext = self.enter(greet).await?;
        Ok(ext)
    }

    async fn jump(
        &self,
        _kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayExt, HyperConnectionErr> {
        let greet = self.greeter.greet(stub).await?;
        let ext = self.enter(greet).await?;
        Ok(ext)
    }
}

pub struct HyperClient {
    pub stub: HyperwayStub,
    tx: mpsc::Sender<UltraWave>,
    status_rx: watch::Receiver<HyperClientStatus>,
    to_client_listener_tx: broadcast::Sender<UltraWave>,
    logger: PointLogger,
    greet_rx: watch::Receiver<Option<Greet>>,
    exchanger: Exchanger
}

impl HyperClient {
    pub fn new(
        stub: HyperwayStub,
        factory: Box<dyn HyperwayExtFactory>,
        logger: PointLogger,
    ) -> Result<HyperClient, MsgErr> {
        let (to_client_listener_tx, _) = broadcast::channel(1024);
        let (to_hyperway_tx, from_client_rx) = mpsc::channel(1024);
        let (status_watch_tx, mut status_rx) = watch::channel(HyperClientStatus::Pending);

        let (status_mpsc_tx, mut status_mpsc_rx): (
            mpsc::Sender<HyperClientStatus>,
            mpsc::Receiver<HyperClientStatus>,
        ) = mpsc::channel(128);

        tokio::spawn(async move {
            while let Some(status) = status_mpsc_rx.recv().await {
                let result = status_watch_tx.send(status.clone());
                if status == HyperClientStatus::Fatal {
                    break;
                }
                if status == HyperClientStatus::Closed {
                    break;
                }
                if let Err(_) = result {
                    break;
                }
            }
        });

        let mut from_runner_rx = HyperClientRunner::new(
            factory,
            from_client_rx,
            status_mpsc_tx.clone(),
            logger.clone(),
        );

        let (greet_tx, greet_rx) = watch::channel(None);

        let exchanger = Exchanger::new( stub.remote.clone(), Timeouts::default() );

        let mut client = Self {
            stub,
            tx: to_hyperway_tx,
            status_rx: status_rx.clone(),
            to_client_listener_tx: to_client_listener_tx.clone(),
            logger: logger.clone(),
            greet_rx,
            exchanger: exchanger.clone()
        };

        {
            let logger = logger.clone();
            tokio::spawn(async move {
                while let Ok(_) = status_rx.changed().await {
                    let status = status_rx.borrow().clone();
                    logger.info(format!("HyperClient status: {}", status.to_string()))
                }
            });
        }

        {
            let logger = logger.clone();
            let status_tx = status_mpsc_tx.clone();
            tokio::spawn(async move {
                async fn relay(
                    mut from_runner_rx: mpsc::Receiver<UltraWave>,
                    to_client_listener_tx: broadcast::Sender<UltraWave>,
                    status_tx: mpsc::Sender<HyperClientStatus>,
                    greet_tx: watch::Sender<Option<Greet>>,
                    exchanger: Exchanger,
                    logger: PointLogger,
                ) -> Result<(), MsgErr> {
                    if let Some(wave) = from_runner_rx.recv().await {
                        match wave.to_reflected() {
                            Ok(reflected) => {
                                if !reflected.core().status.is_success() {
                                    match reflected.core().status.as_u16() {
                                        400 => {
                                            status_tx
                                                .send(HyperClientStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "400: Bad Request: FATAL: something in the knock was incorrect";
                                            return Err(err.into());
                                        }
                                        401 => {
                                            status_tx
                                                .send(HyperClientStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "401: Unauthorized: FATAL: authentication failed (bad credentials?)";
                                            return Err(err.into());
                                        }
                                        403 => {
                                            status_tx
                                                .send(HyperClientStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "403: Forbidden: FATAL: authentication succeeded however the authenticated agent does not have permission to connect to this service";
                                            return Err(err.into());
                                        }
                                        408 => {
                                            status_tx
                                                .send(HyperClientStatus::Panic)
                                                .await
                                                .unwrap_or_default();
                                            let err = "408: Request Timeout: PANIC";
                                            return Err(err.into());
                                        }
                                        301 => {
                                            status_tx
                                                .send(HyperClientStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "301: Moved Permanently: FATAL: please update to new connection address";
                                            return Err(err.into());
                                        }
                                        503 => {
                                            status_tx
                                                .send(HyperClientStatus::Panic)
                                                .await
                                                .unwrap_or_default();
                                            let err =
                                                "503: Service Unavailable: PANIC: try again later";
                                            return Err(err.into());
                                        }
                                        _ => {
                                            status_tx
                                                .send(HyperClientStatus::Panic)
                                                .await
                                                .unwrap_or_default();
                                            let err = format!(
                                                "{}: {}: PANIC: expected 200",
                                                reflected.core().status.as_u16(),
                                                reflected.core().status.to_string()
                                            );
                                            return Err(err.into());
                                        }
                                    }
                                }
                                if let Substance::Greet(greet) = &reflected.core().body {
                                    greet_tx.send(Some(greet.clone()));
                                } else {
                                    status_tx
                                        .send(HyperClientStatus::Fatal)
                                        .await
                                        .unwrap_or_default();
                                    let err = "HyperClient expected first wave Substance to be a reflected Greeting";
                                    return Err(err.into());
                                }
                            }
                            Err(err) => {
                                status_tx
                                    .send(HyperClientStatus::Fatal)
                                    .await
                                    .unwrap_or_default();
                                let err = format!("HyperClient expected first wave Substance to be a reflected Greeting. Instead when attempting to convert to a reflected wave err occured: {}", err.to_string());
                                return Err(err.into());
                            }
                        }
                    }

                    while let Some(wave) = from_runner_rx.recv().await {
                        if wave.is_directed() {
                            to_client_listener_tx.send(wave)?;
                        } else {
                            exchanger.reflected( wave.to_reflected()? ).await?;
                        }
                    }
                    Ok(())
                }

                        relay(
                            from_runner_rx,
                            to_client_listener_tx,
                            status_tx,
                            greet_tx,
                            exchanger,
                            logger.clone(),
                        )
                        .await.unwrap_or_default();
            });
        }

        Ok(client)
    }

    pub async fn proto_transmitter_builder(&self) -> Result<ProtoTransmitterBuilder,MsgErr> {
        self.wait_for_ready(Duration::from_secs(30)).await?;
        let mut builder = ProtoTransmitterBuilder::new( Arc::new(self.router()), self.exchanger.clone() );
        let greet = self.get_greeting().ok_or::<MsgErr>("expected greeting to already be set in HyperClient".into())?;
        builder.agent = SetStrategy::Fill(greet.agent.clone());
        builder.from = SetStrategy::Fill(greet.port.clone());
        Ok(builder)
    }

    pub fn reset(&self) {
        let mut wave = DirectedProto::ping();
        wave.kind(DirectedKind::Signal);
        wave.from(LOCAL_CLIENT.clone().to_port());
        wave.to(LOCAL_CLIENT_RUNNER.clone().to_port());
        wave.method(MsgMethod::new("Reset").unwrap());
        let wave = wave.build().unwrap();
        let wave = wave.to_ultra();
        let tx = self.tx.clone();
        tokio::spawn(async move {
            tx.send(wave).await.unwrap_or_default();
        });
    }

    pub async fn close(&self) {
        let mut wave = DirectedProto::ping();
        wave.kind(DirectedKind::Signal);
        wave.from(LOCAL_CLIENT.clone().to_port());
        wave.to(LOCAL_CLIENT_RUNNER.clone().to_port());
        wave.method(MsgMethod::new("Close").unwrap());
        let wave = wave.build().unwrap();
        let wave = wave.to_ultra();
        let tx = self.tx.clone();
        tx.send(wave).await.unwrap_or_default();
    }

    pub fn router(&self) -> TxRouter {
        TxRouter::new(self.tx.clone())
    }

    pub fn rx(&self) -> broadcast::Receiver<UltraWave> {
        self.to_client_listener_tx.subscribe()
    }

    pub fn get_greeting(&self) -> Option<Greet> {
        self.greet_rx.borrow().clone()
    }

    pub async fn wait_for_greet(&self) -> Result<Greet, MsgErr> {
        let mut greet_rx = self.greet_rx.clone();
        loop {
            let greet = greet_rx.borrow().clone();
            if greet.is_some() {
                return Ok(greet.unwrap());
            } else {
                greet_rx.changed().await?;
            }
        }
    }

    pub async fn wait_for_ready(&self, duration: Duration) -> Result<(), MsgErr> {
        let mut status_rx = self.status_rx.clone();
        let (rtn, mut rtn_rx) = oneshot::channel();

        tokio::spawn(async move {
            loop {
                let status = status_rx.borrow().clone();
                match status {
                    HyperClientStatus::Ready => {
                        rtn.send(Ok(()));
                        break;
                    }
                    HyperClientStatus::Fatal => {
                        rtn.send(Err(MsgErr::from_500(
                            "Fatal status from HyperClient while waiting for Ready",
                        )));
                        break;
                    }
                    _ => {}
                }
            }
        });

        tokio::time::timeout(duration, rtn_rx).await??
    }
}

#[derive(Clone, strum_macros::Display, Eq, PartialEq)]
pub enum HyperClientStatus {
    Unknown,
    Pending,
    Connecting,
    Ready,
    Panic,
    Fatal,
    Closed
}

pub enum HyperClientCall {
    Close,
}

pub enum HyperConnectionErr {
    Fatal(String),
    Retry(String),
}

impl ToString for HyperConnectionErr {
    fn to_string(&self) -> String {
        match self {
            HyperConnectionErr::Fatal(m) => format!("Fatal({})", m),
            HyperConnectionErr::Retry(m) => format!("Retry({})", m),
        }
    }
}

impl From<MsgErr> for HyperConnectionErr {
    fn from(err: MsgErr) -> Self {
        HyperConnectionErr::Retry(err.to_string())
    }
}

pub struct HyperClientRunner {
    ext: Option<HyperwayExt>,
    factory: Box<dyn HyperwayExtFactory>,
    status_tx: mpsc::Sender<HyperClientStatus>,
    to_client_tx: mpsc::Sender<UltraWave>,
    from_client_rx: mpsc::Receiver<UltraWave>,
    logger: PointLogger,
}

impl HyperClientRunner {
    pub fn new(
        factory: Box<dyn HyperwayExtFactory>,
        from_client_rx: mpsc::Receiver<UltraWave>,
        status_tx: mpsc::Sender<HyperClientStatus>,
        logger: PointLogger,
    ) -> mpsc::Receiver<UltraWave> {
        let (to_client_tx, from_runner_rx) = mpsc::channel(1024);
        let logger = logger.push_point("runner").unwrap();
        let runner = Self {
            ext: None,
            factory,
            to_client_tx,
            from_client_rx,
            status_tx,
            logger,
        };

        tokio::spawn(async move {
            runner.start().await;
        });

        from_runner_rx
    }

    async fn start(mut self) {
        self.status_tx
            .send(HyperClientStatus::Pending)
            .await
            .unwrap_or_default();

        loop {
            async fn connect(runner: &mut HyperClientRunner) -> Result<(), HyperConnectionErr> {
                if let Err(_) = runner.status_tx.send(HyperClientStatus::Connecting).await {
                    return Err(HyperConnectionErr::Fatal("can no longer update HyperClient status (probably due to previous Fatal status)".to_string()));
                }
                loop {
                    match runner.logger.result_ctx(
                        "connect",
                        tokio::time::timeout(Duration::from_secs(30), runner.factory.create())
                            .await,
                    ) {
                        Ok(Ok(ext)) => {
                            runner.ext.replace(ext);
                            if let Err(_) = runner.status_tx.send(HyperClientStatus::Ready).await {
                                runner.ext.take();
                                return Err(HyperConnectionErr::Fatal("can no longer update HyperClient status (probably due to previous Fatal status)".to_string()));
                            }
                            return Ok(());
                        }
                        Ok(Err(err)) => {
                            runner.logger.error(format!("{}", err.to_string()));
                        }
                        _ => {}
                    }
                    // wait a little while before attempting to reconnect
                    // maybe add exponential backoff later
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
            }

            async fn relay(runner: &mut HyperClientRunner) -> Result<(), MsgErr> {
                let ext = runner
                    .ext
                    .as_mut()
                    .ok_or::<MsgErr>("must reconnect".into())?;

                loop {
                    tokio::select!(
                        wave = runner.from_client_rx.recv() => {
                                // message comes from client, therefore it should go towards ext (unless it's pointed to the runner)
                                match wave {
                                  Some(wave) => {
                                    if wave.is_directed() && wave.to().is_single() && wave.to().unwrap_single().point == *LOCAL_CLIENT_RUNNER
                                    {
                                        let method: MsgMethod = wave.to_directed().unwrap().core().method.clone().try_into().unwrap();
                                        if method.to_string() == "Reset".to_string() {
                                           return Err(MsgErr::from_500("reset"));
                                        } else if method.to_string() == "Close".to_string(){
                                            runner.status_tx.send(HyperClientStatus::Closed).await;
                                            return Ok(());
                                        }
                                    } else {
                                        match ext.tx.send(wave).await {
                                            Ok(_) => {}
                                            Err(err) => {
                                                // wave gets lost... need to requeue it somehow...
                                                //                                    runner.to_client_tx.try_send(err.0);
                                                return Err(MsgErr::from_500("ext failure"));
                                            }
                                        }
                                    }
                                      }
                                      None => {
                                        break;
                                      }
                                    }
                        }
                        wave = ext.rx.recv() => {
                            match wave {
                                Some( wave ) => {
                                   runner.to_client_tx.send(wave).await;
                                }
                                None => {
                                    runner.logger.warn("NONE on Ext");
                                    break;
                                }
                            }
                        }
                    );
                }

                Ok(())
            }

            loop {
                match connect(&mut self).await {
                    Ok(_) => {}
                    Err(HyperConnectionErr::Fatal(message)) => {
                        // need to log the fatal error message somehow
                        self.status_tx
                            .send(HyperClientStatus::Fatal)
                            .await
                            .unwrap_or_default();
                        return;
                    }
                    Err(HyperConnectionErr::Retry(m)) => {
                        self.status_tx
                            .send(HyperClientStatus::Panic)
                            .await
                            .unwrap_or_default();
                    }
                }

                match relay(&mut self).await {
                    Ok(_) => {
                        // natural end... this runner is ready to be dropped
                        break;
                    }
                    Err(err) => {
                        self.logger.error(format!("{}", err.to_string()));
                        // some error occurred when relaying therefore we need to reconnect
                        self.ext = None;
                    }
                }
            }
        }
    }
}

#[async_trait]
pub trait HyperwayExtFactory: Send + Sync {
    async fn create(&self) -> Result<HyperwayExt, HyperConnectionErr>;
}

pub struct LocalHyperwayGateUnlocker {
    pub knock: Knock,
    pub gate: Arc<dyn HyperGate>,
}

impl LocalHyperwayGateUnlocker {
    pub fn new(remote: Port, gate: Arc<dyn HyperGate>) -> Self {
        let knock = Knock::new(InterchangeKind::Singleton, remote, Substance::Empty);
        Self { knock, gate }
    }
}

#[async_trait]
impl HyperwayExtFactory for LocalHyperwayGateUnlocker {
    async fn create(&self) -> Result<HyperwayExt, HyperConnectionErr> {
        self.gate.knock(self.knock.clone()).await
    }
}

pub struct LocalHyperwayGateJumper {
    pub kind: InterchangeKind,
    pub stub: HyperwayStub,
    pub gate: Arc<dyn HyperGate>,
}

impl LocalHyperwayGateJumper {
    pub fn new(kind: InterchangeKind, stub: HyperwayStub, gate: Arc<dyn HyperGate>) -> Self {
        Self { kind, stub, gate }
    }
}

#[async_trait]
impl HyperwayExtFactory for LocalHyperwayGateJumper {
    async fn create(&self) -> Result<HyperwayExt, HyperConnectionErr> {
        self.gate.jump(self.kind.clone(), self.stub.clone()).await
    }
}

/*
pub struct DirectInterchangeMountHyperwayExtFactory {
    pub stub: HyperwayStub,
    pub interchange: Arc<HyperwayInterchange>,
}

impl DirectInterchangeMountHyperwayExtFactory {
    pub fn new(stub: HyperwayStub, interchange: Arc<HyperwayInterchange>) -> Self {
        Self { stub, interchange }
    }
}

#[async_trait]
impl HyperwayExtFactory for DirectInterchangeMountHyperwayExtFactory {
    async fn create(&self) -> Result<HyperwayExt, HyperConnectionErr> {
        match self.interchange.mount(self.stub.clone()).await {
            Ok(mount) => {
                let knock = Knock::new(
                    InterchangeKind::Singleton,
                    self.stub.remote.clone(),
                    Substance::Empty,
                );
                let wave: Wave<Ping> = knock.into();
                let wave = wave.to_ultra();
                mount.tx.send(wave).await;
                Ok(mount)
            }
            Err(_) => Err(HyperConnectionErr::Fatal(format!(
                "invalid mount point '{}'",
                self.stub.remote.to_string()
            ))),
        }
    }
}

 */

#[cfg(test)]
mod tests {
    use crate::{
        AnonHyperAuthenticator, HyperGate, HyperGateSelector, HyperRouter, HyperwayInterchange,
        InterchangeGate,
    };
    use chrono::{DateTime, Utc};
    use cosmic_api::command::request::create::PointFactoryU64;
    use cosmic_api::id::id::{Point, Uuid};
    use cosmic_api::log::RootLogger;
    use cosmic_api::substance::substance::Substance;
    use cosmic_api::sys::{InterchangeKind, Knock};
    use cosmic_api::wave::HyperWave;
    use dashmap::DashMap;
    use std::collections::HashMap;
    use std::str::FromStr;
    use std::sync::Arc;

    #[no_mangle]
    pub(crate) extern "C" fn cosmic_uuid() -> String {
        uuid::Uuid::new_v4().to_string()
    }

    #[no_mangle]
    pub(crate) extern "C" fn cosmic_timestamp() -> DateTime<Utc> {
        Utc::now()
    }

    pub struct DummyRouter {}

    #[async_trait]
    impl HyperRouter for DummyRouter {
        async fn route(&self, wave: HyperWave) {
            println!("received hyperwave!");
        }
    }

    /*
    #[tokio::test]
    async fn hyper_test() {
        let point = Point::from_str("test").unwrap();
        let logger = RootLogger::default().point(point.clone());
        let interchange = Arc::new(HyperwayInterchange::new(
            logger.push("interchange").unwrap(),
        ));

        let point_factory =
            PointFactoryU64::new(point.push("portals").unwrap(), "portal-".to_string());
        let auth = AnonHyperAuthenticator::new(
            Arc::new(point_factory),
            logger.logger.clone(),
        );

        let gate = InterchangeGate::new(auth, interchange, logger.push("gate").unwrap());

        let mut map = Arc::new(DashMap::new());
        map.insert(InterchangeKind::Cli, Box::new(gate));

        let entry_router = HyperGateSelector::new(map);

        let knock = Knock {
            kind: InterchangeKind::Cli,
            auth: Box::new(Substance::Empty),
            remote: Some(point.push("cli").unwrap()),
        };

        entry_router.knock(knock).await.unwrap();
    }

     */
}

#[cfg(test)]
pub mod test {
    use crate::{
        AnonHyperAuthenticator, AnonHyperAuthenticatorAssignEndPoint, HyperClient,
        HyperConnectionErr, HyperGate, HyperGateSelector, HyperGreeter, HyperRouter, Hyperlane,
        Hyperway, HyperwayExt, HyperwayExtFactory, HyperwayInterchange, HyperwayStub,
        InterchangeGate, LocalHyperwayGateJumper, LocalHyperwayGateUnlocker, MountInterchangeGate,
        TokenAuthenticatorWithRemoteWhitelist,
    };
    use cosmic_api::command::request::create::PointFactoryU64;
    use cosmic_api::error::MsgErr;
    use cosmic_api::id::id::{Layer, Point, ToPoint, ToPort};
    use cosmic_api::log::RootLogger;
    use cosmic_api::msg::MsgMethod;
    use cosmic_api::substance::substance::{Substance, Token};
    use cosmic_api::sys::{Greet, InterchangeKind, Knock};
    use cosmic_api::wave::{
        Agent, DirectedKind, DirectedProto, Exchanger, HyperWave, Pong, ProtoTransmitter,
        ReflectedKind, ReflectedProto, ReflectedWave, Router, TxRouter, UltraWave, Wave,
    };
    use dashmap::DashMap;
    use lazy_static::lazy_static;
    use std::collections::{HashMap, HashSet};
    use std::str::FromStr;
    use std::sync::Arc;
    use std::time::Duration;
    use tokio::sync::{broadcast, mpsc, oneshot};

    lazy_static! {
        pub static ref LESS: Point = Point::from_str("space:users:less").expect("point");
        pub static ref FAE: Point = Point::from_str("space:users:fae").expect("point");
    }

    pub struct TestRouter {}

    #[async_trait]
    impl HyperRouter for TestRouter {
        async fn route(&self, wave: HyperWave) {
            println!("Test Router routing!");
            //    todo!()
        }
    }

    #[derive(Clone)]
    pub struct TestGreeter;

    impl TestGreeter {
        pub fn new() -> Self {
            Self
        }
    }

    #[async_trait]
    impl HyperGreeter for TestGreeter {
        async fn greet(&self, stub: HyperwayStub) -> Result<Greet, MsgErr> {
            Ok(Greet {
                port: stub.remote.clone(),
                agent: stub.agent.clone(),
                hop: Point::remote_endpoint().to_port().with_layer(Layer::Core),
                transport: stub.remote.clone(),
            })
        }
    }

    fn hello_wave() -> UltraWave {
        let mut hello = DirectedProto::ping();
        hello.kind(DirectedKind::Ping);
        hello.to(FAE.clone().to_port());
        hello.from(LESS.clone().to_port());
        hello.method(MsgMethod::new("Hello").unwrap());
        hello.body(Substance::Empty);
        let directed = hello.build().unwrap();
        let wave = directed.to_ultra();
        wave
    }

    #[tokio::test]
    pub async fn test_hyperlane() {
        let hyperlane = Hyperlane::new("test");
        let mut rx = hyperlane.rx(None).await;
        let wave = hello_wave();
        let wave_id = wave.id().clone();
        hyperlane.send(wave).await.unwrap();
        let wave = tokio::time::timeout(Duration::from_secs(5u64), rx.recv())
            .await
            .unwrap()
            .unwrap();
        assert_eq!(wave.id(), wave_id);
    }

    #[tokio::test]
    pub async fn test_hyperway() {
        let hyperway = Hyperway::new(LESS.clone().to_port(), LESS.to_agent());
        let wave = hello_wave();
        let wave_id = wave.id().clone();
        hyperway.outbound.send(wave).await;
        let wave = tokio::time::timeout(
            Duration::from_secs(5u64),
            hyperway.outbound.rx(None).await.recv(),
        )
        .await
        .unwrap()
        .unwrap();

        let wave = hello_wave();
        let wave_id = wave.id().clone();
        hyperway.inbound.send(wave).await;
        let wave = tokio::time::timeout(
            Duration::from_secs(5u64),
            hyperway.inbound.rx(None).await.recv(),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(wave.id(), wave_id);
    }

    /*
    #[tokio::test]
    pub async fn test_hyperway_ext() {
        let hyperway = Hyperway::new(LESS.clone().to_port(), LESS.to_agent());

        let mut ext = hyperway.mount().await;
        let wave = hello_wave();
        let wave_id = wave.id().clone();
        ext.tx.send(wave).await;
        let wave = tokio::time::timeout(
            Duration::from_secs(5u64),
            hyperway.inbound.rx().await.recv(),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(wave.id(), wave_id);

        let wave = hello_wave();
        let wave_id = wave.id().clone();
        hyperway.outbound.send(wave).await;
        let wave = tokio::time::timeout(Duration::from_secs(5u64), ext.rx.recv())
            .await
            .unwrap()
            .unwrap();
        assert_eq!(wave.id(), wave_id);
    }

     */

    #[tokio::test]
    pub async fn test_hyperclient() {
        pub struct TestFactory {
            pub hyperway: Hyperway,
        }

        impl TestFactory {
            pub fn new() -> Self {
                let hyperway = Hyperway::new(LESS.clone().to_port(), LESS.to_agent());
                Self { hyperway }
            }

            pub fn inbound_tx(&self) -> mpsc::Sender<UltraWave> {
                self.hyperway.inbound.tx()
            }

            pub async fn inbound_rx(&self) -> mpsc::Receiver<UltraWave> {
                self.hyperway.inbound.rx(None).await
            }

            pub async fn outbound_rx(&self) -> broadcast::Receiver<UltraWave> {
                self.hyperway.outbound.eavesdrop()
            }

            pub fn outbound_tx(&self) -> mpsc::Sender<UltraWave> {
                self.hyperway.outbound.tx()
            }
        }

        #[async_trait]
        impl HyperwayExtFactory for TestFactory {
            async fn create(&self) -> Result<HyperwayExt, HyperConnectionErr> {
                Ok(self.hyperway.mount(None).await)
            }
        }

        {
            let factory = TestFactory::new();
            let mut inbound_rx = factory.inbound_rx().await;
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("client").unwrap());
            let client = HyperClient::new(
                HyperwayStub::new(LESS.clone().to_port(), LESS.to_agent()),
                Box::new(factory),
                logger,
            )
            .unwrap();

            let client_listener_rx = client.rx();

            client.reset();

            let router = client.router();
            let wave = hello_wave();
            let wave_id = wave.id().clone();
            router.route(wave).await;
            let wave = tokio::time::timeout(Duration::from_secs(5u64), inbound_rx.recv())
                .await
                .unwrap()
                .unwrap();
            assert_eq!(wave.id(), wave_id);
        }

        {
            let factory = TestFactory::new();
            let outbound_tx = factory.outbound_tx();
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("client").unwrap());
            let client = HyperClient::new(
                HyperwayStub::new(LESS.clone().to_port(), LESS.to_agent()),
                Box::new(factory),
                logger,
            )
            .unwrap();

            let mut client_listener_rx = client.rx();

            let wave = hello_wave();
            let wave_id = wave.id().clone();
            outbound_tx.send(wave).await.unwrap();
            let wave = tokio::time::timeout(Duration::from_secs(5u64), client_listener_rx.recv())
                .await
                .unwrap()
                .unwrap();
            assert_eq!(wave.id(), wave_id);
        }
    }

    #[tokio::test]
    pub async fn test() {
        let root_logger = RootLogger::default();
        let logger = root_logger.point(Point::from_str("point").unwrap());
        let interchange = Arc::new(HyperwayInterchange::new(
            logger.push_point("interchange").unwrap(),
        ));

        interchange.add(Hyperway::new(LESS.clone().to_port(), LESS.to_agent())).await;
        interchange.add(Hyperway::new(FAE.clone().to_port(), FAE.to_agent())).await;

        let auth = AnonHyperAuthenticator::new();
        let gate = Arc::new(MountInterchangeGate::new(
            auth,
            TestGreeter::new(),
            interchange.clone(),
            logger.push_point("gate").unwrap(),
        ));
        let mut gates: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>> = Arc::new(DashMap::new());
        gates.insert(InterchangeKind::Singleton, gate);
        let gate = Arc::new(HyperGateSelector::new(gates));

        let less_stub = HyperwayStub::from_port(LESS.clone().to_port());
        let fae_stub = HyperwayStub::from_port(FAE.clone().to_port());

        let less_factory = LocalHyperwayGateUnlocker::new(LESS.clone().to_port(), gate.clone());

        let fae_factory = LocalHyperwayGateUnlocker::new(FAE.clone().to_port(), gate.clone());

        let root_logger = RootLogger::default();
        let logger = root_logger.point(Point::from_str("less-client").unwrap());
        let less_client =
            HyperClient::new(less_stub.clone(), Box::new(less_factory), logger).unwrap();
        let logger = root_logger.point(Point::from_str("fae-client").unwrap());
        let fae_client = HyperClient::new(fae_stub.clone(), Box::new(fae_factory), logger).unwrap();

        let mut less_rx = less_client.rx();
        let mut fae_rx = fae_client.rx();

        let less_router = less_client.router();
        let less_exchanger = Exchanger::new(LESS.clone().to_port(), Default::default());
        let less_transmitter = ProtoTransmitter::new(Arc::new(less_router), less_exchanger.clone());

        let fae_router = fae_client.router();
        let fae_exchanger = Exchanger::new(FAE.clone().to_port(), Default::default());
        let fae_transmitter = ProtoTransmitter::new(Arc::new(fae_router), fae_exchanger.clone());

        {
            let fae = FAE.clone();
            tokio::spawn(async move {
                let wave = fae_rx.recv().await.unwrap();
                let mut reflected = ReflectedProto::new();
                reflected.kind(ReflectedKind::Pong);
                reflected.status(200u16);
                reflected.to(wave.from().clone());
                reflected.from(fae.to_port());
                reflected.intended(wave.to());
                reflected.reflection_of(wave.id());
                let wave = reflected.build().unwrap();
                let wave = wave.to_ultra();
                fae_transmitter.route(wave).await;
            });
        }

        {
            let less_exchanger = less_exchanger.clone();
            tokio::spawn(async move {
                let wave = less_rx.recv().await.unwrap();
                if !wave.is_directed() {
                    less_exchanger.reflected(wave.to_reflected().unwrap()).await;
                }
            });
        }

        let (rtn, mut rtn_rx) = oneshot::channel();
        tokio::spawn(async move {
            let mut hello = DirectedProto::ping();
            hello.kind(DirectedKind::Ping);
            hello.to(FAE.clone().to_port());
            hello.from(LESS.clone().to_port());
            hello.method(MsgMethod::new("Hello").unwrap());
            hello.body(Substance::Empty);
            let pong: Wave<Pong> = less_transmitter.direct(hello).await.unwrap();
            rtn.send(pong.core.status.as_u16() == 200u16);
        });

        let result = tokio::time::timeout(Duration::from_secs(5), rtn_rx)
            .await
            .unwrap()
            .unwrap();
        assert!(result);
    }
}
