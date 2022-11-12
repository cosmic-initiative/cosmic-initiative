#![allow(warnings)]

#[macro_use]
extern crate async_trait;
#[macro_use]
extern crate lazy_static;

use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::sync::atomic::{AtomicU16, Ordering};
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use futures::future::select_all;
use futures::FutureExt;
use tokio::io::AsyncWriteExt;
use tokio::select;
use tokio::sync::mpsc::error::{SendError, SendTimeoutError, TrySendError};
use tokio::sync::mpsc::Receiver;
use tokio::sync::oneshot::Sender;
use tokio::sync::{broadcast, mpsc, oneshot, watch, Mutex, RwLock};

use cosmic_space::command::direct::create::{PointFactoryU64, PointSegTemplate};
use cosmic_space::err::SpaceErr;
use cosmic_space::frame::PrimitiveFrame;
use cosmic_space::hyper::{Greet, HyperSubstance, InterchangeKind, Knock};
use cosmic_space::point::{Layer, Point, PointFactory, Surface, ToPoint, ToSurface, Version};
use cosmic_space::log::{PointLogger, RootLogger, Tracker};
use cosmic_space::particle::Status;
use cosmic_space::settings::Timeouts;
use cosmic_space::substance::{FormErrs, Substance, SubstanceKind, Token};
use cosmic_space::util::uuid;
use cosmic_space::wave::core::ext::ExtMethod;
use cosmic_space::wave::core::hyp::HypMethod;
use cosmic_space::wave::core::Method;
use cosmic_space::wave::exchange::asynch::{
    Exchanger, ProtoTransmitter, ProtoTransmitterBuilder, Router, TxRouter,
};
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{
    Agent, DirectedKind, DirectedProto, Handling, HyperWave, Ping, Pong, Reflectable,
    ReflectedKind, ReflectedProto, ReflectedWave, UltraWave, Wave, WaveId, WaveKind,
};
use cosmic_space::VERSION;

lazy_static! {
    pub static ref LOCAL_CLIENT: Point = Point::from_str("LOCAL::client").expect("point");
    pub static ref LOCAL_CLIENT_RUNNER: Point =
        Point::from_str("LOCAL::client:runner").expect("point");
    pub static ref HYPERLANE_INDEX: AtomicU16 = AtomicU16::new(0);
}

pub enum HyperwayKind {
    Mount,
    Ephemeral,
}

pub struct Hyperway {
    pub remote: Surface,
    outbound: Hyperlane,
    inbound: Hyperlane,
    logger: PointLogger,

    #[cfg(test)]
    pub diagnostic: HyperwayDiagnostic,
}

impl Hyperway {
    pub fn new(remote: Surface, agent: Agent, logger: PointLogger) -> Self {
        let logger = logger.point(remote.point.clone());
        let mut inbound = Hyperlane::new(format!("{}<Inbound>", remote.to_string()));
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
            outbound: Hyperlane::new(format!("{}<Outbound>", remote.to_string())),
            remote,
            inbound,
            logger,
            #[cfg(test)]
            diagnostic: HyperwayDiagnostic::new(),
        }
    }

    pub fn transform_inbound(&self, transform: Box<dyn HyperTransform>) {
        self.inbound
            .tx
            .try_send(HyperlaneCall::Transform(transform));
    }

    pub fn transform_to(&self, to: Surface) {
        self.inbound
            .tx
            .try_send(HyperlaneCall::Transform(Box::new(ToTransform::new(to))));
    }

    pub async fn hyperway_endpoint_near(&self, init_wave: Option<UltraWave>) -> HyperwayEndpoint {
        let drop_tx = None;

        HyperwayEndpoint {
            tx: self.outbound.tx(),
            rx: self.inbound.rx(init_wave).await,
            drop_tx,
            logger: self.logger.clone(),
        }
    }

    pub async fn hyperway_endpoint_near_with_drop_event(
        &self,
        drop_tx: oneshot::Sender<()>,
        init_wave: Option<UltraWave>,
    ) -> HyperwayEndpoint {
        let drop_tx = Some(drop_tx);

        HyperwayEndpoint {
            tx: self.outbound.tx(),
            rx: self.inbound.rx(init_wave).await,
            drop_tx,
            logger: self.logger.clone(),
        }
    }

    pub async fn hyperway_endpoint_far(&self, init_wave: Option<UltraWave>) -> HyperwayEndpoint {
        HyperwayEndpoint {
            tx: self.inbound.tx(),
            rx: self.outbound.rx(init_wave).await,
            drop_tx: None,
            logger: self.logger.clone(),
        }
    }

    pub async fn hyperway_endpoint_far_drop_event(
        &self,
        init_wave: Option<UltraWave>,
        drop_tx: oneshot::Sender<()>,
    ) -> HyperwayEndpoint {
        HyperwayEndpoint {
            tx: self.inbound.tx(),
            rx: self.outbound.rx(init_wave).await,
            drop_tx: Some(drop_tx),
            logger: self.logger.clone(),
        }
    }
}

#[cfg(test)]
pub struct HyperwayDiagnostic {
    pub replaced_ext: broadcast::Sender<Result<(), SpaceErr>>,
}

#[cfg(test)]
impl HyperwayDiagnostic {
    pub fn new() -> Self {
        let (replaced_ext, _) = broadcast::channel(128);
        Self { replaced_ext }
    }
}

pub struct HyperwayEndpoint {
    drop_tx: Option<oneshot::Sender<()>>,
    pub tx: mpsc::Sender<UltraWave>,
    pub rx: mpsc::Receiver<UltraWave>,
    pub logger: PointLogger,
}

impl HyperwayEndpoint {
    pub fn new(
        tx: mpsc::Sender<UltraWave>,
        rx: mpsc::Receiver<UltraWave>,
        logger: PointLogger,
    ) -> Self {
        let drop_tx = None;
        Self {
            tx,
            rx,
            drop_tx,
            logger,
        }
    }

    pub fn new_with_drop(
        tx: mpsc::Sender<UltraWave>,
        rx: mpsc::Receiver<UltraWave>,
        drop_tx: oneshot::Sender<()>,
        logger: PointLogger,
    ) -> Self {
        let drop_tx = Some(drop_tx);
        Self {
            tx,
            rx,
            drop_tx,
            logger,
        }
    }

    pub fn connect(mut self, mut endpoint: HyperwayEndpoint) {
        tokio::spawn(async move {
            let logger = endpoint.logger.clone();
            let end_tx = endpoint.tx.clone();
            {
                let my_tx = self.tx.clone();
                tokio::spawn(async move {
                    let logger = endpoint.logger.push_mark("mux:tx").unwrap();
                    while let Some(wave) = endpoint.rx.recv().await {
                        logger.track(&wave, || Tracker::new("hyperway-endpoint", "Rx"));
                        match logger.result(my_tx.send(wave).await) {
                            Ok(_) => {}
                            Err(_) => break,
                        }
                    }
                });
            }

            let logger = logger.push_mark("mux:rx").unwrap();
            while let Some(wave) = self.rx.recv().await {
                logger.track(&wave, || Tracker::new("hyperway-endpoint", "Tx"));
                match logger.result(end_tx.send(wave).await) {
                    Ok(_) => {}
                    Err(_) => break,
                }
            }
        });
    }

    pub fn add_drop_tx(&mut self, drop_tx: oneshot::Sender<()>) {
        self.drop_tx.replace(drop_tx);
    }

    pub fn router(&self) -> TxRouter {
        TxRouter::new(self.tx.clone())
    }
}

impl Drop for HyperwayEndpoint {
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
    pub remote: Surface,
}

impl From<Greet> for HyperwayStub {
    fn from(greet: Greet) -> Self {
        Self {
            agent: greet.agent,
            remote: greet.surface,
        }
    }
}

impl HyperwayStub {
    pub fn from_port(remote: Surface) -> Self {
        Self {
            agent: remote.to_agent(),
            remote,
        }
    }

    pub fn new(remote: Surface, agent: Agent) -> Self {
        Self { agent, remote }
    }
}

pub enum HyperwayInterchangeCall {
    Wave(UltraWave),
    Internal(Hyperway),
    Remove(Surface),
    Mount {
        stub: HyperwayStub,
        init_wave: Option<UltraWave>,
        rtn: oneshot::Sender<Result<HyperwayEndpoint, SpaceErr>>,
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
    transport_to: Surface,
}

impl TransportTransform {
    pub fn new(transport_to: Surface) -> Self {
        Self { transport_to }
    }
}

impl HyperTransform for TransportTransform {
    fn filter(&self, wave: UltraWave) -> UltraWave {
        let from = wave.from().clone();
        let transport = wave.wrap_in_transport(from, self.transport_to.clone());
        let wave = transport.build().unwrap();
        wave.to_ultra()
    }
}

#[derive(Clone)]
pub struct HopTransform {
    hop_to: Surface,
}

impl HopTransform {
    pub fn new(hop_to: Surface) -> Self {
        Self { hop_to }
    }
}

impl HyperTransform for HopTransform {
    fn filter(&self, wave: UltraWave) -> UltraWave {
        let signal = wave.to_signal().unwrap();
        let from = signal.from.clone();
        let wave = signal.wrap_in_hop(from, self.hop_to.clone());
        let wave = wave.build().unwrap();
        wave.to_ultra()
    }
}

pub struct ToTransform {
    to: Surface,
}

impl ToTransform {
    pub fn new(to: Surface) -> Self {
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
    from: Surface,
}

impl FromTransform {
    pub fn new(from: Surface) -> Self {
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
    label: String,
}

impl Hyperlane {
    pub fn new<S: ToString>(label: S) -> Self {
        #[cfg(test)]
        let (eavesdrop_tx, _) = broadcast::channel(16);

        let label = format!(
            "{}::{}",
            label.to_string(),
            HYPERLANE_INDEX.fetch_add(1, Ordering::Relaxed)
        );

        let (tx, mut rx) = mpsc::channel(1024);
        {
            let label = label.clone();
            let tx = tx.clone();
            #[cfg(test)]
            let eavesdrop_tx = eavesdrop_tx.clone();

            tokio::spawn(async move {
                let mut ext = None;
                let mut queue = vec![];
                let mut transforms = vec![];
                while let Some(call) = rx.recv().await {
                    match call {
                        HyperlaneCall::Ext(ext_tx) => {
                            ext.replace(ext_tx);
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
                    if !queue.is_empty() {
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

    pub async fn send(&self, wave: UltraWave) -> Result<(), SpaceErr> {
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
    singular_to: Option<Surface>,
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
                            let logger = logger.clone();
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
                                None => {
                                    logger.warn(
                                            format!("wave is addressed to hyperway that this interchagne does not have from: {} to: {} ",
                                                    wave.from().to_string(),
                                                    wave.to().to_string()
                                            )
                                        );
                                }
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
                        } => match hyperways.get(&stub.remote) {
                            None => {
                                logger.error(format!(
                                    "mount hyperway {} not found",
                                    stub.remote.to_string()
                                ));
                                rtn.send(Err(format!(
                                    "hyperway {} not found",
                                    stub.remote.to_string()
                                )
                                .into()));
                            }
                            Some(hyperway) => {
                                let endpoint = hyperway.hyperway_endpoint_far(init_wave).await;
                                rtn.send(Ok(endpoint));
                            }
                        },
                    }
                }
            });
        }

        Self {
            call_tx,
            logger,
            singular_to: None,
        }
    }

    pub fn router(&self) -> Box<dyn Router> {
        Box::new(OutboundRouter::new(
            self.call_tx.clone(),
            self.logger.clone(),
        ))
    }

    pub fn point(&self) -> &Point {
        &self.logger.point
    }

    pub async fn mount(
        &self,
        stub: HyperwayStub,
        init_wave: Option<UltraWave>,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
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

    pub fn singular_to(&mut self, to: Surface) {
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

    pub fn remove(&self, hyperway: Surface) {
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
    pub logger: PointLogger,
    pub call_tx: mpsc::Sender<HyperwayInterchangeCall>,
}

impl OutboundRouter {
    pub fn new(call_tx: mpsc::Sender<HyperwayInterchangeCall>, logger: PointLogger) -> Self {
        Self { call_tx, logger }
    }
}

#[async_trait]
impl Router for OutboundRouter {
    async fn route(&self, wave: UltraWave) {
        self.logger
            .track(&wave, || Tracker::new(format!("outbound:router"), "Route"));

        self.call_tx.send(HyperwayInterchangeCall::Wave(wave)).await;
    }
}

#[async_trait]
pub trait HyperGreeter: Send + Sync + Clone + Sized {
    async fn greet(&self, stub: HyperwayStub) -> Result<Greet, SpaceErr>;
}

#[derive(Clone)]
pub struct SimpleGreeter {
    hop: Surface,
    transport: Surface,
}

impl SimpleGreeter {
    pub fn new(hop: Surface, transport: Surface) -> Self {
        Self { hop, transport }
    }
}

#[async_trait]
impl HyperGreeter for SimpleGreeter {
    async fn greet(&self, stub: HyperwayStub) -> Result<Greet, SpaceErr> {
        Ok(Greet {
            surface: stub.remote,
            agent: stub.agent,
            hop: self.hop.clone(),
            transport: self.transport.clone(),
        })
    }
}

#[async_trait]
pub trait HyperAuthenticator: Send + Sync + Clone + Sized {
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, SpaceErr>;
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
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, SpaceErr> {
        if let Substance::Token(token) = &*knock.auth {
            if *token == self.token {
                Ok(HyperwayStub {
                    agent: self.agent.clone(),
                    remote: knock
                        .remote
                        .ok_or::<SpaceErr>("expected a remote entry selection".into())?,
                })
            } else {
                Err(SpaceErr::new(500, "invalid token"))
            }
        } else {
            Err(SpaceErr::new(500, "expected Subtance: Token"))
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
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, SpaceErr> {
        if let Substance::Token(token) = &*knock.auth {
            if *token == self.token {
                let remote = knock
                    .remote
                    .ok_or(SpaceErr::new(500, "expected a remote entry selection"))?;
                if self.whitelist.contains(&remote) {
                    Ok(HyperwayStub {
                        agent: self.agent.clone(),
                        remote,
                    })
                } else {
                    Err(SpaceErr::new(500, "remote is not part of the whitelist"))
                }
            } else {
                Err(SpaceErr::new(500, "invalid token"))
            }
        } else {
            Err(SpaceErr::new(500, "expecting Substance: Token"))
        }
    }
}

#[async_trait]
impl HyperAuthenticator for AnonHyperAuthenticator {
    async fn auth(&self, req: Knock) -> Result<HyperwayStub, SpaceErr> {
        let remote = req
            .remote
            .ok_or(SpaceErr::new(500, "required remote point request"))?;

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
    async fn auth(&self, knock: Knock) -> Result<HyperwayStub, SpaceErr> {
        let remote = self
            .logger
            .result(self.remote_point_factory.create().await)?
            .to_surface();
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
    async fn auth(&self, auth_req: Knock) -> Result<HyperwayStub, SpaceErr> {
        match &*auth_req.auth {
            Substance::Token(token) => {
                if let Some((_, stub)) = self.tokens.remove(token) {
                    return Ok(stub);
                } else {
                    return Err(SpaceErr::new(500, "invalid token"));
                }
            }
            _ => {
                return Err(SpaceErr::new(500, "expected Substance: Token"));
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

    pub async fn dispense(&self) -> Result<(Token, HyperwayStub), SpaceErr> {
        let token = Token::new_uuid();
        let remote_point = self.remote_point_factory.create().await?.to_surface();
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
    selector: HyperGateSelector,
}

impl VersionGate {
    pub fn new(selector: HyperGateSelector) -> Self {
        Self { selector }
    }
    pub async fn unlock(&self, version: semver::Version) -> Result<HyperGateSelector, String> {
        if version == *VERSION {
            Ok(self.selector.clone())
        } else {
            Err("version mismatch".to_string())
        }
    }
}

#[async_trait]
pub trait HyperGate: Send + Sync {
    async fn knock(&self, knock: Knock) -> Result<HyperwayEndpoint, SpaceErr>;

    async fn jump(
        &self,
        kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayEndpoint, SpaceErr>;
}

pub struct HopRouter {
    greet: Greet,
    tx: mpsc::Sender<UltraWave>,
}

impl HopRouter {
    fn to_hop(&self, mut wave: UltraWave) -> Result<UltraWave, SpaceErr> {
        wave.set_agent(self.greet.agent.clone());
        let mut transport = wave
            .wrap_in_transport(self.greet.surface.clone(), self.greet.transport.clone())
            .build()?
            .to_signal()?;
        let hop = transport
            .wrap_in_hop(Point::local_portal().to_surface(), self.greet.hop.clone())
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
}

pub struct HyperApi {
    greet: Greet,
    hyperway: HyperwayEndpoint,
    exchanger: Exchanger,
}

impl HyperApi {
    pub fn new(hyperway: HyperwayEndpoint, greet: Greet, logger: PointLogger) -> Self {
        let exchanger = Exchanger::new(greet.surface.clone(), Default::default(), logger);
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

impl Default for HyperGateSelector {
    fn default() -> Self {
        Self::new(Arc::new(DashMap::new()))
    }
}

impl HyperGateSelector {
    pub fn new(map: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>>) -> Self {
        Self { map }
    }

    pub fn add(&self, kind: InterchangeKind, gate: Arc<dyn HyperGate>) -> Result<(), SpaceErr> {
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
    async fn knock(&self, knock: Knock) -> Result<HyperwayEndpoint, SpaceErr> {
        if let Some(gate) = self.map.get(&knock.kind) {
            gate.value().knock(knock).await
        } else {
            Err(SpaceErr::new(
                500,
                format!("interchange not available: {}", knock.kind.to_string()).as_str(),
            ))
        }
    }

    async fn jump(
        &self,
        kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
        self.map
            .get(&kind)
            .ok_or(SpaceErr::new(
                500,
                format!("interchange kind not available: {}", kind.to_string()).as_str(),
            ))?
            .value()
            .jump(kind, stub)
            .await
    }
}

pub trait HyperwayConfigurator: Send + Sync {
    fn config(&self, greet: &Greet, hyperway: &mut Hyperway);
}

pub struct DefaultHyperwayConfigurator;

impl HyperwayConfigurator for DefaultHyperwayConfigurator {
    fn config(&self, greet: &Greet, hyperway: &mut Hyperway) {}
}

#[derive(Clone)]
pub struct InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator,
{
    logger: PointLogger,
    auth: A,
    greeter: G,
    interchange: Arc<HyperwayInterchange>,
    configurator: C,
}
impl<A, G, C> InterchangeGate<A, G, C>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
    C: HyperwayConfigurator,
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
    C: HyperwayConfigurator,
{
    async fn enter(&self, greet: Greet) -> Result<HyperwayEndpoint, SpaceErr> {
        let mut hyperway = Hyperway::new(
            greet.surface.clone(),
            greet.agent.clone(),
            self.logger.clone(),
        );
        self.configurator.config(&greet, &mut hyperway);

        self.interchange.add(hyperway).await;

        let port = greet.surface.clone();
        let stub = HyperwayStub {
            agent: greet.agent.clone(),
            remote: greet.surface.clone(),
        };

        let mut ext = self.logger.result_ctx(
            "InterchangeGate.enter",
            self.interchange.mount(stub, Some(greet.into())).await,
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
    async fn knock(&self, knock: Knock) -> Result<HyperwayEndpoint, SpaceErr> {
        let stub = self.auth.auth(knock).await?;
        let greet = self.greeter.greet(stub).await?;
        self.enter(greet).await
    }

    async fn jump(
        &self,
        _kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
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

    async fn enter(&self, greet: Greet) -> Result<HyperwayEndpoint, SpaceErr> {
        let stub = HyperwayStub::new(greet.surface.clone(), greet.agent.clone());
        let ext = self
            .interchange
            .mount(stub.clone(), Some(greet.into()))
            .await?;
        Ok(ext)
    }
}

#[async_trait]
impl<A, G> HyperGate for MountInterchangeGate<A, G>
where
    A: HyperAuthenticator,
    G: HyperGreeter,
{
    async fn knock(&self, knock: Knock) -> Result<HyperwayEndpoint, SpaceErr> {
        let stub = self.auth.auth(knock).await?;
        let greet = self.greeter.greet(stub).await?;
        let ext = self.enter(greet).await?;
        Ok(ext)
    }

    async fn jump(
        &self,
        _kind: InterchangeKind,
        stub: HyperwayStub,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
        let greet = self.greeter.greet(stub).await?;
        let ext = self.enter(greet).await?;
        Ok(ext)
    }
}

pub struct HyperClient {
    tx: mpsc::Sender<UltraWave>,
    status_rx: watch::Receiver<HyperConnectionStatus>,
    to_client_listener_tx: broadcast::Sender<UltraWave>,
    logger: PointLogger,
    greet_rx: watch::Receiver<Option<Greet>>,
    exchanger: Option<Exchanger>,
}

impl HyperClient {
    pub fn new(
        factory: Box<dyn HyperwayEndpointFactory>,
        logger: PointLogger,
    ) -> Result<HyperClient, SpaceErr> {
        Self::new_with_exchanger(factory, None, logger)
    }

    pub fn new_with_exchanger(
        factory: Box<dyn HyperwayEndpointFactory>,
        exchanger: Option<Exchanger>,
        logger: PointLogger,
    ) -> Result<HyperClient, SpaceErr> {
        let (to_client_listener_tx, _) = broadcast::channel(1024);
        let (to_hyperway_tx, from_client_rx) = mpsc::channel(1024);
        let (status_watch_tx, mut status_rx) = watch::channel(HyperConnectionStatus::Pending);

        let (status_mpsc_tx, mut status_mpsc_rx): (
            mpsc::Sender<HyperConnectionStatus>,
            mpsc::Receiver<HyperConnectionStatus>,
        ) = mpsc::channel(128);

        tokio::spawn(async move {
            while let Some(status) = status_mpsc_rx.recv().await {
                let result = status_watch_tx.send(status.clone());
                if status == HyperConnectionStatus::Fatal {
                    break;
                }
                if status == HyperConnectionStatus::Closed {
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

        let mut client = Self {
            tx: to_hyperway_tx,
            status_rx: status_rx.clone(),
            to_client_listener_tx: to_client_listener_tx.clone(),
            logger: logger.clone(),
            greet_rx,
            exchanger: exchanger.clone(),
        };

        {
            let logger = logger.clone();
            tokio::spawn(async move {
                while let Ok(_) = status_rx.changed().await {
                    let status = status_rx.borrow().clone();
                    //                    logger.info(format!("HyperClient status: {}", status.to_string()))
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
                    status_tx: mpsc::Sender<HyperConnectionStatus>,
                    greet_tx: watch::Sender<Option<Greet>>,
                    exchanger: Option<Exchanger>,
                    logger: PointLogger,
                ) -> Result<(), SpaceErr> {
                    if let Some(wave) = from_runner_rx.recv().await {
                        logger.track(&wave, || Tracker::new("client", "ReceiveReflected"));

                        match wave.to_reflected() {
                            Ok(reflected) => {
                                if !reflected.core().status.is_success() {
                                    match reflected.core().status.as_u16() {
                                        400 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "400: Bad Request: FATAL: something in the knock was incorrect";
                                            return Err(err.into());
                                        }
                                        401 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "401: Unauthorized: FATAL: authentication failed (bad credentials?)";
                                            return Err(err.into());
                                        }
                                        403 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "403: Forbidden: FATAL: authentication succeeded however the authenticated agent does not have permission to connect to this service";
                                            return Err(err.into());
                                        }
                                        408 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Panic)
                                                .await
                                                .unwrap_or_default();
                                            let err = "408: Request Timeout: PANIC";
                                            return Err(err.into());
                                        }
                                        301 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Fatal)
                                                .await
                                                .unwrap_or_default();
                                            let err = "301: Moved Permanently: FATAL: please update to new connection address";
                                            return Err(err.into());
                                        }
                                        503 => {
                                            status_tx
                                                .send(HyperConnectionStatus::Panic)
                                                .await
                                                .unwrap_or_default();
                                            let err =
                                                "503: Service Unavailable: PANIC: try again later";
                                            return Err(err.into());
                                        }
                                        _ => {
                                            status_tx
                                                .send(HyperConnectionStatus::Panic)
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
                                        .send(HyperConnectionStatus::Fatal)
                                        .await
                                        .unwrap_or_default();
                                    let err = "HyperClient expected first wave Substance to be a reflected Greeting";
                                    return Err(err.into());
                                }
                            }
                            Err(err) => {
                                status_tx
                                    .send(HyperConnectionStatus::Fatal)
                                    .await
                                    .unwrap_or_default();
                                let err = format!("HyperClient expected first wave Substance to be a reflected Greeting. Instead when attempting to convert to a reflected wave err occured: {}", err.to_string());
                                return Err(err.into());
                            }
                        }
                    }

                    while let Some(wave) = from_runner_rx.recv().await {
                        if exchanger.is_some() {
                            if wave.is_directed() {
                                to_client_listener_tx.send(wave)?;
                            } else {
                                exchanger
                                    .as_ref()
                                    .unwrap()
                                    .reflected(wave.to_reflected()?)
                                    .await?;
                            }
                        } else {
                            to_client_listener_tx.send(wave)?;
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
                .await
                .unwrap_or_default();
            });
        }

        Ok(client)
    }

    pub fn exchanger(&self) -> Option<Exchanger> {
        self.exchanger.clone()
    }

    pub async fn transmitter_builder(&self) -> Result<ProtoTransmitterBuilder, SpaceErr> {
        self.wait_for_ready(Duration::from_secs(30)).await?;
        let mut builder = ProtoTransmitterBuilder::new(
            Arc::new(self.router()),
            self.exchanger
                .as_ref()
                .ok_or(SpaceErr::server_error(
                    "cannot create a transmitter on a client that does not have an exchanger",
                ))?
                .clone(),
        );
        let greet = self
            .get_greeting()
            .ok_or::<SpaceErr>("expected greeting to already be set in HyperClient".into())?;
        builder.agent = SetStrategy::Fill(greet.agent.clone());
        builder.from = SetStrategy::Fill(greet.surface.clone());
        Ok(builder)
    }

    pub fn reset(&self) {
        let mut wave = DirectedProto::signal();
        wave.to(LOCAL_CLIENT_RUNNER.clone().to_surface());
        wave.method(ExtMethod::new("Reset").unwrap());
        let wave = wave.build().unwrap();
        let wave = wave.to_ultra();
        let tx = self.tx.clone();
        tokio::spawn(async move {
            tx.send(wave).await.unwrap_or_default();
        });
    }

    pub async fn close(&self) {
        let mut wave = DirectedProto::signal();
        wave.from(LOCAL_CLIENT.clone().to_surface());
        wave.to(LOCAL_CLIENT_RUNNER.clone().to_surface());
        wave.method(ExtMethod::new("Close").unwrap());
        let wave = wave.build().unwrap();
        let wave = wave.to_ultra();
        let tx = self.tx.clone();
        tokio::spawn(async move {
            tx.send(wave).await.unwrap_or_default();
        });
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

    pub async fn wait_for_greet(&self) -> Result<Greet, SpaceErr> {
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

    pub async fn wait_for_ready(&self, duration: Duration) -> Result<(), SpaceErr> {
        let mut status_rx = self.status_rx.clone();
        let (rtn, mut rtn_rx) = oneshot::channel();

        tokio::spawn(async move {
            loop {
                let status = status_rx.borrow().clone();
                match status {
                    HyperConnectionStatus::Ready => {
                        rtn.send(Ok(()));
                        break;
                    }
                    HyperConnectionStatus::Fatal => {
                        rtn.send(Err(SpaceErr::server_error(
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

#[derive(Clone, Eq, PartialEq)]
pub struct HyperConnectionDetails {
    pub status: HyperConnectionStatus,
    pub info: String,
}

impl HyperConnectionDetails {
    pub fn new<S: ToString>(status: HyperConnectionStatus, info: S) -> Self {
        Self {
            status,
            info: info.to_string(),
        }
    }
}

#[derive(Clone, strum_macros::Display, Eq, PartialEq)]
pub enum HyperConnectionStatus {
    Unknown,
    Pending,
    Connecting,
    Handshake,
    Auth,
    Ready,
    Panic,
    Fatal,
    Closed,
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

impl From<SpaceErr> for HyperConnectionErr {
    fn from(err: SpaceErr) -> Self {
        HyperConnectionErr::Retry(err.to_string())
    }
}

pub struct HyperClientRunner {
    ext: Option<HyperwayEndpoint>,
    factory: Box<dyn HyperwayEndpointFactory>,
    status_tx: mpsc::Sender<HyperConnectionStatus>,
    to_client_tx: mpsc::Sender<UltraWave>,
    from_client_rx: mpsc::Receiver<UltraWave>,
    logger: PointLogger,
}

impl HyperClientRunner {
    pub fn new(
        factory: Box<dyn HyperwayEndpointFactory>,
        from_client_rx: mpsc::Receiver<UltraWave>,
        status_tx: mpsc::Sender<HyperConnectionStatus>,
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
            .send(HyperConnectionStatus::Pending)
            .await
            .unwrap_or_default();

        loop {
            async fn connect(runner: &mut HyperClientRunner) -> Result<(), HyperConnectionErr> {
                if let Err(_) = runner
                    .status_tx
                    .send(HyperConnectionStatus::Connecting)
                    .await
                {
                    return Err(HyperConnectionErr::Fatal("can no longer update HyperClient status (probably due to previous Fatal status)".to_string()));
                }
                let (details_tx, mut details_rx): (
                    mpsc::Sender<HyperConnectionDetails>,
                    mpsc::Receiver<HyperConnectionDetails>,
                ) = mpsc::channel(1024);
                {
                    let logger = runner.logger.clone();
                    tokio::spawn(async move {
                        while let Some(detail) = details_rx.recv().await {
                            logger.info(format!("{} | {}", detail.status.to_string(), detail.info));
                        }
                    });
                }
                loop {
                    match runner.logger.result_ctx(
                        "connect",
                        tokio::time::timeout(
                            Duration::from_secs(60),
                            runner.factory.create(details_tx.clone()),
                        )
                        .await,
                    ) {
                        Ok(Ok(ext)) => {
                            runner.ext.replace(ext);
                            if let Err(_) =
                                runner.status_tx.send(HyperConnectionStatus::Ready).await
                            {
                                runner.ext.take();
                                return Err(HyperConnectionErr::Fatal("can no longer update HyperClient status (probably due to previous Fatal status)".to_string()));
                            }

                            return Ok(());
                        }
                        Ok(Err(err)) => {
                            //runner.logger.error(format!("{}", err.to_string()));
                        }
                        Err(err) => {
                            runner.logger.error(format!("{}", err.to_string()));
                        }
                    }
                    // wait a little while before attempting to reconnect
                    // maybe add exponential backoff later
                    tokio::time::sleep(Duration::from_secs(1)).await;
                }
            }

            async fn relay(runner: &mut HyperClientRunner) -> Result<(), SpaceErr> {
                let ext = runner
                    .ext
                    .as_mut()
                    .ok_or::<SpaceErr>("must reconnect".into())?;

                loop {
                    tokio::select!(
                        wave = runner.from_client_rx.recv() => {
                                // message comes from client, therefore it should go towards ext (unless it's pointed to the runner)
                                match wave {
                                  Some(wave) => {
                                    if wave.is_directed() && wave.to().is_single() && wave.to().unwrap_single().point == *LOCAL_CLIENT_RUNNER
                                    {
                                        let method: ExtMethod = wave.to_directed().unwrap().core().method.clone().try_into().unwrap();
                                        if method.to_string() == "Reset".to_string() {
                                           return Err(SpaceErr::server_error("reset"));
                                        } else if method.to_string() == "Close".to_string(){
                                            runner.status_tx.send(HyperConnectionStatus::Closed).await;
                                            return Ok(());
                                        }
                                    } else {
                                        match ext.tx.send(wave).await {
                                            Ok(_) => {}
                                            Err(err) => {
                                                // wave gets lost... need to requeue it somehow...
                                                //                                    runner.to_client_tx.try_send(err.0);
                                                return Err(SpaceErr::server_error("ext failure"));
                                            }
                                        }
                                    }
                                      }
                                      None => {
                                        //runner.logger.warn("from_client_rx.recv() returned None");
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
                                   runner.logger.warn("client hyperway_endpoint has been closed.  This can happen if the client sender (tx) has been dropped.");
                                    break;
                                }
                            }
                        }
                    );
                }

                //                runner.logger.warn("client relay interrupted");

                Ok(())
            }

            loop {
                match connect(&mut self).await {
                    Ok(_) => {}
                    Err(HyperConnectionErr::Fatal(message)) => {
                        // need to log the fatal error message somehow
                        self.status_tx
                            .send(HyperConnectionStatus::Fatal)
                            .await
                            .unwrap_or_default();
                        return;
                    }
                    Err(HyperConnectionErr::Retry(m)) => {
                        self.status_tx
                            .send(HyperConnectionStatus::Panic)
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
                        //self.logger.error(format!("{}", err.to_string()));
                        // some error occurred when relaying therefore we need to reconnect
                        self.ext = None;
                    }
                }
            }
        }
    }
}

#[async_trait]
pub trait HyperwayEndpointFactory: Send + Sync {
    async fn create(
        &self,
        status_tx: mpsc::Sender<HyperConnectionDetails>,
    ) -> Result<HyperwayEndpoint, SpaceErr>;
}

pub struct LocalHyperwayGateUnlocker {
    pub knock: Knock,
    pub gate: Arc<dyn HyperGate>,
}

impl LocalHyperwayGateUnlocker {
    pub fn new(remote: Surface, gate: Arc<dyn HyperGate>) -> Self {
        let knock = Knock::new(InterchangeKind::Singleton, remote, Substance::Empty);
        Self { knock, gate }
    }
}

#[async_trait]
impl HyperwayEndpointFactory for LocalHyperwayGateUnlocker {
    async fn create(
        &self,
        status_tx: mpsc::Sender<HyperConnectionDetails>,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
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
impl HyperwayEndpointFactory for LocalHyperwayGateJumper {
    async fn create(
        &self,
        status_tx: mpsc::Sender<HyperConnectionDetails>,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
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

// connects two interchanges one local, the other via client
pub struct Bridge {
    client: HyperClient,
}

impl Bridge {
    pub fn new(
        mut local_hyperway_endpoint: HyperwayEndpoint,
        remote_factory: Box<dyn HyperwayEndpointFactory>,
        logger: PointLogger,
    ) -> Result<Self, SpaceErr> {
        let client = HyperClient::new(remote_factory, logger)?;
        let client_router = client.router();
        let local_hyperway_endpoint_tx = local_hyperway_endpoint.tx.clone();
        tokio::spawn(async move {
            while let Some(wave) = local_hyperway_endpoint.rx.recv().await {
                client_router.route(wave).await;
            }
        });

        let mut rx = client.rx();
        tokio::spawn(async move {
            while let Ok(wave) = rx.recv().await {
                local_hyperway_endpoint_tx.send(wave).await;
            }
        });

        Ok(Self { client })
    }

    pub fn reset(&self) {
        self.client.reset();
    }

    pub fn close(&self) {
        self.client.close();
    }

    pub fn status(&self) -> HyperConnectionStatus {
        self.client.status_rx.borrow().clone()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::str::FromStr;
    use std::sync::Arc;

    use chrono::{DateTime, Utc};
    use dashmap::DashMap;

    use cosmic_space::command::direct::create::PointFactoryU64;
    use cosmic_space::hyper::{InterchangeKind, Knock};
    use cosmic_space::point::Point;
    use cosmic_space::point::Uuid;
    use cosmic_space::log::RootLogger;
    use cosmic_space::substance::Substance;
    use cosmic_space::wave::HyperWave;

    use crate::{
        AnonHyperAuthenticator, HyperGate, HyperGateSelector, HyperRouter, HyperwayInterchange,
        InterchangeGate,
    };

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
    #[tokio::mem]
    async fn hyper_test() {
        let point = Point::from_str("mem").unwrap();
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

pub mod test_util {
    use std::collections::{HashMap, HashSet};
    use std::str::FromStr;
    use std::sync::Arc;
    use std::time::Duration;

    use dashmap::DashMap;
    use lazy_static::lazy_static;
    use tokio::sync::{broadcast, mpsc, oneshot};

    use cosmic_space::command::direct::create::PointFactoryU64;
    use cosmic_space::err::SpaceErr;
    use cosmic_space::hyper::{Greet, InterchangeKind, Knock};
    use cosmic_space::point::{Layer, Point, Surface, ToPoint, ToSurface};
    use cosmic_space::log::{PointLogger, RootLogger};
    use cosmic_space::settings::Timeouts;
    use cosmic_space::substance::{Substance, Token};
    use cosmic_space::wave::core::cmd::CmdMethod;
    use cosmic_space::wave::core::ext::ExtMethod;
    use cosmic_space::wave::core::{Method, ReflectedCore};
    use cosmic_space::wave::exchange::asynch::{
        Exchanger, ProtoTransmitter, ProtoTransmitterBuilder, Router, TxRouter,
    };
    use cosmic_space::wave::exchange::SetStrategy;
    use cosmic_space::wave::{
        Agent, DirectedKind, DirectedProto, HyperWave, Pong, ReflectedKind, ReflectedProto,
        ReflectedWave, UltraWave, Wave,
    };

    use crate::{
        AnonHyperAuthenticator, AnonHyperAuthenticatorAssignEndPoint, Bridge, HyperClient,
        HyperConnectionDetails, HyperConnectionErr, HyperGate, HyperGateSelector, HyperGreeter,
        HyperRouter, Hyperlane, Hyperway, HyperwayEndpoint, HyperwayEndpointFactory,
        HyperwayInterchange, HyperwayStub, InterchangeGate, LocalHyperwayGateJumper,
        LocalHyperwayGateUnlocker, MountInterchangeGate, TokenAuthenticatorWithRemoteWhitelist,
    };

    lazy_static! {
        pub static ref LESS: Point = Point::from_str("space:users:less").expect("point");
        pub static ref FAE: Point = Point::from_str("space:users:fae").expect("point");
    }

    #[derive(Clone)]
    pub struct SingleInterchangePlatform {
        pub interchange: Arc<HyperwayInterchange>,
        pub gate: Arc<HyperGateSelector>,
    }

    impl SingleInterchangePlatform {
        pub async fn new() -> Self {
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("point").unwrap());
            let interchange = Arc::new(HyperwayInterchange::new(
                logger.push_point("interchange").unwrap(),
            ));

            interchange
                .add(Hyperway::new(
                    LESS.clone().to_surface(),
                    LESS.to_agent(),
                    Default::default(),
                ))
                .await;
            interchange
                .add(Hyperway::new(
                    FAE.clone().to_surface(),
                    FAE.to_agent(),
                    Default::default(),
                ))
                .await;
            let auth = AnonHyperAuthenticator::new();
            let gate = Arc::new(MountInterchangeGate::new(
                auth,
                TestGreeter::new(),
                interchange.clone(),
                logger.push_point("gate").unwrap(),
            ));
            let mut gates: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>> =
                Arc::new(DashMap::new());
            gates.insert(InterchangeKind::Singleton, gate);
            let gate = Arc::new(HyperGateSelector::new(gates));

            Self { interchange, gate }
        }

        pub fn knock(&self, surface: Surface) -> Knock {
            Knock::new(InterchangeKind::Singleton, surface, Substance::Empty)
        }

        pub fn local_hyperway_endpoint_factory(
            &self,
            port: Surface,
        ) -> Box<dyn HyperwayEndpointFactory> {
            Box::new(LocalHyperwayGateUnlocker::new(port, self.gate.clone()))
        }
    }
    pub struct LargeFrameTest {
        fae_factory: Box<dyn HyperwayEndpointFactory>,
        less_factory: Box<dyn HyperwayEndpointFactory>,
    }

    impl LargeFrameTest {
        pub fn new(
            fae_factory: Box<dyn HyperwayEndpointFactory>,
            less_factory: Box<dyn HyperwayEndpointFactory>,
        ) -> Self {
            Self {
                fae_factory,
                less_factory,
            }
        }

        pub async fn go(self) -> Result<(), SpaceErr> {
            let less_exchanger = Exchanger::new(
                LESS.push("exchanger").unwrap().to_surface(),
                Timeouts::default(),
                PointLogger::default(),
            );
            let fae_exchanger = Exchanger::new(
                FAE.push("exchanger").unwrap().to_surface(),
                Timeouts::default(),
                PointLogger::default(),
            );

            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("less-client").unwrap());
            let less_client = HyperClient::new_with_exchanger(
                self.less_factory,
                Some(less_exchanger.clone()),
                logger,
            )
            .unwrap();
            let logger = root_logger.point(Point::from_str("fae-client").unwrap());
            let fae_client = HyperClient::new_with_exchanger(
                self.fae_factory,
                Some(fae_exchanger.clone()),
                logger,
            )
            .unwrap();

            let mut less_rx = less_client.rx();
            let mut fae_rx = fae_client.rx();

            let less_router = less_client.router();
            let less_transmitter =
                ProtoTransmitter::new(Arc::new(less_router), less_exchanger.clone());

            let fae_router = fae_client.router();
            let fae_transmitter =
                ProtoTransmitter::new(Arc::new(fae_router), fae_exchanger.clone());

            {
                let fae = FAE.clone();
                tokio::spawn(async move {
                    fae_client.wait_for_greet().await.unwrap();
                    let wave = fae_rx.recv().await.unwrap();
                    let mut reflected = ReflectedProto::new();
                    reflected.kind(ReflectedKind::Pong);
                    reflected.status(200u16);
                    reflected.to(wave.from().clone());
                    reflected.from(fae.to_surface());
                    reflected.intended(wave.to());
                    reflected.reflection_of(wave.id());
                    let wave = reflected.build().unwrap();
                    let wave = wave.to_ultra();
                    fae_transmitter.route(wave).await;
                });
            }

            let (rtn, mut rtn_rx) = oneshot::channel();
            tokio::spawn(async move {
                less_client.wait_for_greet().await.unwrap();
                let mut hello = DirectedProto::ping();
                hello.to(FAE.clone().to_surface());
                hello.from(LESS.clone().to_surface());
                hello.method(ExtMethod::new("Hello").unwrap());
                let size = 3_000_000usize;
                let mut body = Vec::with_capacity(size);
                for _ in 0..size {
                    body.push(0u8);
                }
                hello.body(Substance::Bin(Arc::new(body)));
                let pong: Wave<Pong> = less_transmitter.direct(hello).await.unwrap();
                rtn.send(pong.core.status.as_u16() == 200u16);
            });

            let result = tokio::time::timeout(Duration::from_secs(30), rtn_rx)
                .await
                .unwrap()
                .unwrap();
            assert!(result);
            Ok(())
        }
    }

    pub struct WaveTest {
        fae_factory: Box<dyn HyperwayEndpointFactory>,
        less_factory: Box<dyn HyperwayEndpointFactory>,
    }

    impl WaveTest {
        pub fn new(
            fae_factory: Box<dyn HyperwayEndpointFactory>,
            less_factory: Box<dyn HyperwayEndpointFactory>,
        ) -> Self {
            Self {
                fae_factory,
                less_factory,
            }
        }

        pub async fn go(self) -> Result<(), SpaceErr> {
            let less_exchanger = Exchanger::new(
                LESS.push("exchanger").unwrap().to_surface(),
                Timeouts::default(),
                PointLogger::default(),
            );
            let fae_exchanger = Exchanger::new(
                FAE.push("exchanger").unwrap().to_surface(),
                Timeouts::default(),
                PointLogger::default(),
            );

            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("less-client").unwrap());
            let less_client = HyperClient::new_with_exchanger(
                self.less_factory,
                Some(less_exchanger.clone()),
                logger,
            )
            .unwrap();
            let logger = root_logger.point(Point::from_str("fae-client").unwrap());
            let fae_client = HyperClient::new_with_exchanger(
                self.fae_factory,
                Some(fae_exchanger.clone()),
                logger,
            )
            .unwrap();

            let mut less_rx = less_client.rx();
            let mut fae_rx = fae_client.rx();

            let less_router = less_client.router();
            let less_transmitter =
                ProtoTransmitter::new(Arc::new(less_router), less_exchanger.clone());

            let fae_router = fae_client.router();
            let fae_transmitter =
                ProtoTransmitter::new(Arc::new(fae_router), fae_exchanger.clone());

            {
                let fae = FAE.clone();
                tokio::spawn(async move {
                    fae_client.wait_for_greet().await.unwrap();
                    let wave = fae_rx.recv().await.unwrap();
                    let mut reflected = ReflectedProto::new();
                    reflected.kind(ReflectedKind::Pong);
                    reflected.status(200u16);
                    reflected.to(wave.from().clone());
                    reflected.from(fae.to_surface());
                    reflected.intended(wave.to());
                    reflected.reflection_of(wave.id());
                    let wave = reflected.build().unwrap();
                    let wave = wave.to_ultra();
                    fae_transmitter.route(wave).await;
                });
            }

            let (rtn, mut rtn_rx) = oneshot::channel();
            tokio::spawn(async move {
                less_client.wait_for_greet().await.unwrap();
                let mut hello = DirectedProto::ping();
                hello.to(FAE.clone().to_surface());
                hello.from(LESS.clone().to_surface());
                hello.method(ExtMethod::new("Hello").unwrap());
                hello.body(Substance::Empty);
                let pong: Wave<Pong> = less_transmitter.direct(hello).await.unwrap();
                rtn.send(pong.core.status.as_u16() == 200u16);
            });

            let result = tokio::time::timeout(Duration::from_secs(30), rtn_rx)
                .await
                .unwrap()
                .unwrap();
            assert!(result);
            Ok(())
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
        async fn greet(&self, stub: HyperwayStub) -> Result<Greet, SpaceErr> {
            Ok(Greet {
                surface: stub.remote.clone(),
                agent: stub.agent.clone(),
                hop: Point::remote_endpoint()
                    .to_surface()
                    .with_layer(Layer::Core),
                transport: stub.remote.clone(),
            })
        }
    }
}

#[cfg(test)]
pub mod test {
    use std::collections::{HashMap, HashSet};
    use std::str::FromStr;
    use std::sync::Arc;
    use std::time::Duration;

    use dashmap::DashMap;
    use lazy_static::lazy_static;
    use tokio::sync::{broadcast, mpsc, oneshot};

    use cosmic_space::command::direct::create::PointFactoryU64;
    use cosmic_space::err::SpaceErr;
    use cosmic_space::hyper::{Greet, InterchangeKind, Knock};
    use cosmic_space::point::{Layer, Point, Surface, ToPoint, ToSurface};
    use cosmic_space::log::RootLogger;
    use cosmic_space::settings::Timeouts;
    use cosmic_space::substance::{Substance, Token};
    use cosmic_space::wave::core::cmd::CmdMethod;
    use cosmic_space::wave::core::ext::ExtMethod;
    use cosmic_space::wave::core::{Method, ReflectedCore};
    use cosmic_space::wave::exchange::asynch::{
        Exchanger, ProtoTransmitter, ProtoTransmitterBuilder, Router, TxRouter,
    };
    use cosmic_space::wave::exchange::SetStrategy;
    use cosmic_space::wave::{
        Agent, DirectedKind, DirectedProto, HyperWave, Pong, ReflectedKind, ReflectedProto,
        ReflectedWave, UltraWave, Wave,
    };

    use crate::test_util::{SingleInterchangePlatform, TestGreeter, WaveTest, FAE, LESS};
    use crate::{
        AnonHyperAuthenticator, AnonHyperAuthenticatorAssignEndPoint, Bridge, HyperClient,
        HyperConnectionDetails, HyperConnectionErr, HyperGate, HyperGateSelector, HyperGreeter,
        HyperRouter, Hyperlane, Hyperway, HyperwayEndpoint, HyperwayEndpointFactory,
        HyperwayInterchange, HyperwayStub, InterchangeGate, LocalHyperwayGateJumper,
        LocalHyperwayGateUnlocker, MountInterchangeGate, TokenAuthenticatorWithRemoteWhitelist,
    };

    pub struct TestRouter {}

    #[async_trait]
    impl HyperRouter for TestRouter {
        async fn route(&self, wave: HyperWave) {
            println!("Test Router routing!");
            //    todo!()
        }
    }

    fn hello_wave() -> UltraWave {
        let mut hello = DirectedProto::ping();
        hello.to(FAE.clone().to_surface());
        hello.from(LESS.clone().to_surface());
        hello.method(ExtMethod::new("Hello").unwrap());
        hello.body(Substance::Empty);
        let directed = hello.build().unwrap();
        let wave = directed.to_ultra();
        wave
    }

    #[tokio::test]
    pub async fn test_hyperlane() {
        let hyperlane = Hyperlane::new("mem");
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
        let hyperway = Hyperway::new(
            LESS.clone().to_surface(),
            LESS.to_agent(),
            Default::default(),
        );
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
    #[tokio::mem]
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

    // disabled for now
    //#[tokio::test]
    pub async fn test_hyperclient() {
        pub struct TestFactory {
            pub hyperway: Hyperway,
        }

        impl TestFactory {
            pub fn new() -> Self {
                let hyperway = Hyperway::new(
                    LESS.clone().to_surface(),
                    LESS.to_agent(),
                    Default::default(),
                );
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
        impl HyperwayEndpointFactory for TestFactory {
            async fn create(
                &self,
                status_tx: mpsc::Sender<HyperConnectionDetails>,
            ) -> Result<HyperwayEndpoint, SpaceErr> {
                Ok(self.hyperway.hyperway_endpoint_far(None).await)
            }
        }

        {
            let factory = Box::new(TestFactory::new());
            let mut inbound_rx = factory.inbound_rx().await;
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("client").unwrap());
            let client = HyperClient::new(factory, logger).unwrap();

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
            let factory = Box::new(TestFactory::new());
            let outbound_tx = factory.outbound_tx();
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str("client").unwrap());
            let client = HyperClient::new(factory, logger).unwrap();

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
    pub async fn test_single_interchange() {
        let test = SingleInterchangePlatform::new().await;
        let less_factory = test.local_hyperway_endpoint_factory(LESS.to_surface());
        let fae_factory = test.local_hyperway_endpoint_factory(FAE.to_surface());
        let test = WaveTest::new(fae_factory, less_factory);
        test.go().await.unwrap();
    }

    #[tokio::test]
    pub async fn test_dual_interchange() {
        let root_logger = RootLogger::default();
        let logger = root_logger.point(Point::from_str("point").unwrap());
        let interchange = Arc::new(HyperwayInterchange::new(
            logger.push_point("interchange").unwrap(),
        ));

        interchange
            .add(Hyperway::new(
                LESS.clone().to_surface(),
                LESS.to_agent(),
                Default::default(),
            ))
            .await;
        interchange
            .add(Hyperway::new(
                FAE.clone().to_surface(),
                FAE.to_agent(),
                Default::default(),
            ))
            .await;

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

        let less_factory = Box::new(LocalHyperwayGateUnlocker::new(
            LESS.clone().to_surface(),
            gate.clone(),
        ));

        let fae_factory = Box::new(LocalHyperwayGateUnlocker::new(
            FAE.clone().to_surface(),
            gate.clone(),
        ));

        let less_exchanger = Exchanger::new(
            LESS.push("exchanger").unwrap().to_surface(),
            Timeouts::default(),
            Default::default(),
        );
        let fae_exchanger = Exchanger::new(
            FAE.push("exchanger").unwrap().to_surface(),
            Timeouts::default(),
            Default::default(),
        );

        let root_logger = RootLogger::default();
        let logger = root_logger.point(Point::from_str("less-client").unwrap());
        let less_client =
            HyperClient::new_with_exchanger(less_factory, Some(less_exchanger.clone()), logger)
                .unwrap();
        let logger = root_logger.point(Point::from_str("fae-client").unwrap());
        let fae_client =
            HyperClient::new_with_exchanger(fae_factory, Some(fae_exchanger.clone()), logger)
                .unwrap();

        let mut less_rx = less_client.rx();
        let mut fae_rx = fae_client.rx();

        let less_router = less_client.router();
        let less_transmitter = ProtoTransmitter::new(Arc::new(less_router), less_exchanger.clone());

        let fae_router = fae_client.router();
        let fae_transmitter = ProtoTransmitter::new(Arc::new(fae_router), fae_exchanger.clone());

        {
            let fae = FAE.clone();
            tokio::spawn(async move {
                let wave = fae_rx.recv().await.unwrap();
                let mut reflected = ReflectedProto::new();
                reflected.kind(ReflectedKind::Pong);
                reflected.status(200u16);
                reflected.to(wave.from().clone());
                reflected.from(fae.to_surface());
                reflected.intended(wave.to());
                reflected.reflection_of(wave.id());
                let wave = reflected.build().unwrap();
                let wave = wave.to_ultra();
                fae_transmitter.route(wave).await;
            });
        }

        let (rtn, mut rtn_rx) = oneshot::channel();
        tokio::spawn(async move {
            let mut hello = DirectedProto::ping();
            hello.to(FAE.clone().to_surface());
            hello.from(LESS.clone().to_surface());
            hello.method(ExtMethod::new("Hello").unwrap());
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

    #[tokio::test]
    pub async fn test_bridge() {
        pub fn create(name: &str) -> (Arc<HyperwayInterchange>, Arc<dyn HyperGate>) {
            let root_logger = RootLogger::default();
            let logger = root_logger.point(Point::from_str(name).unwrap());
            let interchange = Arc::new(HyperwayInterchange::new(
                logger.push_point("interchange").unwrap(),
            ));

            let auth = AnonHyperAuthenticator::new();
            let gate = Arc::new(MountInterchangeGate::new(
                auth,
                TestGreeter::new(),
                interchange.clone(),
                logger.push_point("gate").unwrap(),
            ));
            let mut gates: Arc<DashMap<InterchangeKind, Arc<dyn HyperGate>>> =
                Arc::new(DashMap::new());
            gates.insert(InterchangeKind::Singleton, gate);
            (interchange, Arc::new(HyperGateSelector::new(gates)))
        }

        let (less_interchange, less_gate) = create("less");
        let (fae_interchange, fae_gate) = create("fae");

        {
            let hyperway = Hyperway::new(
                FAE.to_surface().with_layer(Layer::Core),
                Agent::HyperUser,
                Default::default(),
            );
            less_interchange.add(hyperway).await;
            let access = Hyperway::new(
                LESS.to_surface().with_layer(Layer::Core),
                Agent::HyperUser,
                Default::default(),
            );
            less_interchange.add(access).await;
        }
        {
            let hyperway = Hyperway::new(
                LESS.to_surface().with_layer(Layer::Core),
                Agent::HyperUser,
                Default::default(),
            );
            fae_interchange.add(hyperway).await;
            let access = Hyperway::new(
                FAE.to_surface().with_layer(Layer::Core),
                Agent::HyperUser,
                Default::default(),
            );
            fae_interchange.add(access).await;
        }

        let fae_endpoint_from_less = less_interchange
            .mount(
                HyperwayStub {
                    remote: FAE.to_surface().with_layer(Layer::Core),
                    agent: Agent::HyperUser,
                },
                None,
            )
            .await
            .unwrap();
        let fae_factory = Box::new(LocalHyperwayGateUnlocker::new(
            LESS.clone().to_surface(),
            fae_gate.clone(),
        ));
        let logger = RootLogger::default().point(Point::from_str("bridge").unwrap());
        let bridge = Bridge::new(fae_endpoint_from_less, fae_factory, logger);

        let mut less_access = less_interchange
            .mount(
                HyperwayStub {
                    remote: LESS.to_surface().with_layer(Layer::Core),
                    agent: Agent::HyperUser,
                },
                None,
            )
            .await
            .unwrap();
        let mut fae_access = fae_interchange
            .mount(
                HyperwayStub {
                    remote: FAE.to_surface().with_layer(Layer::Core),
                    agent: Agent::HyperUser,
                },
                None,
            )
            .await
            .unwrap();

        tokio::spawn(async move {
            while let Some(wave) = fae_access.rx.recv().await {
                if wave.is_directed() {
                    let directed = wave.to_directed().unwrap();
                    let reflection = directed.reflection().unwrap();
                    let reflection = reflection.make(
                        ReflectedCore::ok(),
                        FAE.to_surface().with_layer(Layer::Core),
                    );
                    fae_access.tx.send(reflection.to_ultra()).await.unwrap();
                }
            }
        });

        let exchanger = Exchanger::new(LESS.to_surface(), Timeouts::default(), Default::default());
        let less_tx = less_access.tx.clone();

        {
            let exchanger = exchanger.clone();
            tokio::spawn(async move {
                while let Some(wave) = less_access.rx.recv().await {
                    if wave.is_reflected() {
                        exchanger
                            .reflected(wave.to_reflected().unwrap())
                            .await
                            .unwrap();
                    }
                }
            });
        }
        let mut transmitter =
            ProtoTransmitterBuilder::new(Arc::new(TxRouter::new(less_tx.clone())), exchanger);
        transmitter.from = SetStrategy::Override(LESS.to_surface());
        transmitter.agent = SetStrategy::Override(Agent::HyperUser);
        let transmitter = transmitter.build();
        let mut wave = DirectedProto::ping();
        wave.method(Method::Cmd(CmdMethod::Bounce));
        wave.to(FAE.to_surface().with_layer(Layer::Core));
        let reply: Wave<Pong> =
            tokio::time::timeout(Duration::from_secs(5), transmitter.direct(wave))
                .await
                .unwrap()
                .unwrap();
        assert!(reply.core.status.is_success());
        assert_eq!(reply.core.body, Substance::Empty);
        assert_eq!(reply.to, LESS.to_surface());
        assert_eq!(reply.from, FAE.to_surface());
        println!("Ok");
    }
}
