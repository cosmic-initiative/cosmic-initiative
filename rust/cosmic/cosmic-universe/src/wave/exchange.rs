use alloc::borrow::Cow;
use std::ops::Deref;
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use http::StatusCode;
use tokio::sync::{broadcast, mpsc, oneshot};

use crate::{Agent, Point, ReflectedCore, Substance, Surface, ToSubstance, UniErr};
use crate::config::bind::RouteSelector;
use crate::loc::{Topic, ToPoint, ToSurface};
use crate::log::{PointLogger, RootLogger, SpanLogger};
use crate::settings::Timeouts;
use crate::wave::{Bounce, BounceBacks, BounceProto, DirectedProto, DirectedWave, Echo, FromReflectedAggregate, Handling, Pong, Recipients, RecipientSelector, ReflectedAggregate, ReflectedProto, ReflectedWave, Scope, Session, ToRecipients, UltraWave, Wave, WaveId};
use crate::wave::core::CoreBounce;

#[derive(Clone)]
pub struct Exchanger {
    pub surface: Surface,
    pub multis: Arc<DashMap<WaveId, mpsc::Sender<ReflectedWave>>>,
    pub singles: Arc<DashMap<WaveId, oneshot::Sender<ReflectedAggregate>>>,
    pub timeouts: Timeouts,
}

impl Exchanger {
    pub fn new(surface: Surface, timeouts: Timeouts) -> Self {
        Self {
            surface,
            singles: Arc::new(DashMap::new()),
            multis: Arc::new(DashMap::new()),
            timeouts,
        }
    }

    pub fn with_surface(&self, surface: Surface) -> Self {
        Self {
            surface,
            singles: self.singles.clone(),
            multis: self.multis.clone(),
            timeouts: self.timeouts.clone(),
        }
    }

    pub async fn reflected(&self, reflect: ReflectedWave) -> Result<(), UniErr> {
        if let Some(multi) = self.multis.get(reflect.reflection_of()) {
            multi.value().send(reflect).await;
        } else if let Some((_, tx)) = self.singles.remove(reflect.reflection_of()) {
            tx.send(ReflectedAggregate::Single(reflect));
        } else {
            let reflect = reflect.to_ultra();
            let kind = match &reflect {
                UltraWave::Ping(_) => "Ping",
                UltraWave::Pong(_) => "Pong",
                UltraWave::Ripple(_) => "Ripple",
                UltraWave::Echo(_) => "Echo",
                UltraWave::Signal(_) => "Signal",
            };
            let reflect = reflect.to_reflected()?;

            return Err(UniErr::from_500(format!(
                "Not expecting reflected message from: {} to: {} KIND: {} STATUS: {}",
                reflect.from().to_string(),
                reflect.to().to_string(),
                kind,
                reflect.core().status.to_string()
            )));
        }
        Ok(())
    }

    pub async fn exchange(&self, directed: &DirectedWave) -> oneshot::Receiver<ReflectedAggregate> {
        let (tx, rx) = oneshot::channel();

        let mut reflected = match directed.reflected_proto() {
            BounceProto::Absorbed => {
                return rx;
            }
            BounceProto::Reflected(reflected) => reflected,
        };

        reflected.from(self.surface.clone());

        let reflection = directed.reflection().unwrap();

        let timeout = self.timeouts.from(directed.handling().wait.clone());
        self.singles.insert(directed.id().clone(), tx);
        match directed.bounce_backs() {
            BounceBacks::None => {
                panic!("we already dealt with this")
            }
            BounceBacks::Single => {
                let singles = self.singles.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_secs(timeout)).await;
                    let id = reflected.reflection_of.as_ref().unwrap();
                    if let Some((_, tx)) = singles.remove(id) {
                        reflected.status = Some(StatusCode::from_u16(408).unwrap());
                        reflected.body = Some(Substance::Empty);
                        reflected.intended = Some(reflection.intended);
                        let reflected = reflected.build().unwrap();
                        tx.send(ReflectedAggregate::Single(reflected));
                    }
                });
            }
            BounceBacks::Count(count) => {
                let (tx, mut rx) = mpsc::channel(count);
                self.multis.insert(directed.id().clone(), tx);
                let singles = self.singles.clone();
                let id = directed.id().clone();
                tokio::spawn(async move {
                    let mut agg = vec![];
                    loop {
                        if let Some(reflected) = rx.recv().await {
                            agg.push(reflected);
                            if count == agg.len() {
                                if let Some((_, tx)) = singles.remove(&id) {
                                    tx.send(ReflectedAggregate::Multi(agg));
                                    break;
                                }
                            }
                        } else {
                            // this would occur in a timeout scenario
                            if let Some((_, tx)) = singles.remove(&id) {
                                reflected.status = Some(StatusCode::from_u16(408).unwrap());
                                reflected.body = Some(Substance::Empty);
                                reflected.intended = Some(reflection.intended);
                                let reflected = reflected.build().unwrap();
                                tx.send(ReflectedAggregate::Multi(vec![reflected]));
                                break;
                            }
                        }
                    }
                });

                let id = directed.id().clone();
                let multis = self.multis.clone();
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_secs(timeout)).await;
                    // all we have to do is remove it, the multi loop will take care of the rest
                    multis.remove(&id);
                });
            }
            BounceBacks::Timer(wait) => {
                let (tx, mut rx) = mpsc::channel(32);
                self.multis.insert(directed.id().clone(), tx);
                let singles = self.singles.clone();
                let id = directed.id().clone();
                tokio::spawn(async move {
                    let mut agg = vec![];
                    loop {
                        if let Some(reflected) = rx.recv().await {
                            agg.push(reflected);
                        } else {
                            // this would occur in a timeout scenario
                            if let Some((_, tx)) = singles.remove(&id) {
                                tx.send(ReflectedAggregate::Multi(agg));
                                break;
                            }
                        }
                    }
                });

                let id = directed.id().clone();
                let multis = self.multis.clone();
                let timeout = self.timeouts.from(wait);
                tokio::spawn(async move {
                    tokio::time::sleep(Duration::from_secs(timeout)).await;
                    // all we have to do is remove it, the multi loop will take care of the rest
                    multis.remove(&id);
                });
            }
        }

        rx
    }
}

impl Default for Exchanger {
    fn default() -> Self {
        Self::new(Point::root().to_surface(), Default::default())
    }
}

#[derive(Clone)]
pub struct DirectedHandlerShell<D>
where
    D: DirectedHandler,
{
    logger: PointLogger,
    handler: D,
    surface: Surface,
    builder: ProtoTransmitterBuilder,
}

impl<D> DirectedHandlerShell<D>
where
    D: DirectedHandler,
{
    pub fn new(
        handler: D,
        builder: ProtoTransmitterBuilder,
        surface: Surface,
        logger: RootLogger,
    ) -> Self {
        let logger = logger.point(surface.point.clone());
        Self {
            handler,
            builder,
            surface,
            logger,
        }
    }

    pub async fn handle(&self, wave: DirectedWave) {
        let logger = self
            .logger
            .point(self.surface.clone().to_point())
            .spanner(&wave);
        let mut transmitter = self.builder.clone().build();
        let reflection = wave.reflection();
        let ctx = RootInCtx::new(wave, self.surface.clone(), logger, transmitter);
        match self.handler.handle(ctx).await {
            CoreBounce::Absorbed => {}
            CoreBounce::Reflected(reflected) => {
                let wave = reflection.unwrap().make(reflected, self.surface.clone());
                let wave = wave.to_ultra();
                let transmitter = self.builder.clone().build();
                transmitter.route(wave).await;
            }
        }
    }
}

pub struct InternalPipeline<H> {
    pub selector: RouteSelector,
    pub handler: H,
}

impl<H> InternalPipeline<H> {
    pub fn new(selector: RouteSelector, mut handler: H) -> Self {
        Self { selector, handler }
    }
}

#[async_trait]
pub trait DirectedHandlerSelector {
    fn select<'a>(&self, select: &'a RecipientSelector<'a>) -> Result<&dyn DirectedHandler, ()>;
}

#[async_trait]
pub trait DirectedHandler: Send + Sync {
    async fn handle(&self, ctx: RootInCtx) -> CoreBounce;

    async fn bounce(&self, ctx: RootInCtx) -> CoreBounce {
        CoreBounce::Reflected(ReflectedCore::ok())
    }
}

pub struct RootInCtx {
    pub to: Surface,
    pub wave: DirectedWave,
    pub session: Option<Session>,
    pub logger: SpanLogger,
    pub transmitter: ProtoTransmitter,
}

impl RootInCtx {
    pub fn new(
        wave: DirectedWave,
        to: Surface,
        logger: SpanLogger,
        transmitter: ProtoTransmitter,
    ) -> Self {
        Self {
            wave,
            to,
            logger,
            session: None,
            transmitter: transmitter,
        }
    }

    pub fn status(self, status: u16, from: Surface) -> Bounce<ReflectedWave> {
        match self.wave {
            DirectedWave::Ping(ping) => Bounce::Reflected(ReflectedWave::Pong(Wave::new(
                Pong::new(
                    ReflectedCore::status(status),
                    ping.from.clone(),
                    self.to.clone().to_recipients(),
                    ping.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Ripple(ripple) => Bounce::Reflected(ReflectedWave::Echo(Wave::new(
                Echo::new(
                    ReflectedCore::status(status),
                    ripple.from.clone(),
                    ripple.to.clone(),
                    ripple.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Signal(_) => Bounce::Absorbed,
        }
    }

    pub fn err(self, status: u16, from: Surface, msg: String) -> Bounce<ReflectedWave> {
        match self.wave {
            DirectedWave::Ping(ping) => Bounce::Reflected(ReflectedWave::Pong(Wave::new(
                Pong::new(
                    ReflectedCore::fail(status, msg),
                    ping.from.clone(),
                    self.to.clone().to_recipients(),
                    ping.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Ripple(ripple) => Bounce::Reflected(ReflectedWave::Echo(Wave::new(
                Echo::new(
                    ReflectedCore::fail(status, msg),
                    ripple.from.clone(),
                    ripple.to.clone(),
                    ripple.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Signal(_) => Bounce::Absorbed,
        }
    }

    pub fn not_found(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        let msg = format!(
            "<{}>{}",
            self.wave.core().method.to_string(),
            self.wave.core().uri.path().to_string()
        );
        self.err(404, to, msg)
    }

    pub fn timeout(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        self.status(408, to)
    }

    pub fn bad_request(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        let msg = format!(
            "<{}>{} -[ {} ]->",
            self.wave.core().method.to_string(),
            self.wave.core().uri.path().to_string(),
            self.wave.core().body.kind().to_string()
        );
        self.err(400, to, msg)
    }

    pub fn server_error(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        self.status(500, to)
    }

    pub fn forbidden(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        let msg = format!(
            "<{}>{} -[ {} ]->",
            self.wave.core().method.to_string(),
            self.wave.core().uri.path().to_string(),
            self.wave.core().body.kind().to_string()
        );
        self.err(401, to, msg)
    }

    pub fn unavailable(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        self.status(503, to)
    }

    pub fn unauthorized(self) -> Bounce<ReflectedWave> {
        let to = self.to.clone();
        self.status(403, to)
    }
}

impl RootInCtx {
    pub fn push<'a, I>(&self) -> Result<InCtx<I>, UniErr>
    where
        Substance: ToSubstance<I>,
    {
        let input = match self.wave.to_substance_ref() {
            Ok(input) => input,
            Err(err) => return Err(err.into()),
        };
        Ok(InCtx {
            root: self,
            input,
            logger: self.logger.clone(),
            transmitter: Cow::Borrowed(&self.transmitter),
        })
    }
}

pub struct InCtx<'a, I> {
    root: &'a RootInCtx,
    pub transmitter: Cow<'a, ProtoTransmitter>,
    pub input: &'a I,
    pub logger: SpanLogger,
}

impl<'a, I> Deref for InCtx<'a, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl<'a, I> InCtx<'a, I> {
    pub fn new(
        root: &'a RootInCtx,
        input: &'a I,
        tx: Cow<'a, ProtoTransmitter>,
        logger: SpanLogger,
    ) -> Self {
        Self {
            root,
            input,
            logger,
            transmitter: tx,
        }
    }

    pub fn from(&self) -> &Surface {
        self.root.wave.from()
    }

    pub fn to(&self) -> &Surface {
        &self.root.to
    }

    pub fn push(self) -> InCtx<'a, I> {
        InCtx {
            root: self.root,
            input: self.input,
            logger: self.logger.span(),
            transmitter: self.transmitter.clone(),
        }
    }

    pub fn push_from(self, from: Surface) -> InCtx<'a, I> {
        let mut transmitter = self.transmitter.clone();
        transmitter.to_mut().from = SetStrategy::Override(from);
        InCtx {
            root: self.root,
            input: self.input,
            logger: self.logger.clone(),
            transmitter,
        }
    }

    pub fn push_input_ref<I2>(self, input: &'a I2) -> InCtx<'a, I2> {
        InCtx {
            root: self.root,
            input,
            logger: self.logger.clone(),
            transmitter: self.transmitter.clone(),
        }
    }

    pub fn wave(&self) -> &DirectedWave {
        &self.root.wave
    }

    pub async fn ping(&self, req: DirectedProto) -> Result<Wave<Pong>, UniErr> {
        self.transmitter.direct(req).await
    }
}

impl<'a, I> InCtx<'a, I> {
    pub fn ok_body(self, substance: Substance) -> ReflectedCore {
        self.root.wave.core().ok_body(substance)
    }

    pub fn not_found(self) -> ReflectedCore {
        self.root.wave.core().not_found()
    }

    pub fn forbidden(self) -> ReflectedCore {
        self.root.wave.core().forbidden()
    }

    pub fn bad_request(self) -> ReflectedCore {
        self.root.wave.core().bad_request()
    }

    pub fn err(self, err: UniErr) -> ReflectedCore {
        self.root.wave.core().err(err)
    }
}

#[derive(Clone)]
pub struct TxRouter {
    pub tx: mpsc::Sender<UltraWave>,
}

impl TxRouter {
    pub fn new(tx: mpsc::Sender<UltraWave>) -> Self {
        Self { tx }
    }
}

#[async_trait]
impl Router for TxRouter {
    async fn route(&self, wave: UltraWave) {
        self.tx.send(wave).await;
    }

    fn route_sync(&self, wave: UltraWave) {
        self.tx.try_send(wave);
    }
}

#[async_trait]
impl Router for BroadTxRouter {
    async fn route(&self, wave: UltraWave) {
        self.tx.send(wave);
    }

    fn route_sync(&self, wave: UltraWave) {
        self.tx.send(wave);
    }
}

#[derive(Clone)]
pub struct BroadTxRouter {
    pub tx: broadcast::Sender<UltraWave>,
}

impl BroadTxRouter {
    pub fn new(tx: broadcast::Sender<UltraWave>) -> Self {
        Self { tx }
    }
}

#[derive(Clone)]
pub struct ProtoTransmitterBuilder {
    pub agent: SetStrategy<Agent>,
    pub scope: SetStrategy<Scope>,
    pub handling: SetStrategy<Handling>,
    pub from: SetStrategy<Surface>,
    pub to: SetStrategy<Recipients>,
    pub router: Arc<dyn Router>,
    pub exchanger: Exchanger,
}

impl ProtoTransmitterBuilder {
    pub fn new(router: Arc<dyn Router>, exchanger: Exchanger) -> ProtoTransmitterBuilder {
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            router,
            exchanger,
        }
    }

    pub fn build(self) -> ProtoTransmitter {
        ProtoTransmitter {
            agent: self.agent,
            scope: self.scope,
            handling: self.handling,
            from: self.from,
            to: self.to,
            router: self.router,
            exchanger: self.exchanger,
        }
    }
}

#[derive(Clone)]
pub struct ProtoTransmitter {
    agent: SetStrategy<Agent>,
    scope: SetStrategy<Scope>,
    handling: SetStrategy<Handling>,
    from: SetStrategy<Surface>,
    to: SetStrategy<Recipients>,
    router: Arc<dyn Router>,
    exchanger: Exchanger,
}

impl ProtoTransmitter {
    pub fn new(router: Arc<dyn Router>, exchanger: Exchanger) -> ProtoTransmitter {
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            router,
            exchanger,
        }
    }

    pub fn from_topic(&mut self, topic: Topic) -> Result<(), UniErr> {
        self.from = match self.from.clone() {
            SetStrategy::None => {
                return Err(UniErr::from_500(
                    "cannot set Topic without first setting Surface",
                ));
            }
            SetStrategy::Fill(from) => SetStrategy::Fill(from.with_topic(topic)),
            SetStrategy::Override(from) => SetStrategy::Override(from.with_topic(topic)),
        };
        Ok(())
    }

    pub async fn direct<D, W>(&self, wave: D) -> Result<W, UniErr>
    where
        W: FromReflectedAggregate,
        D: Into<DirectedProto>,
    {
        let mut wave: DirectedProto = wave.into();

        match &self.from {
            SetStrategy::None => {}
            SetStrategy::Fill(from) => wave.fill_from(from.clone()),
            SetStrategy::Override(from) => wave.from(from.clone()),
        }

        match &self.to {
            SetStrategy::None => {}
            SetStrategy::Fill(to) => wave.fill_to(to.clone()),
            SetStrategy::Override(to) => wave.to(to),
        }

        match &self.agent {
            SetStrategy::None => {}
            SetStrategy::Fill(agent) => wave.fill_agent(agent),
            SetStrategy::Override(agent) => wave.agent(agent.clone()),
        }

        match &self.scope {
            SetStrategy::None => {}
            SetStrategy::Fill(scope) => wave.fill_scope(scope),
            SetStrategy::Override(scope) => wave.scope(scope.clone()),
        }

        match &self.handling {
            SetStrategy::None => {}
            SetStrategy::Fill(handling) => wave.fill_handling(handling),
            SetStrategy::Override(handling) => wave.handling(handling.clone()),
        }

        let directed = wave.build()?;

        match directed.bounce_backs() {
            BounceBacks::None => {
                self.router.route(directed.to_ultra()).await;
                FromReflectedAggregate::from_reflected_aggregate(ReflectedAggregate::None)
            }
            _ => {
                let reflected_rx = self.exchanger.exchange(&directed).await;
                self.router.route(directed.to_ultra()).await;
                let reflected_agg = reflected_rx.await?;
                FromReflectedAggregate::from_reflected_aggregate(reflected_agg)
            }
        }
    }

    pub fn route_sync(&self, wave: UltraWave) {
        self.router.route_sync(wave)
    }

    pub async fn route(&self, wave: UltraWave) {
        self.router.route(wave).await
    }

    pub async fn reflect<W>(&self, wave: W) -> Result<(), UniErr>
    where
        W: Into<ReflectedProto>,
    {
        let mut wave: ReflectedProto = wave.into();

        match &self.from {
            SetStrategy::None => {}
            SetStrategy::Fill(from) => wave.fill_from(from),
            SetStrategy::Override(from) => wave.from(from.clone()),
        }

        match &self.agent {
            SetStrategy::None => {}
            SetStrategy::Fill(agent) => wave.fill_agent(agent),
            SetStrategy::Override(agent) => wave.agent(agent.clone()),
        }

        match &self.scope {
            SetStrategy::None => {}
            SetStrategy::Fill(scope) => wave.fill_scope(scope),
            SetStrategy::Override(scope) => wave.scope(scope.clone()),
        }

        match &self.handling {
            SetStrategy::None => {}
            SetStrategy::Fill(handling) => wave.fill_handling(handling),
            SetStrategy::Override(handling) => wave.handling(handling.clone()),
        }

        let wave = wave.build()?;
        let wave = wave.to_ultra();
        self.router.route(wave).await;

        Ok(())
    }
}

#[async_trait]
pub trait Router: Send + Sync {
    async fn route(&self, wave: UltraWave);
    fn route_sync(&self, wave: UltraWave);
}

#[derive(Clone)]
pub enum SetStrategy<T> {
    None,
    Fill(T),
    Override(T),
}

impl<T> SetStrategy<T> {
    pub fn unwrap(self) -> Result<T, UniErr> {
        match self {
            SetStrategy::None => Err("cannot unwrap a SetStrategy::None".into()),
            SetStrategy::Fill(t) => Ok(t),
            SetStrategy::Override(t) => Ok(t),
        }
    }
}

impl SetStrategy<Surface> {
    pub fn with_topic(self, topic: Topic) -> Result<Self, UniErr> {
        match self {
            SetStrategy::None => Err("cannot set topic if Strategy is None".into()),
            SetStrategy::Fill(surface) => Ok(SetStrategy::Fill(surface.with_topic(topic))),
            SetStrategy::Override(surface) => Ok(SetStrategy::Override(surface.with_topic(topic))),
        }
    }
}