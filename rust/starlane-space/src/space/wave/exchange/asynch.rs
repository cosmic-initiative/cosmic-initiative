use crate::space::err;
use crate::space::err::err;
use crate::space::loc::{ToPoint, ToSurface};
use crate::space::log::{PointLogger, RootLogger, Trackable, Tracker};
use crate::space::particle::traversal::Traversal;
use crate::space::point::Point;
use crate::space::settings::Timeouts;
use crate::space::wave::core::cmd::CmdMethod;
use crate::space::wave::core::http2::StatusCode;
use crate::space::wave::core::CoreBounce;
use crate::space::wave::exchange::{
    BroadTxRouter, DirectedHandlerShellDef, InCtxDef, ProtoTransmitterBuilderDef,
    ProtoTransmitterDef, RootInCtxDef, SetStrategy,
};
use crate::space::wave::{
    BounceBacks, BounceProto, DirectedKind, DirectedProto, DirectedWave, EchoCore,
    FromReflectedAggregate, Handling, PongCore, RecipientSelector, ReflectedAggregate, ReflectedProto,
    ReflectedWave, Scope, Wave, WaveId, WaveVariantDef,
};
use crate::{Agent, ReflectedCore, Substance, Surface, ToSubstance};
use alloc::borrow::Cow;
use dashmap::{DashMap, DashSet};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot};

#[async_trait]
impl Router for TxRouter {
    async fn route(&self, wave: Wave) {
        self.tx.send(wave).await;
    }
}

#[async_trait]
impl Router for BroadTxRouter {
    async fn route(&self, wave: Wave) {
        self.tx.send(wave);
    }
}

#[async_trait]
pub trait Router: Send + Sync {
    async fn route(&self, wave: Wave);
}

#[async_trait]
pub trait TraversalRouter: Send + Sync {
    async fn traverse(&self, traversal: Traversal<Wave>) -> err::Result<()>;
}

#[derive(Clone)]
pub struct AsyncRouter {
    pub router: Arc<dyn Router>,
}

impl AsyncRouter {
    pub fn new(router: Arc<dyn Router>) -> Self {
        Self { router }
    }
}

#[async_trait]
impl Router for AsyncRouter {
    async fn route(&self, wave: Wave) {
        self.router.route(wave).await
    }
}

pub type ProtoTransmitter = ProtoTransmitterDef<AsyncRouter, Exchanger>;

impl ProtoTransmitter {
    pub fn new(router: Arc<dyn Router>, exchanger: Exchanger) -> ProtoTransmitter {
        let router = AsyncRouter::new(router);
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            method: SetStrategy::None,
            via: SetStrategy::None,
            router,
            exchanger,
        }
    }

    pub async fn direct<D, W>(&self, wave: D) -> err::Result<W>
    where
        W: FromReflectedAggregate,
        D: Into<DirectedProto>,
    {
        let mut wave: DirectedProto = wave.into();

        self.prep_direct(&mut wave);

        let directed = wave.build()?;

        match directed.bounce_backs() {
            BounceBacks::None => {
                self.router.route(directed.to_wave()).await;
                FromReflectedAggregate::from_reflected_aggregate(ReflectedAggregate::None)
            }
            _ => {
                let reflected_rx = self.exchanger.exchange(&directed).await;
                self.router.route(directed.to_wave()).await;
                let reflected_agg = reflected_rx.await?;
                FromReflectedAggregate::from_reflected_aggregate(reflected_agg)
            }
        }
    }

    pub async fn ping<D>(&self, ping: D) -> err::Result<WaveVariantDef<PongCore>>
    where
        D: Into<DirectedProto>,
    {
        let mut ping: DirectedProto = ping.into();
        if let Some(DirectedKind::Ping) = ping.kind {
            self.direct(ping).await
        } else {
            Err(err!("expected DirectedKind::Ping"))
        }
    }

    pub async fn ripple<D>(&self, ripple: D) -> err::Result<Vec<WaveVariantDef<EchoCore>>>
    where
        D: Into<DirectedProto>,
    {
        let mut ripple: DirectedProto = ripple.into();
        if let Some(DirectedKind::Ripple) = ripple.kind {
            self.direct(ripple).await
        } else {
            Err(err!("expected DirectedKind::Ping"))
        }
    }

    pub async fn signal<D>(&self, signal: D) -> err::Result<()>
    where
        D: Into<DirectedProto>,
    {
        let mut signal: DirectedProto = signal.into();
        if let Some(DirectedKind::Signal) = signal.kind {
            self.direct(signal).await
        } else {
            Err(err!("expected DirectedKind::Ping"))
        }
    }

    pub async fn bounce_from(&self, to: &Surface, from: &Surface) -> bool {
        let mut directed = DirectedProto::ping();
        directed.from(from.clone());
        directed.to(to.clone());
        directed.method(CmdMethod::Bounce);
        match self.direct(directed).await {
            Ok(pong) => {
                let pong: WaveVariantDef<PongCore> = pong;
                pong.is_ok()
            }
            Err(_) => false,
        }
    }

    pub async fn bounce(&self, to: &Surface) -> bool {
        let mut direct = DirectedProto::ping();
        direct.to(to.clone());
        direct.method(CmdMethod::Bounce);
        match self.direct(direct).await {
            Ok(pong) => {
                let pong: WaveVariantDef<PongCore> = pong;
                pong.is_ok()
            }
            Err(_) => false,
        }
    }

    pub async fn route(&self, wave: Wave) {
        self.router.route(wave).await
    }

    pub async fn reflect<W>(&self, wave: W) -> err::Result<()>
    where
        W: Into<ReflectedProto>,
    {
        let mut wave: ReflectedProto = wave.into();

        self.prep_reflect(&mut wave);

        let wave = wave.build()?;
        let wave = wave.to_wave();
        self.router.route(wave).await;

        Ok(())
    }
}

pub type ProtoTransmitterBuilder = ProtoTransmitterBuilderDef<AsyncRouter, Exchanger>;

impl ProtoTransmitterBuilder {
    pub fn new(router: Arc<dyn Router>, exchanger: Exchanger) -> ProtoTransmitterBuilder {
        let router = AsyncRouter::new(router);
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            via: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            method: SetStrategy::None,
            router,
            exchanger,
        }
    }
}

pub type TraversalTransmitter = ProtoTransmitterDef<Arc<dyn TraversalRouter>, Exchanger>;

impl TraversalTransmitter {
    pub fn new(router: Arc<dyn TraversalRouter>, exchanger: Exchanger) -> Self {
        Self {
            agent: SetStrategy::None,
            scope: SetStrategy::None,
            handling: SetStrategy::None,
            method: SetStrategy::None,
            from: SetStrategy::None,
            to: SetStrategy::None,
            via: SetStrategy::None,
            router,
            exchanger,
        }
    }

    pub async fn direct<W>(&self, traversal: Traversal<DirectedWave>) -> err::Result<W>
    where
        W: FromReflectedAggregate,
    {
        match traversal.bounce_backs() {
            BounceBacks::None => {
                self.router.traverse(traversal.wrap()).await;
                FromReflectedAggregate::from_reflected_aggregate(ReflectedAggregate::None)
            }
            _ => {
                let reflected_rx = self.exchanger.exchange(&traversal.payload).await;
                self.router.traverse(traversal.wrap()).await;
                let reflected_agg = reflected_rx.await?;
                FromReflectedAggregate::from_reflected_aggregate(reflected_agg)
            }
        }
    }
}

pub type RootInCtx = RootInCtxDef<ProtoTransmitter>;

pub type InCtx<'a, I> = InCtxDef<'a, I, ProtoTransmitter>;

impl<'a, I> InCtx<'a, I> {
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
}

#[async_trait]
pub trait DirectedHandlerSelector {
    fn select<'a>(&self, select: &'a RecipientSelector<'a>) -> err::Result<&dyn DirectedHandler>;
}

#[async_trait]
pub trait DirectedHandler: Send + Sync {
    async fn handle(&self, ctx: RootInCtx) -> CoreBounce;

    async fn bounce(&self, ctx: RootInCtx) -> CoreBounce {
        CoreBounce::Reflected(ReflectedCore::ok())
    }
}

#[derive(Clone)]
pub struct TxRouter {
    pub tx: mpsc::Sender<Wave>,
}

impl TxRouter {
    pub fn new(tx: mpsc::Sender<Wave>) -> Self {
        Self { tx }
    }
}

#[derive(Clone)]
pub struct Exchanger {
    pub surface: Surface,
    pub multis: Arc<DashMap<WaveId, mpsc::Sender<ReflectedWave>>>,
    pub singles: Arc<DashMap<WaveId, oneshot::Sender<ReflectedAggregate>>>,
    pub timeouts: Timeouts,
    pub logger: PointLogger,
    #[cfg(test)]
    pub claimed: Arc<DashSet<String>>,
}

impl Exchanger {
    pub fn new(surface: Surface, timeouts: Timeouts, logger: PointLogger) -> Self {
        let logger = logger.point(surface.point.clone());
        Self {
            surface,
            singles: Arc::new(DashMap::new()),
            multis: Arc::new(DashMap::new()),
            timeouts,
            logger,
            #[cfg(test)]
            claimed: Arc::new(DashSet::new()),
        }
    }

    pub fn with_surface(&self, surface: Surface) -> Self {
        let logger = self.logger.point(surface.point.clone());
        Self {
            surface,
            singles: self.singles.clone(),
            multis: self.multis.clone(),
            timeouts: self.timeouts.clone(),
            logger,
            #[cfg(test)]
            claimed: self.claimed.clone(),
        }
    }

    pub async fn reflected(&self, reflect: ReflectedWave) -> err::Result<()> {
        self.logger
            .track(&reflect, || Tracker::new("exchange", "Reflected"));

        if let Some(multi) = self.multis.get(reflect.reflection_of()) {
            multi.value().send(reflect).await;
        } else if let Some((_, tx)) = self.singles.remove(reflect.reflection_of()) {
            #[cfg(test)]
            self.claimed.insert(reflect.reflection_of().to_string());
            tx.send(ReflectedAggregate::Single(reflect));
        } else {
            let reflect = reflect.to_wave();
            let kind = match &reflect {
                Wave::Ping(_) => "Ping",
                Wave::Pong(_) => "Pong",
                Wave::Ripple(_) => "Ripple",
                Wave::Echo(_) => "Echo",
                Wave::Signal(_) => "Signal",
            };
            let reflect = reflect.to_reflected()?;

            #[cfg(test)]
            if self
                .claimed
                .contains(reflect.reflection_of().to_string().as_str())
            {
                return Err(err!(format!(
                    "Reflection already claimed for {} from: {} to: {} KIND: {} STATUS: {}",
                    reflect.reflection_of().to_short_string(),
                    reflect.from().to_string(),
                    reflect.to().to_string(),
                    kind,
                    reflect.core().status.to_string()
                )));
            }
            return Err(err!(format!(
                "Not expecting reflected message for {} from: {} to: {} KIND: {} STATUS: {}",
                reflect.reflection_of().to_short_string(),
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
        Self::new(
            Point::root().to_surface(),
            Default::default(),
            RootLogger::default().point(Point::root()),
        )
    }
}

pub type DirectedHandlerShell<D> = DirectedHandlerShellDef<D, ProtoTransmitterBuilder>;

impl<D> DirectedHandlerShell<D>
where
    D: DirectedHandler,
{
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
                let wave = wave.to_wave();
                let transmitter = self.builder.clone().build();
                transmitter.route(wave).await;
            }
        }
    }
}

impl RootInCtx {
    pub fn push<'a, I>(&self) -> err::Result<InCtx<I>>
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
