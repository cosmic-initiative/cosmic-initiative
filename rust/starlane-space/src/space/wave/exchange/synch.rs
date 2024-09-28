use crate::space::loc::ToPoint;
use crate::space::wave::core::cmd::CmdMethod;
use crate::space::wave::core::CoreBounce;
use crate::space::wave::exchange::{
    DirectedHandlerShellDef, InCtxDef, ProtoTransmitterBuilderDef, ProtoTransmitterDef,
    RootInCtxDef, SetStrategy,
};
use crate::space::wave::{
    Bounce, BounceBacks, DirectedKind, DirectedProto, DirectedWave, EchoCore, FromReflectedAggregate,
    Handling, PongCore, RecipientSelector, ReflectedAggregate, ReflectedProto, ReflectedWave,
    Scope, Wave, WaveVariantDef,
};
use crate::{Agent, ReflectedCore, SpaceErr, Substance, Surface, ToSubstance};
use alloc::borrow::Cow;
use std::sync::Arc;
use anyhow::anyhow;
use crate::space::thiserr::err;

pub trait ExchangeRouter: Send + Sync {
    fn route(&self, wave: Wave);
    fn exchange(&self, direct: DirectedWave) -> anyhow::Result<ReflectedAggregate>;
}

#[derive(Clone)]
pub struct SyncRouter {
    pub router: Arc<dyn ExchangeRouter>,
}

impl SyncRouter {
    pub fn new(router: Arc<dyn ExchangeRouter>) -> Self {
        Self { router }
    }
}

impl ExchangeRouter for SyncRouter {
    fn route(&self, wave: Wave) {
        self.router.route(wave)
    }

    fn exchange(&self, direct: DirectedWave) -> anyhow::Result<ReflectedAggregate> {
        self.router.exchange(direct)
    }
}

pub type ProtoTransmitter = ProtoTransmitterDef<SyncRouter, ()>;

impl ProtoTransmitter {
    pub fn new(router: Arc<dyn ExchangeRouter>) -> ProtoTransmitter {
        let router = SyncRouter::new(router);
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            via: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            method: SetStrategy::None,
            router,
            exchanger: (),
        }
    }

    pub fn direct<D, W>(&self, wave: D) -> anyhow::Result<W>
    where
        W: FromReflectedAggregate,
        D: Into<DirectedProto>,
    {
        let mut wave: DirectedProto = wave.into();

        self.prep_direct(&mut wave);

        let directed = wave.build()?;

        println!("DIRECTE!");
        match directed.bounce_backs() {
            BounceBacks::None => {
                self.router.route(directed.to_wave());
                FromReflectedAggregate::from_reflected_aggregate(ReflectedAggregate::None)
            }
            _ => FromReflectedAggregate::from_reflected_aggregate(self.router.exchange(directed)?),
        }
    }

    pub fn ping<D>(&self, ping: D) -> anyhow::Result<WaveVariantDef<PongCore>>
    where
        D: Into<DirectedProto>,
    {
        println!("PING!");
        let mut ping: DirectedProto = ping.into();
        ping.bounce_backs = Some(BounceBacks::Single);
        if let Some(DirectedKind::Ping) = ping.kind {
            self.direct(ping)
        } else {
            Err(err("expected DirectedKind::Ping"))?
        }
    }

    pub fn ripple<D>(&self, ripple: D) -> anyhow::Result<Vec<WaveVariantDef<EchoCore>>>
    where
        D: Into<DirectedProto>,
    {
        let mut ripple: DirectedProto = ripple.into();
        if let Some(DirectedKind::Ripple) = ripple.kind {
            self.direct(ripple)
        } else {
            Err(anyhow!("expected DirectedKind::Ping"))
        }
    }

    pub fn signal<D>(&self, signal: D) -> anyhow::Result<()>
    where
        D: Into<DirectedProto>,
    {
        let mut signal: DirectedProto = signal.into();
        if let Some(DirectedKind::Signal) = signal.kind {
            self.direct(signal)
        } else {
            Err(anyhow!("expected DirectedKind::Ping"))
        }
    }

    pub fn bounce_from(&self, to: &Surface, from: &Surface) -> bool {
        let mut directed = DirectedProto::ping();
        directed.from(from.clone());
        directed.to(to.clone());
        directed.method(CmdMethod::Bounce);
        match self.ping(directed) {
            Ok(pong) => pong.is_ok(),
            Err(_) => false,
        }
    }

    pub fn bounce(&self, to: &Surface) -> bool {
        let mut directed = DirectedProto::ping();
        directed.to(to.clone());
        directed.method(CmdMethod::Bounce);
        match self.ping(directed) {
            Ok(pong) => pong.is_ok(),
            Err(_) => false,
        }
    }

    pub fn route(&self, wave: Wave) {
        self.router.route(wave)
    }

    pub fn reflect<W>(&self, wave: W) -> anyhow::Result<()>
    where
        W: Into<ReflectedProto>,
    {
        let mut wave: ReflectedProto = wave.into();

        self.prep_reflect(&mut wave);

        let wave = wave.build()?;
        let wave = wave.to_wave();
        self.router.route(wave);

        Ok(())
    }
}

pub type ProtoTransmitterBuilder = ProtoTransmitterBuilderDef<SyncRouter, ()>;

impl ProtoTransmitterBuilder {
    pub fn new(router: Arc<dyn ExchangeRouter>) -> ProtoTransmitterBuilder {
        let router = SyncRouter::new(router);
        Self {
            from: SetStrategy::None,
            to: SetStrategy::None,
            via: SetStrategy::None,
            agent: SetStrategy::Fill(Agent::Anonymous),
            scope: SetStrategy::Fill(Scope::None),
            handling: SetStrategy::Fill(Handling::default()),
            method: SetStrategy::None,
            router,
            exchanger: (),
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

pub trait DirectedHandlerSelector {
    fn select<'a>(&self, select: &'a RecipientSelector<'a>) -> anyhow::Result<&dyn DirectedHandler>;
}

pub trait DirectedHandler: Send + Sync {
    fn handle(&self, ctx: RootInCtx) -> CoreBounce;

    fn bounce(&self, ctx: RootInCtx) -> CoreBounce {
        CoreBounce::Reflected(ReflectedCore::ok())
    }
}

pub struct DirectedHandlerProxy {
    proxy: Box<dyn DirectedHandler>,
}

impl DirectedHandlerProxy {
    pub fn new<D>(handler: D) -> Self
    where
        D: DirectedHandler + 'static + Sized,
    {
        Self {
            proxy: Box::new(handler),
        }
    }

    pub fn boxed<D>(handler: Box<D>) -> Self
    where
        D: DirectedHandler + 'static + Sized,
    {
        Self { proxy: handler }
    }
}

impl DirectedHandler for DirectedHandlerProxy {
    fn handle(&self, ctx: RootInCtx) -> CoreBounce {
        self.proxy.handle(ctx)
    }
}

pub type DirectedHandlerShell =
    DirectedHandlerShellDef<Box<dyn DirectedHandler>, ProtoTransmitterBuilder>;

impl DirectedHandlerShell {
    pub fn handle(&self, wave: DirectedWave) -> Bounce<ReflectedWave> {
        let logger = self
            .logger
            .point(self.surface.clone().to_point())
            .spanner(&wave);
        let mut transmitter = self.builder.clone().build();
        let reflection = wave.reflection();
        let ctx = RootInCtx::new(wave, self.surface.clone(), logger, transmitter.clone());
        match self.handler.handle(ctx) {
            CoreBounce::Absorbed => Bounce::Absorbed,
            CoreBounce::Reflected(reflected) => {
                let wave = reflection.unwrap().make(reflected, self.surface.clone());
                Bounce::Reflected(wave)
            }
        }
    }
}

impl RootInCtx {
    pub fn push<'a, I>(&self) -> anyhow::Result<InCtx<I>>
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
