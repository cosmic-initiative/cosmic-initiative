pub mod asynch;
pub mod synch;

use alloc::borrow::Cow;
use std::ops::Deref;
use anyhow::anyhow;
use asynch::{
    DirectedHandler, Router,
};
use tokio::sync::broadcast;

use crate::space::config::bind::RouteSelector;
use crate::space::loc::{ToPoint, ToSurface, Topic};
use crate::space::log::{PointLogger, RootLogger, SpanLogger};
use crate::space::wave::core::Method;
use crate::space::wave::{
    Bounce, DirectedProto, DirectedWave, EchoCore, FromReflectedAggregate,
    Handling, PongCore, Recipients, ReflectedProto,
    ReflectedWave, Scope, Session, ToRecipients, Wave, WaveVariantDef,
};
use crate::{Agent, ReflectedCore, SpaceErr, Substance, Surface, ToSubstance};

#[derive(Clone)]
pub struct DirectedHandlerShellDef<D, T> {
    logger: PointLogger,
    handler: D,
    surface: Surface,
    builder: T,
}

impl<D, T> DirectedHandlerShellDef<D, T>
where
    D: Sized,
{
    pub fn new(handler: D, builder: T, surface: Surface, logger: RootLogger) -> Self {
        let logger = logger.point(surface.point.clone());
        Self {
            handler,
            builder,
            surface,
            logger,
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

pub struct RootInCtxDef<T> {
    pub to: Surface,
    pub wave: DirectedWave,
    pub session: Option<Session>,
    pub logger: SpanLogger,
    pub transmitter: T,
}

impl<T> RootInCtxDef<T> {
    pub fn new(wave: DirectedWave, to: Surface, logger: SpanLogger, transmitter: T) -> Self {
        Self {
            wave,
            to,
            logger,
            session: None,
            transmitter,
        }
    }

    pub fn status(self, status: u16, from: Surface) -> Bounce<ReflectedWave> {
        match self.wave {
            DirectedWave::Ping(ping) => Bounce::Reflected(ReflectedWave::Pong(WaveVariantDef::new(
                PongCore::new(
                    ReflectedCore::status(status),
                    ping.from.clone(),
                    self.to.clone().to_recipients(),
                    ping.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Ripple(ripple) => Bounce::Reflected(ReflectedWave::Echo(WaveVariantDef::new(
                EchoCore::new(
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
            DirectedWave::Ping(ping) => Bounce::Reflected(ReflectedWave::Pong(WaveVariantDef::new(
                PongCore::new(
                    ReflectedCore::fail(status, msg),
                    ping.from.clone(),
                    self.to.clone().to_recipients(),
                    ping.id.clone(),
                ),
                from,
            ))),
            DirectedWave::Ripple(ripple) => Bounce::Reflected(ReflectedWave::Echo(WaveVariantDef::new(
                EchoCore::new(
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

pub struct InCtxDef<'a, I, T>
where
    T: Clone,
{
    root: &'a RootInCtxDef<T>,
    pub transmitter: Cow<'a, T>,
    pub input: &'a I,
    pub logger: SpanLogger,
}

impl<'a, I, T> Deref for InCtxDef<'a, I, T>
where
    T: Clone,
{
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl<'a, I, T> InCtxDef<'a, I, T>
where
    T: Clone,
{
    pub fn new(
        root: &'a RootInCtxDef<T>,
        input: &'a I,
        tx: Cow<'a, T>,
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

    pub fn push(self) -> InCtxDef<'a, I, T> {
        InCtxDef {
            root: self.root,
            input: self.input,
            logger: self.logger.span(),
            transmitter: self.transmitter.clone(),
        }
    }

    pub fn push_input_ref<I2>(self, input: &'a I2) -> InCtxDef<'a, I2, T> {
        InCtxDef {
            root: self.root,
            input,
            logger: self.logger.clone(),
            transmitter: self.transmitter.clone(),
        }
    }

    pub fn wave(&self) -> &DirectedWave {
        &self.root.wave
    }

    /*
    pub async fn ping(&self, req: DirectedProto) -> Result<Wave<Pong>, UniErr> {
        self.transmitter.direct(req).await
    }

     */

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

    pub fn err(self, err: SpaceErr) -> ReflectedCore {
        self.root.wave.core().err(err)
    }
}

#[derive(Clone)]
pub struct BroadTxRouter {
    pub tx: broadcast::Sender<Wave>,
}

impl BroadTxRouter {
    pub fn new(tx: broadcast::Sender<Wave>) -> Self {
        Self { tx }
    }
}

#[derive(Clone)]
pub struct ProtoTransmitterBuilderDef<R, E> {
    pub agent: SetStrategy<Agent>,
    pub scope: SetStrategy<Scope>,
    pub handling: SetStrategy<Handling>,
    pub method: SetStrategy<Method>,
    pub via: SetStrategy<Surface>,
    pub from: SetStrategy<Surface>,
    pub to: SetStrategy<Recipients>,
    pub router: R,
    pub exchanger: E,
}

impl<R, E> ProtoTransmitterBuilderDef<R, E> {
    pub fn build(self) -> ProtoTransmitterDef<R, E> {
        ProtoTransmitterDef {
            agent: self.agent,
            scope: self.scope,
            handling: self.handling,
            method: self.method,
            from: self.from,
            to: self.to,
            via: self.via,
            router: self.router,
            exchanger: self.exchanger,
        }
    }
}

#[derive(Clone)]
pub struct ProtoTransmitterDef<R, E> {
    agent: SetStrategy<Agent>,
    scope: SetStrategy<Scope>,
    handling: SetStrategy<Handling>,
    method: SetStrategy<Method>,
    from: SetStrategy<Surface>,
    to: SetStrategy<Recipients>,
    via: SetStrategy<Surface>,
    router: R,
    exchanger: E,
}

impl<R, E> ProtoTransmitterDef<R, E> {
    pub fn from_topic(&mut self, topic: Topic) -> Result<(), SpaceErr> {
        self.from = match self.from.clone() {
            SetStrategy::None => {
                return Err(anyhow!(
                    "cannot set Topic without first setting Surface",
                ).chain());
            }
            SetStrategy::Fill(from) => SetStrategy::Fill(from.with_topic(topic)),
            SetStrategy::Override(from) => SetStrategy::Override(from.with_topic(topic)),
        };
        Ok(())
    }

    fn prep_direct(&self, wave: &mut DirectedProto) {
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

        match &self.via {
            SetStrategy::None => {}
            SetStrategy::Fill(via) => wave.fill_via(via.clone()),
            SetStrategy::Override(via) => wave.via(via),
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

        match &self.method {
            SetStrategy::None => {}
            SetStrategy::Fill(method) => wave.fill_method(method),
            SetStrategy::Override(handling) => wave.method(handling.clone()),
        }
    }

    fn prep_reflect(&self, wave: &mut ReflectedProto) {
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
    }
}

#[derive(Clone, strum_macros::Display)]
pub enum SetStrategy<T> {
    /// The ProtoTransmitter will NOT set a value
    None,
    /// The ProtoTransmitter will set the DirectedProto value unless
    /// the value was already explicitly set
    Fill(T),
    /// The ProtoTransmitter will override the DirectedProto value
    /// even if it has already been explicitly set
    Override(T),
}

impl<T> SetStrategy<T> {
    pub fn unwrap(self) -> Result<T, SpaceErr> {
        match self {
            SetStrategy::None => Err("cannot unwrap a SetStrategy::None".into()),
            SetStrategy::Fill(t) => Ok(t),
            SetStrategy::Override(t) => Ok(t),
        }
    }
}

impl SetStrategy<Surface> {
    pub fn with_topic(self, topic: Topic) -> Result<Self, SpaceErr> {
        match self {
            SetStrategy::None => Err("cannot set topic if Strategy is None".into()),
            SetStrategy::Fill(surface) => Ok(SetStrategy::Fill(surface.with_topic(topic))),
            SetStrategy::Override(surface) => Ok(SetStrategy::Override(surface.with_topic(topic))),
        }
    }
}
