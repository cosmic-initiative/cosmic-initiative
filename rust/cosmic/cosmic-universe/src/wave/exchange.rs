pub mod asynch;

use alloc::borrow::Cow;
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::Arc;
use std::time::Duration;

use asynch::{DirectedHandler, Exchanger, InCtx, ProtoTransmitter, ProtoTransmitterBuilder, RootInCtx, Router};
use dashmap::DashMap;
use tokio::sync::{broadcast, mpsc, oneshot};

use crate::config::bind::RouteSelector;
use crate::loc::{Topic, ToPoint, ToSurface};
use crate::log::{PointLogger, RootLogger, SpanLogger};
use crate::settings::Timeouts;
use crate::wave::core::cmd::CmdMethod;
use crate::wave::core::http2::StatusCode;
use crate::wave::core::CoreBounce;
use crate::wave::exchange::asynch::AsyncRouter;
use crate::wave::{
    Bounce, BounceBacks, BounceProto, DirectedProto, DirectedWave, Echo, FromReflectedAggregate,
    Handling, Pong, Recipients, RecipientSelector, ReflectedAggregate, ReflectedProto,
    ReflectedWave, Scope, Session, ToRecipients, UltraWave, Wave, WaveId,
};
use crate::{Agent, Point, ReflectedCore, Substance, Surface, ToSubstance, UniErr, wave};

#[derive(Clone)]
pub struct DirectedHandlerShellDef<D,T>
where
    D: DirectedHandler,
{
    logger: PointLogger,
    handler: D,
    surface: Surface,
    builder: T,
}

impl<D,T> DirectedHandlerShellDef<D,T>
where
    D: DirectedHandler,
{
    pub fn new(
        handler: D,
        builder: T,
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
    pub transmitter: T
}

impl <T> RootInCtxDef<T> {
    pub fn new(
        wave: DirectedWave,
        to: Surface,
        logger: SpanLogger,
        transmitter: T,
    ) -> Self {
        Self {
            wave,
            to,
            logger,
            session: None,
            transmitter
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

pub struct InCtxDef<'a, I, T> where T: Clone {
    root: &'a RootInCtx,
    pub transmitter: Cow<'a, T>,
    pub input: &'a I,
    pub logger: SpanLogger,
}

impl<'a, I,T> Deref for InCtxDef<'a, I, T> where T: Clone{
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.input
    }
}

impl<'a, I, T> InCtxDef<'a, I, T> where T:Clone{
    pub fn new(
        root: &'a RootInCtx,
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

    pub fn err(self, err: UniErr) -> ReflectedCore {
        self.root.wave.core().err(err)
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
pub struct ProtoTransmitterBuilderDef<R,E> {
    pub agent: SetStrategy<Agent>,
    pub scope: SetStrategy<Scope>,
    pub handling: SetStrategy<Handling>,
    pub from: SetStrategy<Surface>,
    pub to: SetStrategy<Recipients>,
    pub router: R,
    pub exchanger: E,
}

impl <R,E> ProtoTransmitterBuilderDef<R,E> {
    pub fn build(self) -> ProtoTransmitterDef<R,E> {
        ProtoTransmitterDef {
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
pub struct ProtoTransmitterDef<R,E> {
    agent: SetStrategy<Agent>,
    scope: SetStrategy<Scope>,
    handling: SetStrategy<Handling>,
    from: SetStrategy<Surface>,
    to: SetStrategy<Recipients>,
    router: R,
    exchanger: E,
}

impl<R,E> ProtoTransmitterDef<R,E> {
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
