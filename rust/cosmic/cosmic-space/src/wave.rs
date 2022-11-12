use ::core::borrow::Borrow;
use alloc::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::convert::Infallible;
use std::env::var;
use std::marker::PhantomData;
use std::ops;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use tokio::sync::{broadcast, mpsc, oneshot, RwLock};
use tokio::time::Instant;

use cosmic_macros_primitive::Autobox;
use cosmic_nom::{Res, SpanExtra};
use exchange::asynch::ProtoTransmitter;

use crate::command::Command;
use crate::command::RawCommand;
use crate::config::bind::RouteSelector;
use crate::err::{CoreReflector, SpaceErr, StatusErr};
use crate::hyper::AssignmentKind;
use crate::hyper::InterchangeKind::DefaultControl;
use crate::point::StarKey;
use crate::point::{
    Layer, Point, PointSeg, RouteSeg, Surface, SurfaceSelector, ToPoint, ToSurface, Topic, Uuid,
};
use crate::log::{
    LogSpan, LogSpanEvent, PointLogger, RootLogger, SpanLogger, Spannable, Trackable, TrailSpanId,
};
use crate::parse::model::Subst;
use crate::parse::sub;
use crate::particle::Watch;
use crate::particle::{Details, Status};
use crate::security::{Permissions, Privilege, Privileges};
use crate::settings::Timeouts;
use crate::substance::Bin;
use crate::substance::{
    Call, CallKind, CmdCall, ExtCall, FormErrs, HttpCall, HypCall, MultipartFormBuilder, Substance,
    SubstanceKind, ToRequestCore, ToSubstance, Token,
};
use crate::util::{uuid, ValueMatcher, ValuePattern};
use crate::wave::core::http2::StatusCode;
use crate::{ANONYMOUS, HYPERUSER};
use url::Url;
use crate::selector::PointSelector;

use self::core::cmd::CmdMethod;
use self::core::ext::ExtMethod;
use self::core::http2::HttpMethod;
use self::core::hyp::HypMethod;
use self::core::{CoreBounce, DirectedCore, Method, ReflectedCore};

pub mod core;
pub mod exchange;

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    Hash,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum WaveKind {
    Ping,   // Request
    Pong,   // Response
    Ripple, // Broadcast
    Echo,   // Broadcast Response
    Signal, // Notification
            /*
            Photon, // Ack
                   Reverb,  // Ack
                  */
}

impl WaveKind {
    pub fn reflected_kind(&self) -> Result<ReflectedKind, SpaceErr> {
        match self {
            WaveKind::Pong => Ok(ReflectedKind::Pong),
            WaveKind::Echo => Ok(ReflectedKind::Echo),
            _ => Err(SpaceErr::bad_request("expected a reflected WaveKind")),
        }
    }
}

pub type UltraWave = UltraWaveDef<Recipients>;
pub type SingularUltraWave = UltraWaveDef<Surface>;

impl SingularUltraWave {
    pub fn to_ultra(self) -> Result<UltraWave, SpaceErr> {
        match self {
            SingularUltraWave::Ping(ping) => Ok(UltraWave::Ping(ping)),
            SingularUltraWave::Pong(pong) => Ok(UltraWave::Pong(pong)),
            SingularUltraWave::Echo(echo) => Ok(UltraWave::Echo(echo)),
            SingularUltraWave::Signal(signal) => Ok(UltraWave::Signal(signal)),
            SingularUltraWave::Ripple(ripple) => {
                let ripple = ripple.to_multiple();
                Ok(UltraWave::Ripple(ripple))
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum UltraWaveDef<T>
where
    T: ToRecipients + Clone,
{
    Ping(Wave<Ping>),
    Pong(Wave<Pong>),
    Ripple(Wave<RippleDef<T>>),
    Echo(Wave<Echo>),
    Signal(Wave<Signal>),
}

impl<W> Spannable for UltraWaveDef<W>
where
    W: ToRecipients + Clone,
{
    fn span_id(&self) -> String {
        self.id().to_string()
    }

    fn span_type(&self) -> &'static str {
        "Wave"
    }
}

impl Trackable for UltraWave {
    fn track_id(&self) -> String {
        self.id().to_short_string()
    }

    fn track_method(&self) -> String {
        match self {
            UltraWave::Ping(ping) => ping.core.method.to_deep_string(),
            UltraWave::Pong(pong) => pong.core.status.to_string(),
            UltraWave::Ripple(ripple) => ripple.core.method.to_deep_string(),
            UltraWave::Echo(echo) => echo.core.status.to_string(),
            UltraWave::Signal(signal) => signal.core.method.to_deep_string(),
        }
    }

    fn track_payload(&self) -> String {
        match self {
            UltraWave::Ping(ping) => ping.core.body.kind().to_string(),
            UltraWave::Pong(pong) => pong.core.body.kind().to_string(),
            UltraWave::Ripple(ripple) => ripple.core.body.kind().to_string(),
            UltraWave::Echo(echo) => echo.core.body.kind().to_string(),
            UltraWave::Signal(signal) => signal.core.body.kind().to_string(),
        }
    }

    fn track_from(&self) -> String {
        self.from().to_string()
    }

    fn track_to(&self) -> String {
        self.to().to_string()
    }

    fn track(&self) -> bool {
        match self {
            UltraWave::Ping(ping) => ping.track,
            UltraWave::Pong(pong) => pong.track,
            UltraWave::Ripple(ripple) => ripple.track,
            UltraWave::Echo(echo) => echo.track,
            UltraWave::Signal(signal) => signal.track,
        }
    }
    fn track_payload_fmt(&self) -> String {
        match self {
            UltraWave::Signal(signal) => signal.track_payload_fmt(),
            UltraWave::Ping(_) => self.track_payload(),
            UltraWave::Pong(_) => self.track_payload(),
            UltraWave::Ripple(_) => self.track_payload(),
            UltraWave::Echo(_) => self.track_payload(),
        }
    }
}

impl<T> UltraWaveDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn via_desc(&self) -> String {
        let via = match self {
            UltraWaveDef::Ping(w) => w.via.as_ref(),
            UltraWaveDef::Pong(w) => w.via.as_ref(),
            UltraWaveDef::Ripple(w) => w.via.as_ref(),
            UltraWaveDef::Echo(w) => w.via.as_ref(),
            UltraWaveDef::Signal(w) => w.via.as_ref(),
        };

        match via {
            None => "None".to_string(),
            Some(via) => via.to_string(),
        }
    }

    pub fn has_visited(&self, star: &Point) -> bool {
        match self {
            UltraWaveDef::Ripple(ripple) => ripple.history.contains(star),
            _ => false,
        }
    }

    pub fn add_history(&mut self, point: &Point) {
        match self {
            UltraWaveDef::Ping(_) => {}
            UltraWaveDef::Pong(_) => {}
            UltraWaveDef::Ripple(ripple) => {
                ripple.history.insert(point.clone());
            }
            UltraWaveDef::Echo(_) => {}
            UltraWaveDef::Signal(_) => {}
        }
    }

    pub fn history(&self) -> HashSet<Point> {
        match self {
            UltraWaveDef::Ping(_) => HashSet::new(),
            UltraWaveDef::Pong(_) => HashSet::new(),
            UltraWaveDef::Ripple(ripple) => ripple.history.clone(),
            UltraWaveDef::Echo(_) => HashSet::new(),
            UltraWaveDef::Signal(_) => HashSet::new(),
        }
    }

    pub fn id(&self) -> WaveId {
        match self {
            UltraWaveDef::Ping(w) => w.id.clone(),
            UltraWaveDef::Pong(w) => w.id.clone(),
            UltraWaveDef::Ripple(w) => w.id.clone(),
            UltraWaveDef::Echo(w) => w.id.clone(),
            UltraWaveDef::Signal(w) => w.id.clone(),
        }
    }

    pub fn body(&self) -> &Substance {
        match self {
            UltraWaveDef::Ping(ping) => &ping.body,
            UltraWaveDef::Pong(pong) => &pong.core.body,
            UltraWaveDef::Ripple(ripple) => &ripple.body,
            UltraWaveDef::Echo(echo) => &echo.core.body,
            UltraWaveDef::Signal(signal) => &signal.core.body,
        }
    }

    pub fn payload(&self) -> Option<&UltraWave> {
        if let Substance::UltraWave(wave) = self.body() {
            Some(wave)
        } else {
            None
        }
    }
}

impl UltraWave {
    pub fn can_shard(&self) -> bool {
        match self {
            UltraWave::Ripple(_) => true,
            _ => false,
        }
    }

    pub fn to_singular(self) -> Result<SingularUltraWave, SpaceErr> {
        match self {
            UltraWave::Ping(ping) => Ok(SingularUltraWave::Ping(ping)),
            UltraWave::Pong(pong) => Ok(SingularUltraWave::Pong(pong)),
            UltraWave::Echo(echo) => Ok(SingularUltraWave::Echo(echo)),
            UltraWave::Signal(signal) => Ok(SingularUltraWave::Signal(signal)),
            UltraWave::Ripple(_) => Err(SpaceErr::server_error(
                "cannot change Ripple into a singular",
            )),
        }
    }

    pub fn wrap_in_transport(self, from: Surface, to: Surface) -> DirectedProto {
        let mut signal = DirectedProto::signal();
        signal.fill(&self);
        signal.from(from);
        signal.agent(self.agent().clone());
        signal.handling(self.handling().clone());
        signal.method(HypMethod::Transport);
        //        signal.track = self.track();
        signal.body(Substance::UltraWave(Box::new(self)));
        signal.to(to);
        signal
    }

    pub fn unwrap_from_hop(self) -> Result<Wave<Signal>, SpaceErr> {
        let signal = self.to_signal()?;
        signal.unwrap_from_hop()
    }

    pub fn unwrap_from_transport(self) -> Result<UltraWave, SpaceErr> {
        let signal = self.to_signal()?;
        signal.unwrap_from_transport()
    }

    pub fn to_substance(self) -> Substance {
        Substance::UltraWave(Box::new(self))
    }

    pub fn to_directed(self) -> Result<DirectedWave, SpaceErr> {
        match self {
            UltraWave::Ping(ping) => Ok(ping.to_directed()),
            UltraWave::Ripple(ripple) => Ok(ripple.to_directed()),
            UltraWave::Signal(signal) => Ok(signal.to_directed()),
            _ => Err(SpaceErr::bad_request("expected a DirectedWave")),
        }
    }

    pub fn to_reflected(self) -> Result<ReflectedWave, SpaceErr> {
        match self {
            UltraWave::Pong(pong) => Ok(pong.to_reflected()),
            UltraWave::Echo(echo) => Ok(echo.to_reflected()),
            _ => Err(SpaceErr::bad_request(format!(
                "expected: ReflectedWave; encountered: {}",
                self.desc()
            ))),
        }
    }

    pub fn kind(&self) -> WaveKind {
        match self {
            UltraWave::Ping(_) => WaveKind::Ping,
            UltraWave::Pong(_) => WaveKind::Pong,
            UltraWave::Ripple(_) => WaveKind::Ripple,
            UltraWave::Echo(_) => WaveKind::Echo,
            UltraWave::Signal(_) => WaveKind::Signal,
        }
    }

    /// return a description of this wave for debugging purposes
    pub fn desc(&self) -> String {
        if self.is_directed() {
            let directed = self.clone().to_directed().unwrap();
            format!(
                "{}<{}>[{}]",
                self.kind().to_string(),
                directed.core().method.to_string(),
                directed.core().body.kind().to_string()
            )
        } else {
            let reflected = self.clone().to_reflected().unwrap();
            format!(
                "{}<{}>[{}]",
                self.kind().to_string(),
                reflected.core().status.to_string(),
                reflected.core().body.kind().to_string()
            )
        }
    }

    pub fn hops(&self) -> u16 {
        match self {
            UltraWave::Ping(w) => w.hops,
            UltraWave::Pong(w) => w.hops,
            UltraWave::Ripple(w) => w.hops,
            UltraWave::Echo(w) => w.hops,
            UltraWave::Signal(w) => w.hops,
        }
    }

    pub fn max_hops(&self) -> u16 {
        if let Some(wave) = self.transported() {
            let child = wave.max_hops();
            if child > self.hops() {
                return child;
            }
        }
        self.hops()
    }

    pub fn inc_hops(&mut self) {
        match self {
            UltraWave::Ping(w) => w.hops += 1,
            UltraWave::Pong(w) => w.hops += 1,
            UltraWave::Ripple(w) => w.hops += 1,
            UltraWave::Echo(w) => w.hops += 1,
            UltraWave::Signal(w) => w.hops += 1,
        };

        if let Some(wave) = self.transported_mut() {
            wave.inc_hops();
        }
    }

    pub fn add_to_history(&mut self, star: Point) {
        match self {
            UltraWave::Ripple(ripple) => {
                ripple.history.insert(star);
            }
            _ => {}
        }
    }

    pub fn to_signal(self) -> Result<Wave<Signal>, SpaceErr> {
        match self {
            UltraWave::Signal(signal) => Ok(signal),
            _ => Err(SpaceErr::bad_request(format!(
                "expecting: Wave<Signal> encountered: Wave<{}>",
                self.kind().to_string()
            ))),
        }
    }

    pub fn method(&self) -> Option<&Method> {
        match self {
            UltraWave::Ping(ping) => Some(&ping.method),
            UltraWave::Ripple(ripple) => Some(&ripple.method),
            UltraWave::Signal(signal) => Some(&signal.method),
            _ => None,
        }
    }

    pub fn is_directed(&self) -> bool {
        match self {
            UltraWave::Ping(_) => true,
            UltraWave::Pong(_) => false,
            UltraWave::Ripple(_) => true,
            UltraWave::Echo(_) => false,
            UltraWave::Signal(_) => true,
        }
    }

    pub fn is_reflected(&self) -> bool {
        match self {
            UltraWave::Ping(_) => false,
            UltraWave::Pong(_) => true,
            UltraWave::Ripple(_) => false,
            UltraWave::Echo(_) => true,
            UltraWave::Signal(_) => false,
        }
    }

    pub fn to(&self) -> Recipients {
        match self {
            UltraWave::Ping(ping) => ping.to.clone().to_recipients(),
            UltraWave::Pong(pong) => pong.to.clone().to_recipients(),
            UltraWave::Ripple(ripple) => ripple.to.clone(),
            UltraWave::Echo(echo) => echo.to.clone().to_recipients(),
            UltraWave::Signal(signal) => signal.to.clone().to_recipients(),
        }
    }

    pub fn from(&self) -> &Surface {
        match self {
            UltraWave::Ping(ping) => &ping.from,
            UltraWave::Pong(pong) => &pong.from,
            UltraWave::Ripple(ripple) => &ripple.from,
            UltraWave::Echo(echo) => &echo.from,
            UltraWave::Signal(signal) => &signal.from,
        }
    }

    pub fn set_agent(&mut self, agent: Agent) {
        match self {
            UltraWave::Ping(ping) => ping.agent = agent,
            UltraWave::Pong(pong) => pong.agent = agent,
            UltraWave::Ripple(ripple) => ripple.agent = agent,
            UltraWave::Echo(echo) => echo.agent = agent,
            UltraWave::Signal(signal) => signal.agent = agent,
        }
    }

    pub fn set_to(&mut self, to: Surface) {
        match self {
            UltraWave::Ping(ping) => ping.to = to,
            UltraWave::Pong(pong) => pong.to = to,
            UltraWave::Ripple(ripple) => ripple.to = to.to_recipients(),
            UltraWave::Echo(echo) => echo.to = to,
            UltraWave::Signal(signal) => signal.to = to,
        }
    }

    pub fn set_from(&mut self, from: Surface) {
        match self {
            UltraWave::Ping(ping) => ping.from = from,
            UltraWave::Pong(pong) => pong.from = from,
            UltraWave::Ripple(ripple) => ripple.from = from,
            UltraWave::Echo(echo) => echo.from = from,
            UltraWave::Signal(signal) => signal.from = from,
        }
    }

    pub fn agent(&self) -> &Agent {
        match self {
            UltraWave::Ping(ping) => &ping.agent,
            UltraWave::Pong(pong) => &pong.agent,
            UltraWave::Ripple(ripple) => &ripple.agent,
            UltraWave::Echo(echo) => &echo.agent,
            UltraWave::Signal(signal) => &signal.agent,
        }
    }

    pub fn handling(&self) -> &Handling {
        match self {
            UltraWave::Ping(ping) => &ping.handling,
            UltraWave::Pong(pong) => &pong.handling,
            UltraWave::Ripple(ripple) => &ripple.handling,
            UltraWave::Echo(echo) => &echo.handling,
            UltraWave::Signal(signal) => &signal.handling,
        }
    }

    pub fn track(&self) -> bool {
        match self {
            UltraWave::Ping(ping) => ping.track,
            UltraWave::Pong(pong) => pong.track,
            UltraWave::Ripple(ripple) => ripple.track,
            UltraWave::Echo(echo) => echo.track,
            UltraWave::Signal(signal) => signal.track,
        }
    }

    pub fn set_track(&mut self, track: bool) {
        match self {
            UltraWave::Ping(ping) => ping.track = track,
            UltraWave::Pong(pong) => pong.track = track,
            UltraWave::Ripple(ripple) => ripple.track = track,
            UltraWave::Echo(echo) => echo.track = track,
            UltraWave::Signal(signal) => signal.track = track,
        }
    }

    pub fn scope(&self) -> &Scope {
        match self {
            UltraWave::Ping(ping) => &ping.scope,
            UltraWave::Pong(pong) => &pong.scope,
            UltraWave::Ripple(ripple) => &ripple.scope,
            UltraWave::Echo(echo) => &echo.scope,
            UltraWave::Signal(signal) => &signal.scope,
        }
    }
    pub fn to_ripple(self) -> Result<Wave<Ripple>, SpaceErr> {
        match self {
            UltraWave::Ripple(ripple) => Ok(ripple),
            _ => Err("not a ripple".into()),
        }
    }

    pub fn transported(&self) -> Option<&UltraWave> {
        match self {
            UltraWave::Ping(w) => w.core.body.ultrawave(),
            UltraWave::Pong(w) => w.core.body.ultrawave(),
            UltraWave::Ripple(w) => w.core.body.ultrawave(),
            UltraWave::Echo(w) => w.core.body.ultrawave(),
            UltraWave::Signal(w) => w.core.body.ultrawave(),
        }
    }

    pub fn transported_mut(&mut self) -> Option<&mut UltraWave> {
        match self {
            UltraWave::Ping(w) => w.core.body.ultrawave_mut(),
            UltraWave::Pong(w) => w.core.body.ultrawave_mut(),
            UltraWave::Ripple(w) => w.core.body.ultrawave_mut(),
            UltraWave::Echo(w) => w.core.body.ultrawave_mut(),
            UltraWave::Signal(w) => w.core.body.ultrawave_mut(),
        }
    }
}

impl<S> ToSubstance<S> for UltraWave
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        match self {
            UltraWave::Ping(ping) => ping.to_substance(),
            UltraWave::Pong(pong) => pong.to_substance(),
            UltraWave::Ripple(ripple) => ripple.to_substance(),
            UltraWave::Echo(echo) => echo.to_substance(),
            UltraWave::Signal(signal) => signal.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        match self {
            UltraWave::Ping(ping) => ping.to_substance_ref(),
            UltraWave::Pong(pong) => pong.to_substance_ref(),
            UltraWave::Ripple(ripple) => ripple.to_substance_ref(),
            UltraWave::Echo(echo) => echo.to_substance_ref(),
            UltraWave::Signal(signal) => signal.to_substance_ref(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct WaveId {
    uuid: Uuid,
    kind: WaveKind,
}

impl WaveId {
    pub fn new(kind: WaveKind) -> Self {
        let uuid = uuid();
        Self::with_uuid(kind, uuid)
    }

    pub fn with_uuid(kind: WaveKind, uuid: Uuid) -> Self {
        Self { uuid, kind }
    }

    pub fn to_short_string(&self) -> String {
        if self.uuid.to_string().len() > 8 {
            format!(
                "<Wave<{}>>::{}",
                self.kind.to_string(),
                self.uuid.to_string().as_str()[..8].to_string()
            )
        } else {
            self.to_string()
        }
    }
}

impl ToString for WaveId {
    fn to_string(&self) -> String {
        format!(
            "<Wave<{}>>::{}",
            self.kind.to_string(),
            self.uuid.to_string()
        )
    }
}

pub trait Reflectable<R> {
    fn forbidden(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(403, responder)
    }

    fn bad_request(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(400, responder)
    }

    fn not_found(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(404, responder)
    }

    fn timeout(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(408, responder)
    }

    fn server_error(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(500, responder)
    }

    fn status(self, status: u16, responder: Surface) -> R
    where
        Self: Sized;

    fn fail<M: ToString>(self, status: u16, message: M, responder: Surface) -> R
    where
        Self: Sized;

    fn err(self, err: SpaceErr, responder: Surface) -> R
    where
        Self: Sized;

    fn ok(self, responder: Surface) -> R
    where
        Self: Sized,
    {
        self.status(200, responder)
    }

    fn ok_body(self, body: Substance, responder: Surface) -> R
    where
        Self: Sized;

    fn core(self, core: ReflectedCore, responder: Surface) -> R
    where
        Self: Sized;

    fn result<C: Into<ReflectedCore>>(self, result: Result<C, SpaceErr>, responder: Surface) -> R
    where
        Self: Sized,
    {
        match result {
            Ok(core) => self.core(core.into(), responder),
            Err(err) => self.core(err.into(), responder),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirectWaveStub {
    pub id: WaveId,
    pub agent: Agent,
    pub handling: Handling,
    pub from: Surface,
    pub to: Recipients,
    pub span: Option<TrailSpanId>,
}

impl Into<WaitTime> for &DirectWaveStub {
    fn into(self) -> WaitTime {
        self.handling.wait.clone()
    }
}

pub type Ripple = RippleDef<Recipients>;
pub type SingularRipple = RippleDef<Surface>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RippleDef<T: ToRecipients + Clone> {
    pub to: T,
    pub core: DirectedCore,
    pub bounce_backs: BounceBacks,
    pub history: HashSet<Point>,
}

impl Ripple {
    pub fn new<T>(core: DirectedCore, to: T, bounce_backs: BounceBacks) -> Self
    where
        T: ToRecipients,
    {
        Self {
            to: to.to_recipients(),
            core,
            bounce_backs,
            history: HashSet::new(),
        }
    }
}

impl<T> RippleDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn replace_to<T2: ToRecipients + Clone>(self, to: T2) -> RippleDef<T2> {
        RippleDef {
            to,
            core: self.core,
            bounce_backs: self.bounce_backs,
            history: self.history,
        }
    }
}

impl Wave<SingularRipple> {
    pub fn to_singular_ultra(self) -> SingularUltraWave {
        SingularUltraWave::Ripple(self)
    }

    pub fn to_multiple(self) -> Wave<Ripple> {
        let ripple = self
            .variant
            .clone()
            .replace_to(self.variant.to.clone().to_recipients());
        self.replace(ripple)
    }
}

impl Wave<SingularRipple> {
    pub fn as_multi(&self, recipients: Recipients) -> Wave<Ripple> {
        let ripple = self.variant.clone().replace_to(recipients);
        self.clone().replace(ripple)
    }
}

impl Wave<Ripple> {
    pub fn as_single(&self, surface: Surface) -> Wave<SingularRipple> {
        let ripple = self.variant.clone().replace_to(surface);
        self.clone().replace(ripple)
    }

    pub fn to_singular_directed(self) -> Result<SingularDirectedWave, SpaceErr> {
        let to = self.to.clone().to_single()?;
        Ok(self.as_single(to).to_singular_directed())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum BounceBacks {
    None,
    Single,
    Count(usize),
    Timer(WaitTime),
}

impl BounceBacks {
    pub fn has_bounce(&self) -> bool {
        match self {
            BounceBacks::None => false,
            BounceBacks::Single => true,
            BounceBacks::Count(_) => true,
            BounceBacks::Timer(_) => true,
        }
    }
}

impl<S, T> ToSubstance<S> for RippleDef<T>
where
    Substance: ToSubstance<S>,
    T: ToRecipients + Clone,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.core.to_substance_ref()
    }
}

impl<T> RippleDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn require_method<M: Into<Method> + ToString + Clone>(
        self,
        method: M,
    ) -> Result<RippleDef<T>, SpaceErr> {
        if self.core.method == method.clone().into() {
            Ok(self)
        } else {
            Err(SpaceErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            ))
        }
    }

    pub fn require_body<B>(self) -> Result<B, SpaceErr>
    where
        B: TryFrom<Substance, Error = SpaceErr>,
    {
        match B::try_from(self.body.clone()) {
            Ok(body) => Ok(body),
            Err(err) => Err(SpaceErr::bad_request("expected a body")),
        }
    }
}

impl<T> Deref for RippleDef<T>
where
    T: ToRecipients + Clone,
{
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl<T> DerefMut for RippleDef<T>
where
    T: ToRecipients + Clone,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Signal {
    pub to: Surface,
    pub core: DirectedCore,
}

impl WaveVariant for Signal {
    fn kind(&self) -> WaveKind {
        WaveKind::Signal
    }
}

impl Signal {
    pub fn new(to: Surface, core: DirectedCore) -> Self {
        Self { to, core }
    }

    pub fn bounce_backs(&self) -> BounceBacks {
        BounceBacks::None
    }
}

impl Deref for Signal {
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl DerefMut for Signal {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Ping {
    pub to: Surface,
    pub core: DirectedCore,
}

impl Wave<Ping> {
    pub fn to_singular_directed(self) -> SingularDirectedWave {
        SingularDirectedWave::Ping(self)
    }

    pub fn with_core(mut self, core: DirectedCore) -> Self {
        self.variant.core = core;
        self
    }
}

impl<S> ToSubstance<S> for Ping
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.core.to_substance_ref()
    }
}

impl Deref for Ping {
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl DerefMut for Ping {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

impl Into<DirectedProto> for Wave<Ping> {
    fn into(self) -> DirectedProto {
        let mut core = self.core.clone();
        DirectedProto {
            to: Some(self.to.clone().to_recipients()),
            method: None,
            core,
            id: self.id,
            from: Some(self.from),
            handling: Some(self.handling),
            scope: Some(self.scope),
            agent: Some(self.agent),
            kind: None,
            bounce_backs: None,
            track: self.track,
            via: self.via,
            history: HashSet::new(),
        }
    }
}

impl Ping {
    pub fn require_method<M: Into<Method> + ToString + Clone>(
        self,
        method: M,
    ) -> Result<Ping, SpaceErr> {
        if self.core.method == method.clone().into() {
            Ok(self)
        } else {
            Err(SpaceErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            ))
        }
    }

    pub fn require_body<B>(self) -> Result<B, SpaceErr>
    where
        B: TryFrom<Substance, Error = SpaceErr>,
    {
        match B::try_from(self.clone().core.body) {
            Ok(body) => Ok(body),
            Err(err) => Err(SpaceErr::bad_request("body is required")),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct WaveXtra<V> {
    pub wave: Wave<V>,
    pub session: Session,
}

impl<V> WaveXtra<V> {
    pub fn new(wave: Wave<V>, session: Session) -> Self {
        Self { wave, session }
    }
}

impl TryFrom<Ping> for RawCommand {
    type Error = SpaceErr;

    fn try_from(request: Ping) -> Result<Self, Self::Error> {
        request.core.body.try_into()
    }
}

impl TryFrom<Pong> for Substance {
    type Error = SpaceErr;

    fn try_from(response: Pong) -> Result<Self, Self::Error> {
        Ok(response.core.body)
    }
}

impl TryInto<Bin> for Pong {
    type Error = SpaceErr;

    fn try_into(self) -> Result<Bin, Self::Error> {
        match self.core.body {
            Substance::Bin(bin) => Ok(bin),
            _ => Err(SpaceErr::bad_request("expected Bin")),
        }
    }
}

impl Into<DirectedCore> for RawCommand {
    fn into(self) -> DirectedCore {
        DirectedCore::substance(
            ExtMethod::new("ExecCommand").unwrap().into(),
            Substance::RawCommand(self),
        )
    }
}

impl Ping {
    pub fn new<P: ToSurface>(core: DirectedCore, to: P) -> Self {
        Self {
            to: to.to_surface(),
            core,
        }
    }
}

#[derive(Clone, strum_macros::Display)]
pub enum ReflectedKind {
    Pong,
    Echo,
}

#[derive(Clone, strum_macros::Display)]
pub enum DirectedKind {
    Ping,
    Ripple,
    Signal,
}

#[derive(Clone)]
pub struct ReflectedProto {
    pub id: WaveId,
    pub intended: Option<Recipients>,
    pub from: Option<Surface>,
    pub to: Option<Surface>,
    pub body: Option<Substance>,
    pub status: Option<StatusCode>,
    pub handling: Option<Handling>,
    pub scope: Option<Scope>,
    pub agent: Option<Agent>,
    pub reflection_of: Option<WaveId>,
    pub kind: Option<ReflectedKind>,
    pub track: bool,
}

impl ReflectedProto {
    pub fn new() -> Self {
        Self {
            id: WaveId::new(WaveKind::Echo),
            intended: None,
            from: None,
            to: None,
            body: None,
            status: None,
            handling: None,
            scope: None,
            agent: None,
            reflection_of: None,
            kind: None,
            track: false,
        }
    }

    pub fn kind(&mut self, kind: ReflectedKind) {
        self.kind.replace(kind);
    }

    pub fn fill<V>(&mut self, wave: &Wave<V>) {
        self.reflection_of = Some(wave.id.clone());
        self.fill_to(&wave.from);
        self.fill_handling(&wave.handling);
        self.fill_scope(&wave.scope);
        self.fill_agent(&wave.agent);
        self.reflection_of = Some(wave.id.clone());
    }

    pub fn fill_kind(&mut self, kind: ReflectedKind) {
        if self.kind.is_none() {
            self.kind.replace(kind);
        }
    }

    pub fn fill_intended<I: ToRecipients + Clone>(&mut self, intended: I) {
        if self.intended.is_none() {
            self.intended.replace(intended.to_recipients());
        }
    }

    pub fn fill_to(&mut self, to: &Surface) {
        if self.to.is_none() {
            self.to.replace(to.clone());
        }
    }

    pub fn fill_from(&mut self, from: &Surface) {
        if self.from.is_none() {
            self.from.replace(from.clone());
        }
    }

    pub fn fill_scope(&mut self, scope: &Scope) {
        if self.scope.is_none() {
            self.scope.replace(scope.clone());
        }
    }

    pub fn fill_agent(&mut self, agent: &Agent) {
        if self.agent.is_none() {
            self.agent.replace(agent.clone());
        }
    }

    pub fn fill_handling(&mut self, handling: &Handling) {
        if self.handling.is_none() {
            self.handling.replace(handling.clone());
        }
    }

    pub fn fill_status(&mut self, status: &StatusCode) {
        if self.status.is_none() {
            self.status.replace(status.clone());
        }
    }

    pub fn body(&mut self, body: Substance) -> Result<(), SpaceErr> {
        self.body.replace(body);
        Ok(())
    }

    pub fn intended<I: ToRecipients + Clone>(&mut self, intended: I) {
        self.intended.replace(intended.to_recipients());
    }

    pub fn reflection_of(&mut self, id: WaveId) {
        self.reflection_of.replace(id);
    }

    pub fn status(&mut self, status: u16) {
        self.status
            .replace(StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()));
    }

    pub fn to(&mut self, to: Surface) {
        self.to.replace(to.clone());
    }

    pub fn scope(&mut self, scope: Scope) {
        self.scope.replace(scope);
    }

    pub fn agent(&mut self, agent: Agent) {
        self.agent.replace(agent);
    }

    pub fn handling(&mut self, handling: Handling) {
        self.handling.replace(handling);
    }

    pub fn from(&mut self, from: Surface) {
        self.from.replace(from);
    }

    pub fn build(self) -> Result<ReflectedWave, SpaceErr> {
        let mut core = ReflectedCore::new();
        core.body = self.body.or_else(|| Some(Substance::Empty)).unwrap();
        core.status = self
            .status
            .or_else(|| Some(StatusCode::from_u16(200u16).unwrap()))
            .unwrap();
        match self.kind.ok_or("missing ReflectedWave Kind")? {
            ReflectedKind::Pong => {
                let mut pong = Wave::new(
                    Pong::new(
                        core,
                        self.to.ok_or("ReflectedProto missing to")?,
                        self.intended.ok_or("Reflected Proto Missing intended")?,
                        self.reflection_of.ok_or("response to expected")?,
                    ),
                    self.from.ok_or("expected from")?,
                );
                pong.track = self.track;
                Ok(pong.to_reflected())
            }
            ReflectedKind::Echo => {
                let mut echo = Wave::new(
                    Echo::new(
                        core,
                        self.to.ok_or("ReflectedProto missing to")?,
                        self.intended.ok_or("Reflected Proto Missing intended")?,
                        self.reflection_of.ok_or("response to expected")?,
                    ),
                    self.from.ok_or("expected from")?,
                );
                echo.track = self.track;
                Ok(echo.to_reflected())
            }
        }
    }
}

#[derive(Clone)]
pub struct DirectedProto {
    pub id: WaveId,
    pub from: Option<Surface>,
    pub to: Option<Recipients>,
    pub core: DirectedCore,
    pub method: Option<Method>,
    pub handling: Option<Handling>,
    pub scope: Option<Scope>,
    pub agent: Option<Agent>,
    pub kind: Option<DirectedKind>,
    pub bounce_backs: Option<BounceBacks>,
    pub via: Option<Surface>,
    pub track: bool,
    pub history: HashSet<Point>,
}
impl Trackable for DirectedProto {
    fn track_id(&self) -> String {
        self.id.to_short_string()
    }

    fn track_method(&self) -> String {
        self.core.method.to_deep_string()
    }

    fn track_payload(&self) -> String {
        self.core.body.to_string()
    }

    fn track_from(&self) -> String {
        match &self.from {
            None => "None".to_string(),
            Some(from) => from.to_string(),
        }
    }

    fn track_to(&self) -> String {
        match &self.to {
            None => "None".to_string(),
            Some(to) => to.to_string(),
        }
    }

    fn track(&self) -> bool {
        self.track
    }
}

impl DirectedProto {
    pub fn build(self) -> Result<DirectedWave, SpaceErr> {
        let kind = self.kind.ok_or::<SpaceErr>(
            "kind must be set for DirectedProto to create the proper DirectedWave".into(),
        )?;

        let mut core = self.core.clone();
        if let Some(method) = self.method {
            core.method = method;
        }

        let mut wave = match kind {
            DirectedKind::Ping => {
                let mut wave = Wave::new(
                    Ping {
                        to: self
                            .to
                            .ok_or(SpaceErr::new(500u16, "must set 'to'"))?
                            .single_or()?,
                        core,
                    },
                    self.from.ok_or(SpaceErr::new(500u16, "must set 'from'"))?,
                );
                wave.agent = self.agent.unwrap_or_else(|| Agent::Anonymous);
                wave.handling = self.handling.unwrap_or_else(|| Handling::default());
                wave.scope = self.scope.unwrap_or_else(|| Scope::None);
                wave.via = self.via;
                wave.track = self.track;
                wave.to_directed()
            }
            DirectedKind::Ripple => {
                let mut wave = Wave::new(
                    Ripple {
                        to: self.to.ok_or(SpaceErr::new(500u16, "must set 'to'"))?,
                        core,
                        bounce_backs: self.bounce_backs.ok_or("BounceBacks must be set")?,
                        history: self.history,
                    },
                    self.from.ok_or(SpaceErr::new(500u16, "must set 'from'"))?,
                );
                wave.agent = self.agent.unwrap_or_else(|| Agent::Anonymous);
                wave.handling = self.handling.unwrap_or_else(|| Handling::default());
                wave.scope = self.scope.unwrap_or_else(|| Scope::None);
                wave.via = self.via;
                wave.track = self.track;
                wave.to_directed()
            }
            DirectedKind::Signal => {
                let mut wave = Wave::new(
                    Signal {
                        to: self
                            .to
                            .ok_or(SpaceErr::new(500u16, "must set 'to'"))?
                            .single_or()?,
                        core,
                    },
                    self.from.ok_or(SpaceErr::new(500u16, "must set 'from'"))?,
                );
                wave.agent = self.agent.unwrap_or_else(|| Agent::Anonymous);
                wave.handling = self.handling.unwrap_or_else(|| Handling::default());
                wave.scope = self.scope.unwrap_or_else(|| Scope::None);
                wave.via = self.via;
                wave.track = self.track;
                wave.to_directed()
            }
        };

        Ok(wave)
    }

    pub fn fill(&mut self, wave: &UltraWave) {
        self.fill_handling(wave.handling());
        self.fill_scope(wave.scope());
        self.fill_agent(wave.agent());
    }

    pub fn fill_kind(&mut self, kind: DirectedKind) {
        if self.kind.is_none() {
            self.kind.replace(kind);
        }
    }

    pub fn fill_to<R: ToRecipients + Clone>(&mut self, to: R) {
        if self.to.is_none() {
            self.to.replace(to.to_recipients());
        }
    }

    pub fn fill_from<P: ToSurface>(&mut self, from: P) {
        if self.from.is_none() {
            self.from.replace(from.to_surface());
        }
    }

    pub fn fill_scope(&mut self, scope: &Scope) {
        if self.scope.is_none() {
            self.scope.replace(scope.clone());
        }
    }

    pub fn fill_agent(&mut self, agent: &Agent) {
        if self.agent.is_none() {
            self.agent.replace(agent.clone());
        }
    }

    pub fn fill_handling(&mut self, handling: &Handling) {
        if self.handling.is_none() {
            self.handling.replace(handling.clone());
        }
    }

    pub fn fill_method(&mut self, method: &Method) {
        if self.method.is_none() {
            self.method.replace(method.clone());
        }
    }

    pub fn agent(&mut self, agent: Agent) {
        self.agent.replace(agent);
    }

    pub fn bounce_backs(&mut self, bounce_backs: BounceBacks) {
        self.bounce_backs.replace(bounce_backs);
    }

    pub fn scope(&mut self, scope: Scope) {
        self.scope.replace(scope);
    }

    pub fn handling(&mut self, handling: Handling) {
        self.handling.replace(handling);
    }

    pub fn kind(kind: &DirectedKind) -> Self {
        match kind {
            DirectedKind::Ping => Self::ping(),
            DirectedKind::Ripple => Self::ripple(),
            DirectedKind::Signal => Self::signal(),
        }
    }

    pub fn body(&mut self, body: Substance) {
        self.core.body = body;
    }

    pub fn history(&mut self, history: HashSet<Point>) {
        self.history = history;
    }

    pub fn uri(&mut self, uri: Url) {
        self.core.uri = uri;
    }

    pub fn core(&mut self, core: DirectedCore) -> Result<(), SpaceErr> {
        self.core = core;
        Ok(())
    }

    pub fn method<M: Into<Method> + Clone>(&mut self, method: M) {
        self.method.replace(method.clone().into());
        self.core.method = method.into();
    }

    pub fn to<P: ToRecipients + Clone>(&mut self, to: P) {
        self.to.replace(to.to_recipients());
    }

    pub fn from<P: ToSurface>(&mut self, from: P) {
        self.from.replace(from.to_surface());
    }

    pub fn fill_via<P: ToSurface>(&mut self, via: P) {
        if self.via.is_none() {
            self.via.replace(via.to_surface());
        }
    }

    pub fn via<P: ToSurface>(&mut self, via: &P) {
        self.via.replace(via.to_surface());
    }
}

impl DirectedProto {
    pub fn ping() -> Self {
        Self {
            id: WaveId::new(WaveKind::Ping),
            kind: Some(DirectedKind::Ping),
            bounce_backs: Some(BounceBacks::Single),
            ..DirectedProto::default()
        }
    }

    pub fn signal() -> Self {
        Self {
            id: WaveId::new(WaveKind::Signal),
            kind: Some(DirectedKind::Signal),
            bounce_backs: Some(BounceBacks::None),
            ..DirectedProto::default()
        }
    }

    pub fn ripple() -> Self {
        Self {
            id: WaveId::new(WaveKind::Ripple),
            kind: Some(DirectedKind::Ripple),
            ..DirectedProto::default()
        }
    }

    pub fn to_with_method<P: ToRecipients + Clone>(to: P, method: Method) -> Self {
        Self {
            id: WaveId::new(WaveKind::Ping),
            to: Some(to.to_recipients()),
            kind: Some(DirectedKind::Ping),
            core: DirectedCore::new(method),
            ..DirectedProto::default()
        }
    }

    pub fn from_core(core: DirectedCore) -> Self {
        Self {
            id: WaveId::new(WaveKind::Ping),
            kind: Some(DirectedKind::Ping),
            core,
            ..DirectedProto::default()
        }
    }

    pub fn sys<M: Into<HypMethod>, P: ToRecipients + Clone>(to: P, method: M) -> Self {
        let method: HypMethod = method.into();
        let method: Method = method.into();
        Self::to_with_method(to, method)
    }

    pub fn msg<M: Into<ExtMethod>, P: ToRecipients + Clone>(to: P, method: M) -> Self {
        let method: ExtMethod = method.into();
        let method: Method = method.into();
        Self::to_with_method(to, method)
    }

    pub fn http<M: Into<HttpMethod>, P: ToRecipients + Clone>(to: P, method: M) -> Self {
        let method: HttpMethod = method.into();
        let method: Method = method.into();
        Self::to_with_method(to, method)
    }

    pub fn cmd<M: Into<CmdMethod>, P: ToRecipients + Clone>(to: P, method: M) -> Self {
        let method: CmdMethod = method.into();
        let method: Method = method.into();
        Self::to_with_method(to, method)
    }
}

impl Default for DirectedProto {
    fn default() -> Self {
        Self {
            id: WaveId::new(WaveKind::Ping),
            method: None,
            core: DirectedCore::default(),
            from: None,
            to: None,
            handling: None,
            scope: None,
            agent: None,
            kind: None,
            bounce_backs: None,
            via: None,
            track: false,
            history: Default::default(),
        }
    }
}

pub type Echoes = Vec<Wave<Echo>>;

impl FromReflectedAggregate for () {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> Result<Self, SpaceErr>
    where
        Self: Sized,
    {
        match agg {
            ReflectedAggregate::None => Ok(()),
            _ => Err(SpaceErr::bad_request(
                "expected a ReflectedAggregate of None",
            )),
        }
    }
}

impl FromReflectedAggregate for Echoes {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> Result<Self, SpaceErr>
    where
        Self: Sized,
    {
        match agg {
            ReflectedAggregate::Multi(reflected) => {
                let mut echoes = Echoes::new();
                for r in reflected {
                    let echo: Wave<Echo> = r.to_echo()?;
                    echoes.push(echo);
                }
                Ok(echoes)
            }
            _ => Err(SpaceErr::bad_request(
                "expecting a ReflectedAggregate of Multi",
            )),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Echo {
    /// this is meant to be the intended request recipient, which may not be the point responding
    /// to this message in the case it was intercepted and filtered at some point
    pub to: Surface,
    pub intended: Recipients,
    pub core: ReflectedCore,
    pub reflection_of: WaveId,
}

impl<S> ToSubstance<S> for Echo
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.core.to_substance_ref()
    }
}

impl Echo {
    pub fn is_ok(&self) -> bool {
        self.core.is_ok()
    }

    pub fn core<E>(result: Result<Wave<Pong>, E>) -> ReflectedCore {
        match result {
            Ok(reflected) => reflected.variant.core,
            Err(err) => ReflectedCore::server_error(),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        self.core.as_result()
    }
}

impl Echo {
    pub fn new(
        core: ReflectedCore,
        to: Surface,
        intended: Recipients,
        reflection_of: WaveId,
    ) -> Self {
        Self {
            to,
            intended,
            core,
            reflection_of,
        }
    }

    pub fn ok_or(self) -> Result<Self, SpaceErr> {
        if self.core.status.is_success() {
            Ok(self)
        } else {
            if let Substance::Text(error) = self.core.body {
                Err(error.into())
            } else {
                Err(format!("error code: {}", self.core.status.to_string()).into())
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Pong {
    /// this is meant to be the intended request recipient, which may not be the point responding
    /// to this message in the case it was intercepted and filtered at some point
    pub to: Surface,
    pub intended: Recipients,
    pub core: ReflectedCore,
    pub reflection_of: WaveId,
}

impl FromReflectedAggregate for Wave<Pong> {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> Result<Self, SpaceErr>
    where
        Self: Sized,
    {
        match agg {
            ReflectedAggregate::Single(reflected) => match reflected {
                ReflectedWave::Pong(pong) => Ok(pong),
                _ => Err(SpaceErr::bad_request("expected a Pong Reflected")),
            },
            ReflectedAggregate::None => Err(SpaceErr::bad_request(
                "expected a Single Reflected, encountered: None",
            )),
            ReflectedAggregate::Multi(_) => Err(SpaceErr::bad_request(
                "expected a Single Reflected, encountered: Multi",
            )),
        }
    }
}

impl<S> ToSubstance<S> for Pong
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.core.to_substance_ref()
    }
}

impl Pong {
    pub fn is_ok(&self) -> bool {
        self.core.is_ok()
    }

    pub fn core<E>(result: Result<Wave<Pong>, E>) -> ReflectedCore {
        match result {
            Ok(reflected) => reflected.variant.core,
            Err(err) => ReflectedCore::server_error(),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        self.core.as_result()
    }

    pub fn ok_or(&self) -> Result<(), SpaceErr> {
        if self.is_ok() {
            Ok(())
        } else {
            if let Substance::FormErrs(errs) = &self.core.body {
                Err(format!("{} : {}", self.core.status.to_string(), errs.to_string()).into())
            } else if let Substance::Err(err) = &self.core.body {
                Err(err.clone())
            } else {
                Err(self.core.status.to_string().into())
            }
        }
    }
}

impl Pong {
    pub fn new(
        core: ReflectedCore,
        to: Surface,
        intended: Recipients,
        reflection_of: WaveId,
    ) -> Self {
        Self {
            to,
            intended,
            core,
            reflection_of,
        }
    }
}

pub struct RecipientSelector<'a> {
    pub to: &'a Surface,
    pub wave: &'a DirectedWave,
}

impl<'a> RecipientSelector<'a> {
    pub fn new(to: &'a Surface, wave: &'a Wave<DirectedWave>) -> Self {
        Self { to, wave }
    }
}

pub type DirectedWave = DirectedWaveDef<Recipients>;
pub type SingularDirectedWave = DirectedWaveDef<Surface>;

impl Into<DirectedProto> for DirectedWave {
    fn into(self) -> DirectedProto {
        let mut proto = DirectedProto {
            id: self.id().clone(),
            kind: Some(self.directed_kind()),
            ..DirectedProto::default()
        };
        proto.id = self.id().clone();
        proto.core(self.core().clone());
        proto.to(self.to());
        proto.from(self.from().clone());
        proto.agent(self.agent().clone());
        proto.scope(self.scope().clone());
        proto.handling(self.handling().clone());
        proto.track = self.track();
        proto.bounce_backs(self.bounce_backs());
        proto.agent(self.agent().clone());
        if let Some(via) = self.via() {
            proto.via(via);
        }
        proto
    }
}

impl Trackable for DirectedWave {
    fn track_id(&self) -> String {
        self.id().to_short_string()
    }

    fn track_method(&self) -> String {
        match self {
            Self::Ping(ping) => ping.core.method.to_deep_string(),
            Self::Ripple(ripple) => ripple.core.method.to_deep_string(),
            Self::Signal(signal) => signal.core.method.to_deep_string(),
        }
    }

    fn track_payload(&self) -> String {
        match self {
            Self::Ping(ping) => ping.core.body.kind().to_string(),
            Self::Ripple(ripple) => ripple.core.body.kind().to_string(),
            Self::Signal(signal) => signal.core.body.kind().to_string(),
        }
    }

    fn track_from(&self) -> String {
        self.from().to_string()
    }

    fn track_to(&self) -> String {
        self.to().to_string()
    }

    fn track(&self) -> bool {
        match self {
            Self::Ping(ping) => ping.track,
            Self::Ripple(ripple) => ripple.track,
            Self::Signal(signal) => signal.track,
        }
    }
    fn track_payload_fmt(&self) -> String {
        match self {
            Self::Signal(signal) => signal.track_payload_fmt(),
            Self::Ping(ping) => ping.track_payload_fmt(),
            Self::Ripple(_) => self.track_payload(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum DirectedWaveDef<T>
where
    T: ToRecipients + Clone,
{
    Ping(Wave<Ping>),
    Ripple(Wave<RippleDef<T>>),
    Signal(Wave<Signal>),
}

impl<T> DirectedWaveDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn kind(&self) -> WaveKind {
        match self {
            DirectedWaveDef::Ping(_) => WaveKind::Ping,
            DirectedWaveDef::Ripple(_) => WaveKind::Ripple,
            DirectedWaveDef::Signal(_) => WaveKind::Signal,
        }
    }

    pub fn has_visited(&self, star: &Point) -> bool {
        match self {
            Self::Ripple(ripple) => ripple.history.contains(star),
            _ => false,
        }
    }

    pub fn add_history(&mut self, point: &Point) {
        match self {
            Self::Ping(_) => {}
            Self::Ripple(ripple) => {
                ripple.history.insert(point.clone());
            }
            Self::Signal(_) => {}
        }
    }

    pub fn history(&self) -> HashSet<Point> {
        match self {
            Self::Ping(_) => HashSet::new(),
            Self::Ripple(ripple) => ripple.history.clone(),
            Self::Signal(_) => HashSet::new(),
        }
    }
}

impl<W> Spannable for DirectedWaveDef<W>
where
    W: ToRecipients + Clone,
{
    fn span_id(&self) -> String {
        self.id().to_string()
    }

    fn span_type(&self) -> &'static str {
        "Wave"
    }
}

impl DirectedWave {
    pub fn to(&self) -> Recipients {
        match self {
            Self::Ping(ping) => ping.to.clone().to_recipients(),
            Self::Ripple(ripple) => ripple.to.clone(),
            Self::Signal(signal) => signal.to.clone().to_recipients(),
        }
    }

    pub fn hops(&self) -> u16 {
        match self {
            DirectedWave::Ping(ping) => ping.hops.clone(),
            DirectedWave::Ripple(ripple) => ripple.hops.clone(),
            DirectedWave::Signal(signal) => signal.hops.clone(),
        }
    }

    pub fn is_signal(&self) -> bool {
        match self {
            DirectedWave::Signal(_) => true,
            _ => false,
        }
    }

    pub fn reflection(&self) -> Result<Reflection, SpaceErr> {
        Ok(Reflection {
            kind: match self {
                DirectedWave::Ping(_) => ReflectedKind::Pong,
                DirectedWave::Ripple(_) => ReflectedKind::Echo,
                DirectedWave::Signal(_) => return Err("signals do not have a reflected".into()),
            },
            to: self.reflect_to().clone(),
            intended: self.to(),
            reflection_of: self.id().clone(),
            track: self.track(),
        })
    }

    pub fn to_signal(self) -> Result<Wave<Signal>, SpaceErr> {
        match self {
            DirectedWave::Signal(signal) => Ok(signal),
            _ => Err("not a signal wave".into()),
        }
    }

    pub fn to_call(&self, to: Surface) -> Result<Call, SpaceErr> {
        let kind = match &self.core().method {
            Method::Cmd(method) => CallKind::Cmd(CmdCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Hyp(method) => CallKind::Hyp(HypCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Http(method) => CallKind::Http(HttpCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Ext(method) => CallKind::Ext(ExtCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
        };

        Ok(Call {
            point: to.point,
            kind,
        })
    }
}

impl Trackable for SingularDirectedWave {
    fn track_id(&self) -> String {
        self.id().to_short_string()
    }

    fn track_method(&self) -> String {
        match self {
            Self::Ping(ping) => ping.core.method.to_deep_string(),
            Self::Ripple(ripple) => ripple.core.method.to_deep_string(),
            Self::Signal(signal) => signal.core.method.to_deep_string(),
        }
    }

    fn track_payload(&self) -> String {
        match self {
            Self::Ping(ping) => ping.core.body.kind().to_string(),
            Self::Ripple(ripple) => ripple.core.body.kind().to_string(),
            Self::Signal(signal) => signal.core.body.kind().to_string(),
        }
    }

    fn track_from(&self) -> String {
        self.from().to_string()
    }

    fn track_to(&self) -> String {
        self.to().to_string()
    }

    fn track(&self) -> bool {
        match self {
            Self::Ping(ping) => ping.track,
            Self::Ripple(ripple) => ripple.track,
            Self::Signal(signal) => signal.track,
        }
    }
    fn track_payload_fmt(&self) -> String {
        match self {
            Self::Signal(signal) => signal.track_payload_fmt(),
            Self::Ping(ping) => ping.track_payload_fmt(),
            Self::Ripple(_) => self.track_payload(),
        }
    }
}
impl SingularDirectedWave {
    pub fn to(&self) -> Surface {
        match self {
            Self::Ping(ping) => ping.to.clone(),
            Self::Ripple(ripple) => ripple.to.clone(),
            Self::Signal(signal) => signal.to.clone(),
        }
    }

    pub fn to_call(&self) -> Result<Call, SpaceErr> {
        let kind = match &self.core().method {
            Method::Cmd(method) => CallKind::Cmd(CmdCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Hyp(method) => CallKind::Hyp(HypCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Http(method) => CallKind::Http(HttpCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
            Method::Ext(method) => CallKind::Ext(ExtCall::new(
                method.clone(),
                Subst::new(self.core().uri.path())?,
            )),
        };

        Ok(Call {
            point: self.to().clone().to_point(),
            kind,
        })
    }

    pub fn reflection(&self) -> Result<Reflection, SpaceErr> {
        Ok(Reflection {
            kind: match self {
                SingularDirectedWave::Ping(_) => ReflectedKind::Pong,
                SingularDirectedWave::Ripple(_) => ReflectedKind::Echo,
                SingularDirectedWave::Signal(_) => {
                    return Err("signals do not have a reflected".into())
                }
            },
            to: self.from().clone(),
            intended: self.to().to_recipients(),
            reflection_of: self.id().clone(),
            track: self.track(),
        })
    }

    pub fn to_ultra(self) -> UltraWave {
        match self {
            SingularDirectedWave::Ping(ping) => UltraWave::Ping(ping),
            SingularDirectedWave::Signal(signal) => UltraWave::Signal(signal),
            SingularDirectedWave::Ripple(ripple) => UltraWave::Ripple(ripple.to_multiple()),
        }
    }
}

impl<T> DirectedWaveDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn id(&self) -> &WaveId {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.id,
            DirectedWaveDef::Ripple(ripple) => &ripple.id,
            DirectedWaveDef::Signal(signal) => &signal.id,
        }
    }

    pub fn agent(&self) -> &Agent {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.agent,
            DirectedWaveDef::Ripple(ripple) => &ripple.agent,
            DirectedWaveDef::Signal(signal) => &signal.agent,
        }
    }

    pub fn scope(&self) -> &Scope {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.scope,
            DirectedWaveDef::Ripple(ripple) => &ripple.scope,
            DirectedWaveDef::Signal(signal) => &signal.scope,
        }
    }

    pub fn handling(&self) -> &Handling {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.handling,
            DirectedWaveDef::Ripple(ripple) => &ripple.handling,
            DirectedWaveDef::Signal(signal) => &signal.handling,
        }
    }

    pub fn err(&self, err: SpaceErr, responder: Surface) -> Bounce<ReflectedWave> {
        match self {
            DirectedWaveDef::Ping(ping) => {
                Bounce::Reflected(ping.err(err, responder).to_reflected())
            }
            DirectedWaveDef::Ripple(ripple) => {
                Bounce::Reflected(ripple.err(err, responder).to_reflected())
            }
            DirectedWaveDef::Signal(_) => Bounce::Absorbed,
        }
    }

    pub fn bounce_backs(&self) -> BounceBacks {
        match self {
            DirectedWaveDef::Ping(ping) => ping.bounce_backs(),
            DirectedWaveDef::Ripple(ripple) => ripple.bounce_backs(),
            DirectedWaveDef::Signal(signal) => signal.bounce_backs(),
        }
    }

    pub fn set_bounce_backs(&mut self, bounce_backs: BounceBacks) -> Result<(), SpaceErr> {
        match self {
            DirectedWaveDef::Ripple(ripple) => {
                ripple.bounce_backs = bounce_backs;
                Ok(())
            }
            _ => Err(SpaceErr::server_error(
                "can only set bouncebacks for Ripple",
            )),
        }
    }

    pub fn from(&self) -> &Surface {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.from,
            DirectedWaveDef::Ripple(ripple) => &ripple.from,
            DirectedWaveDef::Signal(signal) => &signal.from,
        }
    }

    pub fn via(&self) -> &Option<Surface> {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.via,
            DirectedWaveDef::Ripple(ripple) => &ripple.via,
            DirectedWaveDef::Signal(signal) => &signal.via,
        }
    }

    pub fn reflect_to(&self) -> &Surface {
        self.via().as_ref().unwrap_or(self.from())
    }

    pub fn take_via(&mut self) -> Option<Surface> {
        match self {
            DirectedWaveDef::Ping(ping) => ping.via.take(),
            DirectedWaveDef::Ripple(ripple) => ripple.via.take(),
            DirectedWaveDef::Signal(signal) => signal.via.take(),
        }
    }

    pub fn replace_via(&mut self, surface: Surface) -> Option<Surface> {
        match self {
            DirectedWaveDef::Ping(ping) => ping.via.replace(surface),
            DirectedWaveDef::Ripple(ripple) => ripple.via.replace(surface),
            DirectedWaveDef::Signal(signal) => signal.via.replace(surface),
        }
    }

    pub fn body(&self) -> &Substance {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.core.body,
            DirectedWaveDef::Ripple(ripple) => &ripple.core.body,
            DirectedWaveDef::Signal(signal) => &signal.core.body,
        }
    }

    pub fn directed_kind(&self) -> DirectedKind {
        match self {
            DirectedWaveDef::Ping(_) => DirectedKind::Ping,
            DirectedWaveDef::Ripple(_) => DirectedKind::Ripple,
            DirectedWaveDef::Signal(_) => DirectedKind::Signal,
        }
    }

    pub fn core(&self) -> &DirectedCore {
        match self {
            DirectedWaveDef::Ping(ping) => &ping.core,
            DirectedWaveDef::Ripple(ripple) => &ripple.core,
            DirectedWaveDef::Signal(signal) => &signal.core,
        }
    }

    pub fn core_mut(&mut self) -> &mut DirectedCore {
        match self {
            DirectedWaveDef::Ping(ping) => &mut ping.core,
            DirectedWaveDef::Ripple(ripple) => &mut ripple.core,
            DirectedWaveDef::Signal(signal) => &mut signal.core,
        }
    }
}

#[derive(Clone)]
pub struct Reflection {
    pub kind: ReflectedKind,
    pub to: Surface,
    pub intended: Recipients,
    pub reflection_of: WaveId,
    pub track: bool,
}

impl Reflection {
    pub fn make(self, core: ReflectedCore, from: Surface) -> ReflectedWave {
        match self.kind {
            ReflectedKind::Pong => {
                let mut wave = Wave::new(
                    Pong::new(core, self.to, self.intended, self.reflection_of),
                    from,
                );
                wave.track = self.track;
                wave.to_reflected()
            }
            ReflectedKind::Echo => {
                let mut wave = Wave::new(
                    Echo::new(core, self.to, self.intended, self.reflection_of),
                    from,
                );
                wave.track = self.track;
                wave.to_reflected()
            }
        }
    }
}

impl<S> ToSubstance<S> for DirectedWave
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        match self {
            DirectedWave::Ping(ping) => ping.to_substance(),
            DirectedWave::Ripple(ripple) => ripple.to_substance(),
            DirectedWave::Signal(signal) => signal.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        match self {
            DirectedWave::Ping(ping) => ping.to_substance_ref(),
            DirectedWave::Ripple(ripple) => ripple.to_substance_ref(),
            DirectedWave::Signal(signal) => signal.to_substance_ref(),
        }
    }
}

impl DirectedWave {
    pub fn to_ultra(self) -> UltraWave {
        match self {
            DirectedWave::Ping(ping) => UltraWave::Ping(ping),
            DirectedWave::Ripple(ripple) => UltraWave::Ripple(ripple),
            DirectedWave::Signal(signal) => UltraWave::Signal(signal),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum ReflectedWave {
    Pong(Wave<Pong>),
    Echo(Wave<Echo>),
}

impl Trackable for ReflectedWave {
    fn track_id(&self) -> String {
        self.id().to_string()
    }

    fn track_method(&self) -> String {
        self.core().status.to_string()
    }

    fn track_payload(&self) -> String {
        self.core().body.kind().to_string()
    }

    fn track_from(&self) -> String {
        self.from().to_string()
    }

    fn track_to(&self) -> String {
        self.to().to_string()
    }

    fn track(&self) -> bool {
        match self {
            ReflectedWave::Pong(pong) => pong.track,
            ReflectedWave::Echo(echo) => echo.track,
        }
    }
}

impl<S> ToSubstance<S> for ReflectedWave
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        match self {
            ReflectedWave::Pong(pong) => pong.to_substance(),
            ReflectedWave::Echo(echo) => echo.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        match self {
            ReflectedWave::Pong(pong) => pong.to_substance_ref(),
            ReflectedWave::Echo(echo) => echo.to_substance_ref(),
        }
    }
}

pub trait ToReflected {
    fn to_reflected(self) -> ReflectedWave;
    fn from_reflected(reflected: ReflectedWave) -> Result<Self, SpaceErr>
    where
        Self: Sized;
}

impl ReflectedWave {
    pub fn from(&self) -> &Surface {
        match self {
            ReflectedWave::Pong(pong) => &pong.from,
            ReflectedWave::Echo(echo) => &echo.from,
        }
    }

    pub fn to(&self) -> &Surface {
        match self {
            ReflectedWave::Pong(pong) => &pong.to,
            ReflectedWave::Echo(echo) => &echo.to,
        }
    }

    pub fn id(&self) -> &WaveId {
        match self {
            ReflectedWave::Pong(pong) => &pong.id,
            ReflectedWave::Echo(echo) => &echo.id,
        }
    }

    pub fn to_ultra(self) -> UltraWave {
        match self {
            ReflectedWave::Pong(pong) => UltraWave::Pong(pong),
            ReflectedWave::Echo(echo) => UltraWave::Echo(echo),
        }
    }

    pub fn reflection_of(&self) -> &WaveId {
        match self {
            ReflectedWave::Pong(pong) => &pong.reflection_of,
            ReflectedWave::Echo(echo) => &echo.reflection_of,
        }
    }

    pub fn core(&self) -> &ReflectedCore {
        match self {
            ReflectedWave::Pong(pong) => &pong.core,
            ReflectedWave::Echo(echo) => &echo.core,
        }
    }

    pub fn to_echo(self) -> Result<Wave<Echo>, SpaceErr> {
        match self {
            ReflectedWave::Echo(echo) => Ok(echo),
            _ => Err(SpaceErr::bad_request("expected Wave to be an Echo")),
        }
    }

    pub fn to_pong(self) -> Result<Wave<Pong>, SpaceErr> {
        match self {
            ReflectedWave::Pong(pong) => Ok(pong),
            _ => Err(SpaceErr::bad_request("expecrted wave to be a Pong")),
        }
    }
}

impl ReflectedWave {
    pub fn is_success(&self) -> bool {
        match self {
            ReflectedWave::Pong(pong) => return pong.core.status.is_success(),
            ReflectedWave::Echo(echo) => return echo.core.status.is_success(),
        }
    }

    pub fn success_or(&self) -> Result<(), SpaceErr> {
        if self.is_success() {
            Ok(())
        } else {
            match self {
                ReflectedWave::Pong(pong) => Err(SpaceErr::Status {
                    status: pong.core.status.as_u16(),
                    message: "error".to_string(),
                }),
                ReflectedWave::Echo(echo) => Err(SpaceErr::Status {
                    status: echo.core.status.as_u16(),
                    message: "error".to_string(),
                }),
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Recipients {
    Single(Surface),
    Multi(Vec<Surface>),
    Watchers(Watch),
    Stars,
}

impl ToString for Recipients {
    fn to_string(&self) -> String {
        match self {
            Recipients::Single(surface) => surface.to_string(),
            Recipients::Multi(_) => "Multi".to_string(),
            Recipients::Watchers(_) => "Watchers".to_string(),
            Recipients::Stars => "Stars".to_string(),
        }
    }
}

impl ToRecipients for Recipients {
    fn to_recipients(self) -> Recipients {
        self
    }
}

impl Recipients {
    pub fn to_single(self) -> Result<Surface, SpaceErr> {
        match self {
            Recipients::Single(surface) => Ok(surface),
            _ => Err(SpaceErr::server_error(
                "cannot convert a multiple recipient into a single",
            )),
        }
    }
    pub fn is_match(&self, point: &Point) -> bool {
        match self {
            Recipients::Single(surface) => surface.point == *point,
            Recipients::Multi(ports) => {
                for port in ports {
                    if port.point == *point {
                        return true;
                    }
                }
                false
            }
            Recipients::Watchers(_) => false,
            Recipients::Stars => {
                if let RouteSeg::Star(_) = point.route {
                    if point.segments.len() == 1
                        && *point.segments.first().unwrap() == PointSeg::Base("star".to_string())
                    {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        }
    }

    pub fn split(map: HashMap<Point, Vec<Surface>>) -> HashMap<Point, Recipients> {
        let mut rtn = HashMap::new();
        for (point, value) in map {
            rtn.insert(point, Recipients::Multi(value));
        }
        rtn
    }
}

impl ToRecipients for &Recipients {
    fn to_recipients(self) -> Recipients {
        self.clone()
    }
}

pub trait ToRecipients {
    fn to_recipients(self) -> Recipients;
}

impl Recipients {
    pub fn select_ports(&self, point: &Point) -> Vec<&Surface> {
        let mut rtn = vec![];
        match self {
            Recipients::Single(surface) => {
                if surface.point == *point {
                    rtn.push(surface);
                }
            }
            Recipients::Multi(surfaces) => {
                for surface in surfaces {
                    if surface.point == *point {
                        rtn.push(surface);
                    }
                }
            }
            Recipients::Watchers(_) => {}
            Recipients::Stars => {}
        }
        rtn
    }

    pub fn is_single(&self) -> bool {
        match self {
            Recipients::Single(_) => true,
            Recipients::Multi(_) => false,
            Recipients::Watchers(_) => false,
            Recipients::Stars => false,
        }
    }

    pub fn is_multi(&self) -> bool {
        match self {
            Recipients::Single(_) => false,
            Recipients::Multi(_) => true,
            Recipients::Watchers(_) => false,
            Recipients::Stars => false,
        }
    }

    pub fn is_stars(&self) -> bool {
        match self {
            Recipients::Single(_) => false,
            Recipients::Multi(_) => false,
            Recipients::Watchers(_) => false,
            Recipients::Stars => true,
        }
    }

    pub fn is_watch(&self) -> bool {
        match self {
            Recipients::Single(_) => false,
            Recipients::Multi(_) => false,
            Recipients::Watchers(_) => true,
            Recipients::Stars => false,
        }
    }

    pub fn unwrap_single(self) -> Surface {
        self.single_or().expect("single")
    }

    pub fn single_or(self) -> Result<Surface, SpaceErr> {
        if let Recipients::Single(rtn) = self {
            Ok(rtn)
        } else {
            Err("not a single".into())
        }
    }
}

pub type IpAddr = String;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Origin {
    Ip(IpAddr),
    Point(Point),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Crypt<S> {
    pub payload: S,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct SessionId {
    pub origin: Crypt<Origin>,
    pub uuid: Uuid,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Wave<V> {
    pub id: WaveId,
    pub session: Option<SessionId>,
    pub variant: V,
    pub agent: Agent,
    pub handling: Handling,
    pub scope: Scope,
    pub from: Surface,
    pub via: Option<Surface>,
    pub hops: u16,
    pub track: bool,
}

impl<S, V> ToSubstance<S> for Wave<V>
where
    V: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.variant.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.variant.to_substance_ref()
    }
}

impl<V> Wave<V> {
    pub fn inc_hops(&mut self) {
        self.hops = self.hops + 1;
    }
}

impl Wave<Ripple> {
    pub fn to_ultra(self) -> UltraWave {
        UltraWave::Ripple(self)
    }

    pub fn to_directed(self) -> DirectedWave {
        DirectedWave::Ripple(self)
    }

    pub fn with_core(mut self, core: DirectedCore) -> Self {
        self.variant.core = core;
        self
    }
}

impl<T> Wave<RippleDef<T>>
where
    T: ToRecipients + Clone,
{
    pub fn err(&self, err: SpaceErr, responder: Surface) -> Wave<Echo> {
        Wave::new(
            Echo::new(
                self.variant.err(err),
                self.from.clone(),
                self.to.clone().to_recipients(),
                self.id.clone(),
            ),
            responder,
        )
    }
}

impl Trackable for Wave<Signal> {
    fn track_id(&self) -> String {
        self.id.to_short_string()
    }

    fn track_method(&self) -> String {
        self.method.to_deep_string()
    }

    fn track_payload(&self) -> String {
        self.core.body.kind().to_string()
    }

    fn track_from(&self) -> String {
        self.from.to_string()
    }

    fn track_to(&self) -> String {
        self.to.to_string()
    }

    fn track(&self) -> bool {
        self.track
    }

    fn track_payload_fmt(&self) -> String {
        match &self.core.body {
            Substance::UltraWave(wave) => {
                format!("UltraWave({})", wave.track_key_fmt())
            }
            _ => self.track_payload(),
        }
    }
}

impl Wave<Signal> {
    pub fn to_ultra(self) -> UltraWave {
        UltraWave::Signal(self)
    }

    pub fn to_directed(self) -> DirectedWave {
        DirectedWave::Signal(self)
    }

    pub fn with_core(mut self, core: DirectedCore) -> Self {
        self.variant.core = core;
        self
    }

    pub fn wrap_in_hop(self, from: Surface, to: Surface) -> DirectedProto {
        let mut signal = DirectedProto::signal();
        signal.from(from);
        signal.agent(self.agent.clone());
        signal.handling(self.handling.clone());
        signal.method(HypMethod::Hop);
        signal.track = self.track;
        signal.body(Substance::UltraWave(Box::new(self.to_ultra())));
        signal.to(to);
        signal
    }

    pub fn unwrap_from_hop(self) -> Result<Wave<Signal>, SpaceErr> {
        if self.method != Method::Hyp(HypMethod::Hop) {
            return Err(SpaceErr::server_error(
                "expected signal wave to have method Hop",
            ));
        }
        if let Substance::UltraWave(wave) = &self.body {
            Ok((*wave.clone()).to_signal()?)
        } else {
            Err(SpaceErr::server_error(
                "expected body substance to be of type UltraWave for a transport signal",
            ))
        }
    }

    pub fn unwrap_from_transport(self) -> Result<UltraWave, SpaceErr> {
        if self.method != Method::Hyp(HypMethod::Transport) {
            return Err(SpaceErr::server_error(
                "expected signal wave to have method Transport",
            ));
        }
        if let Substance::UltraWave(wave) = &self.body {
            Ok(*wave.clone())
        } else {
            Err(SpaceErr::server_error(
                "expected body substance to be of type UltraWave for a transport signal",
            ))
        }
    }

    pub fn to_singular_directed(self) -> SingularDirectedWave {
        SingularDirectedWave::Signal(self)
    }
}

impl<S> ToSubstance<S> for Signal
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.core.to_substance_ref()
    }
}

impl Trackable for Wave<Ping> {
    fn track_id(&self) -> String {
        self.id.to_short_string()
    }

    fn track_method(&self) -> String {
        self.method.to_deep_string()
    }

    fn track_payload(&self) -> String {
        self.core.body.kind().to_string()
    }

    fn track_from(&self) -> String {
        self.from.to_string()
    }

    fn track_to(&self) -> String {
        self.to.to_string()
    }

    fn track(&self) -> bool {
        self.track
    }

    fn track_payload_fmt(&self) -> String {
        match &self.core.body {
            Substance::UltraWave(wave) => {
                format!("UltraWave({})", wave.track_key_fmt())
            }
            _ => self.track_payload(),
        }
    }
}

impl Wave<Ping> {
    pub fn to_ultra(self) -> UltraWave {
        UltraWave::Ping(self)
    }

    pub fn to_directed(self) -> DirectedWave {
        DirectedWave::Ping(self)
    }

    pub fn err(&self, err: SpaceErr, responder: Surface) -> Wave<Pong> {
        Wave::new(
            Pong::new(
                self.variant.err(err),
                self.from.clone(),
                self.to.clone().to_recipients(),
                self.id.clone(),
            ),
            responder,
        )
    }

    pub fn bounce_backs(&self) -> BounceBacks {
        BounceBacks::Single
    }
}

impl Wave<Pong> {
    pub fn to_ultra(self) -> UltraWave {
        UltraWave::Pong(self)
    }

    pub fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Pong(self)
    }
}

impl ToReflected for Wave<Pong> {
    fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Pong(self)
    }

    fn from_reflected(reflected: ReflectedWave) -> Result<Self, SpaceErr> {
        match reflected {
            ReflectedWave::Pong(pong) => Ok(pong),
            _ => Err(SpaceErr::bad_request("expected wave to be a Pong")),
        }
    }
}

impl ToReflected for Wave<Echo> {
    fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Echo(self)
    }

    fn from_reflected(reflected: ReflectedWave) -> Result<Self, SpaceErr> {
        match reflected {
            ReflectedWave::Echo(echo) => Ok(echo),
            _ => Err(SpaceErr::bad_request("expected Wave to be an Echo")),
        }
    }
}

impl Wave<Echo> {
    pub fn to_ultra(self) -> UltraWave {
        UltraWave::Echo(self)
    }

    pub fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Echo(self)
    }
}

impl TryFrom<ReflectedWave> for Wave<Pong> {
    type Error = SpaceErr;

    fn try_from(wave: ReflectedWave) -> Result<Self, Self::Error> {
        match wave {
            ReflectedWave::Pong(pong) => Ok(pong),
            _ => Err(SpaceErr::bad_request("Expected Wave to be a Pong")),
        }
    }
}

impl<V> Wave<V> {
    pub fn new(variant: V, from: Surface) -> Self
    where
        V: WaveVariant,
    {
        Self {
            id: WaveId::new(variant.kind().clone()),
            session: None,
            agent: Default::default(),
            handling: Default::default(),
            scope: Default::default(),
            variant,
            from,
            hops: 0,
            track: false,
            via: None,
        }
    }

    pub fn replace<V2>(self, variant: V2) -> Wave<V2>
    where
        V2: WaveVariant,
    {
        Wave {
            id: self.id,
            session: self.session,
            agent: self.agent,
            handling: self.handling,
            scope: self.scope,
            variant,
            from: self.from,
            hops: self.hops,
            track: false,
            via: self.via,
        }
    }
}

pub trait WaveVariant {
    fn kind(&self) -> WaveKind;
}

impl WaveVariant for Ping {
    fn kind(&self) -> WaveKind {
        WaveKind::Ping
    }
}

impl WaveVariant for Pong {
    fn kind(&self) -> WaveKind {
        WaveKind::Pong
    }
}

impl<T> WaveVariant for RippleDef<T>
where
    T: ToRecipients + Clone,
{
    fn kind(&self) -> WaveKind {
        WaveKind::Ripple
    }
}

impl WaveVariant for Echo {
    fn kind(&self) -> WaveKind {
        WaveKind::Echo
    }
}

impl Wave<Ping> {
    pub fn pong(&self) -> ReflectedProto {
        let mut pong = ReflectedProto::new();
        pong.kind(ReflectedKind::Pong);
        pong.fill(self);
        pong
    }
}

impl<T> Wave<RippleDef<T>>
where
    T: ToRecipients + Clone,
{
    pub fn echo(&self) -> ReflectedProto {
        let mut echo = ReflectedProto::new();
        echo.kind(ReflectedKind::Echo);
        echo.fill(self);
        echo
    }

    pub fn bounce_backs(&self) -> BounceBacks {
        self.bounce_backs.clone()
    }
}

impl Wave<SingularRipple> {
    pub fn to_singular_directed(self) -> SingularDirectedWave {
        SingularDirectedWave::Ripple(self)
    }
}

impl DirectedWave {
    pub fn reflected_proto(&self) -> BounceProto {
        match self {
            DirectedWave::Ping(ping) => BounceProto::Reflected(ping.pong()),
            DirectedWave::Ripple(ripple) => BounceProto::Reflected(ripple.echo()),
            DirectedWave::Signal(_) => BounceProto::Absorbed,
        }
    }
}

impl<V> Deref for Wave<V> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.variant
    }
}

impl<V> DerefMut for Wave<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.variant
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Agent {
    Anonymous,
    HyperUser,
    Point(Point),
}

impl ToPoint for Agent {
    fn to_point(&self) -> Point {
        match self {
            Agent::Anonymous => ANONYMOUS.clone(),
            Agent::HyperUser => HYPERUSER.clone(),
            Agent::Point(point) => point.clone(),
        }
    }
}

impl Default for Agent {
    fn default() -> Self {
        Self::Anonymous
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub id: Uuid,
    pub attributes: HashMap<String, String>,
}

impl Session {
    pub fn new() -> Self {
        Self {
            id: uuid(),
            attributes: HashMap::new(),
        }
    }
    pub fn get_preferred_username(&self) -> Option<String> {
        self.attributes
            .get(&"preferred_username".to_string())
            .cloned()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Scope {
    Full,
    None,
    Grants(HashSet<ScopeGrant>),
}

impl Scope {
    pub fn has_grant(&self, grant: &ScopeGrant) -> Result<(), ()> {
        match self {
            Scope::Full => Ok(()),
            Scope::None => Err(()),
            Scope::Grants(grants) if grants.contains(grant) => Ok(()),
            _ => Err(()),
        }
    }

    pub fn enumerated_grants(&self) -> HashSet<ScopeGrant> {
        match self {
            Scope::Full => HashSet::new(),
            Scope::None => HashSet::new(),
            Scope::Grants(grants) => grants.clone(),
        }
    }
}

impl From<HashSet<ScopeGrant>> for Scope {
    fn from(grants: HashSet<ScopeGrant>) -> Self {
        Scope::Grants(grants)
    }
}

impl ops::BitAnd<Scope> for Scope {
    type Output = Scope;

    fn bitand(self, rhs: Scope) -> Self::Output {
        if self == Self::Full && rhs == Self::Full {
            Self::Full
        } else if self == Self::None || rhs == Self::None {
            Self::None
        } else {
            let mut grants = self.enumerated_grants();
            grants.retain(|grant| rhs.has_grant(grant).is_ok());
            grants.into()
        }
    }
}

impl ops::BitOr<Scope> for Scope {
    type Output = Scope;

    fn bitor(self, rhs: Scope) -> Self::Output {
        if self == Self::Full || rhs == Scope::Full {
            Self::Full
        } else {
            let left = self.enumerated_grants();
            let right = rhs.enumerated_grants();
            Self::Grants(left.union(&right).cloned().collect())
        }
    }
}

impl Scope {
    /*
    pub fn mask( &self, on: &AddressKindPath ) -> Access {
        match self {
            Scope::Full => {
                access.clone()
            }
            Scope::None => {
                Access::none()
            }
            Scope::Grants(grants) => {
                let mut access  = access.clone();
                let mut privileges = EnumeratedPrivileges::none();
                let mut permissions = Permissions::full();
                for grant in grants {
                   if grant.on.matches(on) {
                       match &grant.aspect {
                           ScopeGrantAspect::Perm(and) => permissions.and(and),
                           ScopeGrantAspect::Priv(and) =>  privileges.insert(and.clone())
                       }
                   }
               }
            }
        }
    }

     */
}

impl Default for Scope {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct ScopeGrant {
    pub on: PointSelector,
    pub kind: ScopeGrantKind,
    pub aspect: ScopeGrantAspect,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum ScopeGrantKind {
    Or,
    And,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum ScopeGrantAspect {
    Perm(Permissions),
    Priv(Privilege),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Handling {
    pub kind: HandlingKind,
    pub priority: Priority,
    pub retries: Retries,
    pub wait: WaitTime,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum HandlingKind {
    Durable,   // Mesh will guarantee delivery eventually once Request call has returned
    Queued,    // Slower but more reliable delivery, message can be lost if a star crashes, etc
    Immediate, // Wave should never touch a filesystem, it will be in memory for its entire journey for immediate processing
}

impl Default for Handling {
    fn default() -> Self {
        Self {
            kind: HandlingKind::Queued,
            priority: Default::default(),
            retries: Default::default(),
            wait: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum WaitTime {
    High,
    Med,
    Low,
}

impl Default for WaitTime {
    fn default() -> Self {
        WaitTime::Low
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Retries {
    None,
    Max,
    Medium,
    Min,
}

impl Default for Retries {
    fn default() -> Self {
        Retries::None
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Priority {
    Hyper,
    Super,
    High,
    Med,
    Low,
}

impl Default for Priority {
    fn default() -> Self {
        Self::Med
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Karma {
    Hyper,
    Super,
    High,
    Med,
    Low,
    None,
}

impl Default for Karma {
    fn default() -> Self {
        Self::High
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Urgency {
    High,
    Low,
}

pub trait TransportPlanner {
    fn dest(&self, surface: Surface) -> Surface;
}

pub enum Bounce<W> {
    Absorbed,
    Reflected(W),
}

impl<W> Bounce<W> {
    pub fn to_core_bounce(self) -> CoreBounce
    where
        W: TryInto<ReflectedCore, Error = SpaceErr>,
    {
        match self {
            Bounce::Absorbed => Bounce::Absorbed,
            Bounce::Reflected(reflected) => match reflected.try_into() {
                Ok(reflected) => CoreBounce::Reflected(reflected),
                Err(err) => CoreBounce::Reflected(err.as_reflected_core()),
            },
        }
    }
}

impl Into<CoreBounce> for Bounce<ReflectedWave> {
    fn into(self) -> CoreBounce {
        match self {
            Bounce::Absorbed => CoreBounce::Absorbed,
            Bounce::Reflected(reflected) => CoreBounce::Reflected(reflected.core().clone()),
        }
    }
}

pub enum BounceProto {
    Absorbed,
    Reflected(ReflectedProto),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReflectedAggregate {
    None,
    Single(ReflectedWave),
    Multi(Vec<ReflectedWave>),
}

pub trait FromReflectedAggregate {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> Result<Self, SpaceErr>
    where
        Self: Sized;
}

impl TryInto<Wave<Pong>> for ReflectedAggregate {
    type Error = SpaceErr;
    fn try_into(self) -> Result<Wave<Pong>, Self::Error> {
        match self {
            Self::Single(reflected) => match reflected {
                ReflectedWave::Pong(pong) => Ok(pong),
                _ => Err(SpaceErr::bad_request(
                    "Expected ReflectedAggregate to be for a Pong",
                )),
            },
            _ => Err(SpaceErr::bad_request(
                "Expected ReflectedAggregate to be a Single",
            )),
        }
    }
}

impl TryInto<Vec<Wave<Echo>>> for ReflectedAggregate {
    type Error = SpaceErr;
    fn try_into(self) -> Result<Vec<Wave<Echo>>, Self::Error> {
        match self {
            Self::Single(reflected) => match reflected {
                ReflectedWave::Echo(echo) => Ok(vec![echo]),
                _ => Err(SpaceErr::bad_request(
                    "Expected Reflected to be a Single Echo",
                )),
            },
            ReflectedAggregate::None => Ok(vec![]),
            ReflectedAggregate::Multi(waves) => {
                let mut echoes = vec![];
                for w in waves {
                    echoes.push(w.to_echo()?);
                }
                Ok(echoes)
            }
        }
    }
}

#[derive(Clone)]
pub struct Delivery {
    pub to: Surface,
    pub wave: DirectedWave,
}

impl Delivery {
    pub fn new(to: Surface, wave: DirectedWave) -> Self {
        Self { to, wave }
    }
}

impl Deref for Delivery {
    type Target = DirectedWave;

    fn deref(&self) -> &Self::Target {
        &self.wave
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct HyperWave {
    point: Point,
    wave: UltraWave,
}
