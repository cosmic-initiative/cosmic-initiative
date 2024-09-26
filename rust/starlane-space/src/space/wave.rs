use ::core::borrow::Borrow;
use std::collections::{HashMap, HashSet};
use std::ops;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use crate::space::command::RawCommand;
use crate::space::err::{SpaceErr};
use crate::space::loc::{Surface, ToPoint, ToSurface, Uuid};
use crate::space::log::{
    Spannable, Trackable, TrailSpanId,
};
use crate::space::parse::model::Subst;
use crate::space::particle::Watch;
use crate::space::point::{Point, PointSeg, RouteSeg};
use crate::space::security::{Permissions, Privilege};
use crate::space::selector::Selector;
use crate::space::substance::Bin;
use crate::space::substance::{
    Call, CallKind, CmdCall, ExtCall, HttpCall, HypCall, Substance
    , ToRequestCore, ToSubstance,
};
use crate::space::util::{uuid, ValueMatcher};
use crate::space::wave::core::http2::StatusCode;
use url::Url;
use crate::space::{ANONYMOUS, HYPERUSER};
use self::core::cmd::CmdMethod;
use self::core::ext::ExtMethod;
use self::core::http2::HttpMethod;
use self::core::hyp::HypMethod;
use self::core::{CoreBounce, CoreReflector, DirectedCore, Method, ReflectedCore};

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
    pub fn reflected_kind(&self) -> anyhow::Result<ReflectedKind> {
        match self {
            WaveKind::Pong => Ok(ReflectedKind::Pong),
            WaveKind::Echo => Ok(ReflectedKind::Echo),
            _ => Err(SpaceErr::bad_request("expected a reflected WaveKind")),
        }
    }
}

pub type Wave = WaveDef<Recipients>;
pub type SingularWave = WaveDef<Surface>;

impl SingularWave {
    pub fn to_wave(self) -> anyhow::Result<Wave> {
        match self {
            SingularWave::Ping(ping) => Ok(Wave::Ping(ping)),
            SingularWave::Pong(pong) => Ok(Wave::Pong(pong)),
            SingularWave::Echo(echo) => Ok(Wave::Echo(echo)),
            SingularWave::Signal(signal) => Ok(Wave::Signal(signal)),
            SingularWave::Ripple(ripple) => {
                let ripple = ripple.to_multiple();
                Ok(Wave::Ripple(ripple))
            }
        }
    }
}

pub type Ping = WaveVariantDef<PingCore>;
pub type Pong = WaveVariantDef<PongCore>;
pub type Echo = WaveVariantDef<EchoCore>;
pub type Signal  = WaveVariantDef<SignalCore>;


#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum WaveDef<T>
where
    T: ToRecipients + Clone,
{
    Ping(WaveVariantDef<PingCore>),
    Pong(WaveVariantDef<PongCore>),
    Ripple(WaveVariantDef<RippleCoreDef<T>>),
    Echo(WaveVariantDef<EchoCore>),
    Signal(WaveVariantDef<SignalCore>),
}

impl<W> Spannable for WaveDef<W>
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

impl Trackable for Wave {
    fn track_id(&self) -> String {
        self.id().to_short_string()
    }

    fn track_method(&self) -> String {
        match self {
            Wave::Ping(ping) => ping.core.method.to_deep_string(),
            Wave::Pong(pong) => pong.core.status.to_string(),
            Wave::Ripple(ripple) => ripple.core.method.to_deep_string(),
            Wave::Echo(echo) => echo.core.status.to_string(),
            Wave::Signal(signal) => signal.core.method.to_deep_string(),
        }
    }

    fn track_payload(&self) -> String {
        match self {
            Wave::Ping(ping) => ping.core.body.kind().to_string(),
            Wave::Pong(pong) => pong.core.body.kind().to_string(),
            Wave::Ripple(ripple) => ripple.core.body.kind().to_string(),
            Wave::Echo(echo) => echo.core.body.kind().to_string(),
            Wave::Signal(signal) => signal.core.body.kind().to_string(),
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
            Wave::Ping(ping) => ping.track,
            Wave::Pong(pong) => pong.track,
            Wave::Ripple(ripple) => ripple.track,
            Wave::Echo(echo) => echo.track,
            Wave::Signal(signal) => signal.track,
        }
    }
    fn track_payload_fmt(&self) -> String {
        match self {
            Wave::Signal(signal) => signal.track_payload_fmt(),
            Wave::Ping(_) => self.track_payload(),
            Wave::Pong(_) => self.track_payload(),
            Wave::Ripple(_) => self.track_payload(),
            Wave::Echo(_) => self.track_payload(),
        }
    }
}

impl<T> WaveDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn via_desc(&self) -> String {
        let via = match self {
            WaveDef::Ping(w) => w.via.as_ref(),
            WaveDef::Pong(w) => w.via.as_ref(),
            WaveDef::Ripple(w) => w.via.as_ref(),
            WaveDef::Echo(w) => w.via.as_ref(),
            WaveDef::Signal(w) => w.via.as_ref(),
        };

        match via {
            None => "None".to_string(),
            Some(via) => via.to_string(),
        }
    }

    pub fn has_visited(&self, star: &Point) -> bool {
        match self {
            WaveDef::Ripple(ripple) => ripple.history.contains(star),
            _ => false,
        }
    }

    pub fn add_history(&mut self, point: &Point) {
        match self {
            WaveDef::Ping(_) => {}
            WaveDef::Pong(_) => {}
            WaveDef::Ripple(ripple) => {
                ripple.history.insert(point.clone());
            }
            WaveDef::Echo(_) => {}
            WaveDef::Signal(_) => {}
        }
    }

    pub fn history(&self) -> HashSet<Point> {
        match self {
            WaveDef::Ping(_) => HashSet::new(),
            WaveDef::Pong(_) => HashSet::new(),
            WaveDef::Ripple(ripple) => ripple.history.clone(),
            WaveDef::Echo(_) => HashSet::new(),
            WaveDef::Signal(_) => HashSet::new(),
        }
    }

    pub fn id(&self) -> WaveId {
        match self {
            WaveDef::Ping(w) => w.id.clone(),
            WaveDef::Pong(w) => w.id.clone(),
            WaveDef::Ripple(w) => w.id.clone(),
            WaveDef::Echo(w) => w.id.clone(),
            WaveDef::Signal(w) => w.id.clone(),
        }
    }

    pub fn body(&self) -> &Substance {
        match self {
            WaveDef::Ping(ping) => &ping.body,
            WaveDef::Pong(pong) => &pong.core.body,
            WaveDef::Ripple(ripple) => &ripple.body,
            WaveDef::Echo(echo) => &echo.core.body,
            WaveDef::Signal(signal) => &signal.core.body,
        }
    }

    pub fn payload(&self) -> Option<&Wave> {
        if let Substance::Wave(wave) = self.body() {
            Some(wave)
        } else {
            None
        }
    }
}

impl Wave {
    pub fn can_shard(&self) -> bool {
        match self {
            Wave::Ripple(_) => true,
            _ => false,
        }
    }

    pub fn to_singular(self) -> anyhow::Result<SingularWave> {
        match self {
            Wave::Ping(ping) => Ok(SingularWave::Ping(ping)),
            Wave::Pong(pong) => Ok(SingularWave::Pong(pong)),
            Wave::Echo(echo) => Ok(SingularWave::Echo(echo)),
            Wave::Signal(signal) => Ok(SingularWave::Signal(signal)),
            Wave::Ripple(_) => Err(err(
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
        signal.body(Substance::Wave(Box::new(self)));
        signal.to(to);
        signal
    }

    pub fn unwrap_from_hop(self) -> anyhow::Result<WaveVariantDef<SignalCore>> {
        let signal = self.to_signal()?;
        signal.unwrap_from_hop()
    }

    pub fn unwrap_from_transport(self) -> anyhow::Result<Wave> {
        let signal = self.to_signal()?;
        signal.unwrap_from_transport()
    }

    pub fn to_substance(self) -> Substance {
        Substance::Wave(Box::new(self))
    }

    pub fn to_directed(self) -> anyhow::Result<DirectedWave> {
        match self {
            Wave::Ping(ping) => Ok(ping.to_directed()),
            Wave::Ripple(ripple) => Ok(ripple.to_directed()),
            Wave::Signal(signal) => Ok(signal.to_directed()),
            _ => Err(SpaceErr::bad_request("expected a DirectedWave")),
        }
    }

    pub fn to_reflected(self) -> anyhow::Result<ReflectedWave> {
        match self {
            Wave::Pong(pong) => Ok(pong.to_reflected()),
            Wave::Echo(echo) => Ok(echo.to_reflected()),
            _ => Err(SpaceErr::bad_request(format!(
                "expected: ReflectedWave; encountered: {}",
                self.desc()
            ))),
        }
    }

    pub fn kind(&self) -> WaveKind {
        match self {
            Wave::Ping(_) => WaveKind::Ping,
            Wave::Pong(_) => WaveKind::Pong,
            Wave::Ripple(_) => WaveKind::Ripple,
            Wave::Echo(_) => WaveKind::Echo,
            Wave::Signal(_) => WaveKind::Signal,
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
            Wave::Ping(w) => w.hops,
            Wave::Pong(w) => w.hops,
            Wave::Ripple(w) => w.hops,
            Wave::Echo(w) => w.hops,
            Wave::Signal(w) => w.hops,
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
            Wave::Ping(w) => w.hops += 1,
            Wave::Pong(w) => w.hops += 1,
            Wave::Ripple(w) => w.hops += 1,
            Wave::Echo(w) => w.hops += 1,
            Wave::Signal(w) => w.hops += 1,
        };

        if let Some(wave) = self.transported_mut() {
            wave.inc_hops();
        }
    }

    pub fn add_to_history(&mut self, star: Point) {
        match self {
            Wave::Ripple(ripple) => {
                ripple.history.insert(star);
            }
            _ => {}
        }
    }

    pub fn to_signal(self) -> anyhow::Result<WaveVariantDef<SignalCore>> {
        match self {
            Wave::Signal(signal) => Ok(signal),
            _ => Err(SpaceErr::bad_request(format!(
                "expecting: Wave<Signal> encountered: Wave<{}>",
                self.kind().to_string()
            ))),
        }
    }

    pub fn method(&self) -> Option<&Method> {
        match self {
            Wave::Ping(ping) => Some(&ping.method),
            Wave::Ripple(ripple) => Some(&ripple.method),
            Wave::Signal(signal) => Some(&signal.method),
            _ => None,
        }
    }

    pub fn is_directed(&self) -> bool {
        match self {
            Wave::Ping(_) => true,
            Wave::Pong(_) => false,
            Wave::Ripple(_) => true,
            Wave::Echo(_) => false,
            Wave::Signal(_) => true,
        }
    }

    pub fn is_reflected(&self) -> bool {
        match self {
            Wave::Ping(_) => false,
            Wave::Pong(_) => true,
            Wave::Ripple(_) => false,
            Wave::Echo(_) => true,
            Wave::Signal(_) => false,
        }
    }

    pub fn to(&self) -> Recipients {
        match self {
            Wave::Ping(ping) => ping.to.clone().to_recipients(),
            Wave::Pong(pong) => pong.to.clone().to_recipients(),
            Wave::Ripple(ripple) => ripple.to.clone(),
            Wave::Echo(echo) => echo.to.clone().to_recipients(),
            Wave::Signal(signal) => signal.to.clone().to_recipients(),
        }
    }

    pub fn from(&self) -> &Surface {
        match self {
            Wave::Ping(ping) => &ping.from,
            Wave::Pong(pong) => &pong.from,
            Wave::Ripple(ripple) => &ripple.from,
            Wave::Echo(echo) => &echo.from,
            Wave::Signal(signal) => &signal.from,
        }
    }

    pub fn set_agent(&mut self, agent: Agent) {
        match self {
            Wave::Ping(ping) => ping.agent = agent,
            Wave::Pong(pong) => pong.agent = agent,
            Wave::Ripple(ripple) => ripple.agent = agent,
            Wave::Echo(echo) => echo.agent = agent,
            Wave::Signal(signal) => signal.agent = agent,
        }
    }

    pub fn set_to(&mut self, to: Surface) {
        match self {
            Wave::Ping(ping) => ping.to = to,
            Wave::Pong(pong) => pong.to = to,
            Wave::Ripple(ripple) => ripple.to = to.to_recipients(),
            Wave::Echo(echo) => echo.to = to,
            Wave::Signal(signal) => signal.to = to,
        }
    }

    pub fn set_from(&mut self, from: Surface) {
        match self {
            Wave::Ping(ping) => ping.from = from,
            Wave::Pong(pong) => pong.from = from,
            Wave::Ripple(ripple) => ripple.from = from,
            Wave::Echo(echo) => echo.from = from,
            Wave::Signal(signal) => signal.from = from,
        }
    }

    pub fn agent(&self) -> &Agent {
        match self {
            Wave::Ping(ping) => &ping.agent,
            Wave::Pong(pong) => &pong.agent,
            Wave::Ripple(ripple) => &ripple.agent,
            Wave::Echo(echo) => &echo.agent,
            Wave::Signal(signal) => &signal.agent,
        }
    }

    pub fn handling(&self) -> &Handling {
        match self {
            Wave::Ping(ping) => &ping.handling,
            Wave::Pong(pong) => &pong.handling,
            Wave::Ripple(ripple) => &ripple.handling,
            Wave::Echo(echo) => &echo.handling,
            Wave::Signal(signal) => &signal.handling,
        }
    }

    pub fn track(&self) -> bool {
        match self {
            Wave::Ping(ping) => ping.track,
            Wave::Pong(pong) => pong.track,
            Wave::Ripple(ripple) => ripple.track,
            Wave::Echo(echo) => echo.track,
            Wave::Signal(signal) => signal.track,
        }
    }

    pub fn set_track(&mut self, track: bool) {
        match self {
            Wave::Ping(ping) => ping.track = track,
            Wave::Pong(pong) => pong.track = track,
            Wave::Ripple(ripple) => ripple.track = track,
            Wave::Echo(echo) => echo.track = track,
            Wave::Signal(signal) => signal.track = track,
        }
    }

    pub fn scope(&self) -> &Scope {
        match self {
            Wave::Ping(ping) => &ping.scope,
            Wave::Pong(pong) => &pong.scope,
            Wave::Ripple(ripple) => &ripple.scope,
            Wave::Echo(echo) => &echo.scope,
            Wave::Signal(signal) => &signal.scope,
        }
    }
    pub fn to_ripple(self) -> anyhow::Result<WaveVariantDef<Ripple>> {
        match self {
            Wave::Ripple(ripple) => Ok(ripple),
            _ => Err("not a ripple".into()),
        }
    }

    pub fn transported(&self) -> Option<&Wave> {
        match self {
            Wave::Ping(w) => w.core.body.wave(),
            Wave::Pong(w) => w.core.body.wave(),
            Wave::Ripple(w) => w.core.body.wave(),
            Wave::Echo(w) => w.core.body.wave(),
            Wave::Signal(w) => w.core.body.wave(),
        }
    }

    pub fn transported_mut(&mut self) -> Option<&mut Wave> {
        match self {
            Wave::Ping(w) => w.core.body.wave_mut(),
            Wave::Pong(w) => w.core.body.wave_mut(),
            Wave::Ripple(w) => w.core.body.wave_mut(),
            Wave::Echo(w) => w.core.body.wave_mut(),
            Wave::Signal(w) => w.core.body.wave_mut(),
        }
    }
}

impl<S> ToSubstance<S> for Wave
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        match self {
            Wave::Ping(ping) => ping.to_substance(),
            Wave::Pong(pong) => pong.to_substance(),
            Wave::Ripple(ripple) => ripple.to_substance(),
            Wave::Echo(echo) => echo.to_substance(),
            Wave::Signal(signal) => signal.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        match self {
            Wave::Ping(ping) => ping.to_substance_ref(),
            Wave::Pong(pong) => pong.to_substance_ref(),
            Wave::Ripple(ripple) => ripple.to_substance_ref(),
            Wave::Echo(echo) => echo.to_substance_ref(),
            Wave::Signal(signal) => signal.to_substance_ref(),
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

    fn result<C: Into<ReflectedCore>>(self, result: anyhow::Result<C>, responder: Surface) -> R
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

pub type Ripple = RippleCoreDef<Recipients>;
pub type SingularRipple = RippleCoreDef<Surface>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RippleCoreDef<T: ToRecipients + Clone> {
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

impl<T> RippleCoreDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn replace_to<T2: ToRecipients + Clone>(self, to: T2) -> RippleCoreDef<T2> {
        RippleCoreDef {
            to,
            core: self.core,
            bounce_backs: self.bounce_backs,
            history: self.history,
        }
    }
}

impl WaveVariantDef<SingularRipple> {
    pub fn to_singular_ultra(self) -> SingularWave {
        SingularWave::Ripple(self)
    }

    pub fn to_multiple(self) -> WaveVariantDef<Ripple> {
        let ripple = self
            .variant
            .clone()
            .replace_to(self.variant.to.clone().to_recipients());
        self.replace(ripple)
    }
}

impl WaveVariantDef<SingularRipple> {
    pub fn as_multi(&self, recipients: Recipients) -> WaveVariantDef<Ripple> {
        let ripple = self.variant.clone().replace_to(recipients);
        self.clone().replace(ripple)
    }
}

impl WaveVariantDef<Ripple> {
    pub fn as_single(&self, surface: Surface) -> WaveVariantDef<SingularRipple> {
        let ripple = self.variant.clone().replace_to(surface);
        self.clone().replace(ripple)
    }

    pub fn to_singular_directed(self) -> anyhow::Result<SingularDirectedWave> {
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

impl<S, T> ToSubstance<S> for RippleCoreDef<T>
where
    Substance: ToSubstance<S>,
    T: ToRecipients + Clone,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.core.to_substance_ref()
    }
}

impl<T> RippleCoreDef<T>
where
    T: ToRecipients + Clone,
{
    pub fn require_method<M: Into<Method> + ToString + Clone>(
        self,
        method: M,
    ) -> anyhow::Result<RippleCoreDef<T>> {
        if self.core.method == method.clone().into() {
            Ok(self)
        } else {
            Err(SpaceErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            ))
        }
    }

    pub fn require_body<B>(self) -> anyhow::Result<B>
    where
        B: TryFrom<Substance, Error = SpaceErr>,
    {
        match B::try_from(self.body.clone()) {
            Ok(body) => Ok(body),
            Err(err) => Err(SpaceErr::bad_request("expected a body")),
        }
    }
}

impl<T> Deref for RippleCoreDef<T>
where
    T: ToRecipients + Clone,
{
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl<T> DerefMut for RippleCoreDef<T>
where
    T: ToRecipients + Clone,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct SignalCore {
    pub to: Surface,
    pub core: DirectedCore,
}

impl WaveVariant for SignalCore {
    fn kind(&self) -> WaveKind {
        WaveKind::Signal
    }
}

impl SignalCore {
    pub fn new(to: Surface, core: DirectedCore) -> Self {
        Self { to, core }
    }

    pub fn bounce_backs(&self) -> BounceBacks {
        BounceBacks::None
    }
}

impl Deref for SignalCore {
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl DerefMut for SignalCore {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PingCore {
    pub to: Surface,
    pub core: DirectedCore,
}

impl WaveVariantDef<PingCore> {
    pub fn to_singular_directed(self) -> SingularDirectedWave {
        SingularDirectedWave::Ping(self)
    }

    pub fn with_core(mut self, core: DirectedCore) -> Self {
        self.variant.core = core;
        self
    }
}

impl<S> ToSubstance<S> for PingCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.core.to_substance_ref()
    }
}

impl Deref for PingCore {
    type Target = DirectedCore;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl DerefMut for PingCore {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

impl Into<DirectedProto> for WaveVariantDef<PingCore> {
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

impl PingCore {
    pub fn require_method<M: Into<Method> + ToString + Clone>(
        self,
        method: M,
    ) -> anyhow::Result<PingCore> {
        if self.core.method == method.clone().into() {
            Ok(self)
        } else {
            Err(SpaceErr::new(
                400,
                format!("Bad Request: expecting method: {}", method.to_string()).as_str(),
            ))
        }
    }

    pub fn require_body<B>(self) -> anyhow::Result<B>
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
    pub wave: WaveVariantDef<V>,
    pub session: Session,
}

impl<V> WaveXtra<V> {
    pub fn new(wave: WaveVariantDef<V>, session: Session) -> Self {
        Self { wave, session }
    }
}

impl TryFrom<PingCore> for RawCommand {
    type Error = SpaceErr;

    fn try_from(request: PingCore) -> Result<Self, Self::Error> {
        request.core.body.try_into()
    }
}

impl TryFrom<PongCore> for Substance {
    type Error = SpaceErr;

    fn try_from(response: PongCore) -> Result<Self, Self::Error> {
        Ok(response.core.body)
    }
}

impl TryInto<Bin> for PongCore {
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

impl PingCore {
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

    pub fn fill<V>(&mut self, wave: &WaveVariantDef<V>) {
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

    pub fn body(&mut self, body: Substance) -> anyhow::Result<()> {
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

    pub fn build(self) -> anyhow::Result<ReflectedWave> {
        let mut core = ReflectedCore::new();
        core.body = self.body.or_else(|| Some(Substance::Empty)).unwrap();
        core.status = self
            .status
            .or_else(|| Some(StatusCode::from_u16(200u16).unwrap()))
            .unwrap();
        match self.kind.ok_or("missing ReflectedWave Kind")? {
            ReflectedKind::Pong => {
                let mut pong = WaveVariantDef::new(
                    PongCore::new(
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
                let mut echo = WaveVariantDef::new(
                    EchoCore::new(
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
    pub fn build(self) -> anyhow::Result<DirectedWave> {
        let kind = self.kind.ok_or::<SpaceErr>(
            "kind must be set for DirectedProto to create the proper DirectedWave".into(),
        )?;

        let mut core = self.core.clone();
        if let Some(method) = self.method {
            core.method = method;
        }

        let mut wave = match kind {
            DirectedKind::Ping => {
                let mut wave = WaveVariantDef::new(
                    PingCore {
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
                let mut wave = WaveVariantDef::new(
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
                let mut wave = WaveVariantDef::new(
                    SignalCore {
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

    pub fn fill(&mut self, wave: &Wave) {
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

    pub fn core(&mut self, core: DirectedCore) -> anyhow::Result<()> {
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

pub type Echoes = Vec<WaveVariantDef<EchoCore>>;

impl FromReflectedAggregate for () {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> anyhow::Result<Self>
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
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        match agg {
            ReflectedAggregate::Multi(reflected) => {
                let mut echoes = Echoes::new();
                for r in reflected {
                    let echo: WaveVariantDef<EchoCore> = r.to_echo()?;
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
pub struct EchoCore {
    /// this is meant to be the intended request recipient, which may not be the point responding
    /// to this message in the case it was intercepted and filtered at some point
    pub to: Surface,
    pub intended: Recipients,
    pub core: ReflectedCore,
    pub reflection_of: WaveId,
}

impl<S> ToSubstance<S> for EchoCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.core.to_substance_ref()
    }
}

impl EchoCore {
    pub fn is_ok(&self) -> bool {
        self.core.is_ok()
    }

    pub fn core<E>(result: Result<WaveVariantDef<PongCore>, E>) -> ReflectedCore {
        match result {
            Ok(reflected) => reflected.variant.core,
            Err(err) => ReflectedCore::server_error(),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        self.core.as_result()
    }
}

impl EchoCore {
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

    pub fn ok_or(self) -> anyhow::Result<Self> {
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
pub struct PongCore {
    /// this is meant to be the intended request recipient, which may not be the point responding
    /// to this message in the case it was intercepted and filtered at some point
    pub to: Surface,
    pub intended: Recipients,
    pub core: ReflectedCore,
    pub reflection_of: WaveId,
}

impl FromReflectedAggregate for WaveVariantDef<PongCore> {
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> anyhow::Result<Self>
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

impl<S> ToSubstance<S> for PongCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.core.to_substance_ref()
    }
}

impl PongCore {
    pub fn is_ok(&self) -> bool {
        self.core.is_ok()
    }

    pub fn core<E>(result: Result<WaveVariantDef<PongCore>, E>) -> ReflectedCore {
        match result {
            Ok(reflected) => reflected.variant.core,
            Err(err) => ReflectedCore::server_error(),
        }
    }

    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        self.core.as_result()
    }

    pub fn ok_or(&self) -> anyhow::Result<()> {
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

impl PongCore {
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
    pub fn new(to: &'a Surface, wave: &'a WaveVariantDef<DirectedWave>) -> Self {
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
    Ping(WaveVariantDef<PingCore>),
    Ripple(WaveVariantDef<RippleCoreDef<T>>),
    Signal(WaveVariantDef<SignalCore>),
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

    pub fn reflection(&self) -> anyhow::Result<Reflection> {
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

    pub fn to_signal(self) -> anyhow::Result<WaveVariantDef<SignalCore>> {
        match self {
            DirectedWave::Signal(signal) => Ok(signal),
            _ => Err("not a signal wave".into()),
        }
    }

    pub fn to_call(&self, to: Surface) -> anyhow::Result<Call> {
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

    pub fn to_call(&self) -> anyhow::Result<Call> {
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

    pub fn reflection(&self) -> anyhow::Result<Reflection> {
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

    pub fn to_wave(self) -> Wave {
        match self {
            SingularDirectedWave::Ping(ping) => Wave::Ping(ping),
            SingularDirectedWave::Signal(signal) => Wave::Signal(signal),
            SingularDirectedWave::Ripple(ripple) => Wave::Ripple(ripple.to_multiple()),
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

    pub fn set_bounce_backs(&mut self, bounce_backs: BounceBacks) -> anyhow::Result<()> {
        match self {
            DirectedWaveDef::Ripple(ripple) => {
                ripple.bounce_backs = bounce_backs;
                Ok(())
            }
            _ => Err(err(
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
                let mut wave = WaveVariantDef::new(
                    PongCore::new(core, self.to, self.intended, self.reflection_of),
                    from,
                );
                wave.track = self.track;
                wave.to_reflected()
            }
            ReflectedKind::Echo => {
                let mut wave = WaveVariantDef::new(
                    EchoCore::new(core, self.to, self.intended, self.reflection_of),
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
    fn to_substance(self) -> anyhow::Result<S> {
        match self {
            DirectedWave::Ping(ping) => ping.to_substance(),
            DirectedWave::Ripple(ripple) => ripple.to_substance(),
            DirectedWave::Signal(signal) => signal.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        match self {
            DirectedWave::Ping(ping) => ping.to_substance_ref(),
            DirectedWave::Ripple(ripple) => ripple.to_substance_ref(),
            DirectedWave::Signal(signal) => signal.to_substance_ref(),
        }
    }
}

impl DirectedWave {
    pub fn to_wave(self) -> Wave {
        match self {
            DirectedWave::Ping(ping) => Wave::Ping(ping),
            DirectedWave::Ripple(ripple) => Wave::Ripple(ripple),
            DirectedWave::Signal(signal) => Wave::Signal(signal),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum ReflectedWave {
    Pong(WaveVariantDef<PongCore>),
    Echo(WaveVariantDef<EchoCore>),
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
    fn to_substance(self) -> anyhow::Result<S> {
        match self {
            ReflectedWave::Pong(pong) => pong.to_substance(),
            ReflectedWave::Echo(echo) => echo.to_substance(),
        }
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        match self {
            ReflectedWave::Pong(pong) => pong.to_substance_ref(),
            ReflectedWave::Echo(echo) => echo.to_substance_ref(),
        }
    }
}

pub trait ToReflected {
    fn to_reflected(self) -> ReflectedWave;
    fn from_reflected(reflected: ReflectedWave) -> anyhow::Result<Self>
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

    pub fn to_wave(self) -> Wave {
        match self {
            ReflectedWave::Pong(pong) => Wave::Pong(pong),
            ReflectedWave::Echo(echo) => Wave::Echo(echo),
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

    pub fn to_echo(self) -> anyhow::Result<WaveVariantDef<EchoCore>> {
        match self {
            ReflectedWave::Echo(echo) => Ok(echo),
            _ => Err(SpaceErr::bad_request("expected Wave to be an Echo")),
        }
    }

    pub fn to_pong(self) -> anyhow::Result<WaveVariantDef<PongCore>> {
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

    pub fn success_or(&self) -> anyhow::Result<()> {
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
    pub fn to_single(self) -> anyhow::Result<Surface> {
        match self {
            Recipients::Single(surface) => Ok(surface),
            _ => Err(err(
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
                        && *point.segments.first().unwrap() == PointSeg::Space("star".to_string())
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

    pub fn single_or(self) -> anyhow::Result<Surface> {
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
pub struct WaveVariantDef<V> {
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

impl<S, V> ToSubstance<S> for WaveVariantDef<V>
where
    V: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.variant.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.variant.to_substance_ref()
    }
}

impl<V> WaveVariantDef<V> {
    pub fn inc_hops(&mut self) {
        self.hops = self.hops + 1;
    }
}

impl WaveVariantDef<Ripple> {
    pub fn to_wave(self) -> Wave {
        Wave::Ripple(self)
    }

    pub fn to_directed(self) -> DirectedWave {
        DirectedWave::Ripple(self)
    }

    pub fn with_core(mut self, core: DirectedCore) -> Self {
        self.variant.core = core;
        self
    }
}

impl<T> WaveVariantDef<RippleCoreDef<T>>
where
    T: ToRecipients + Clone,
{
    pub fn err(&self, err: SpaceErr, responder: Surface) -> WaveVariantDef<EchoCore> {
        WaveVariantDef::new(
            EchoCore::new(
                self.variant.err(err),
                self.from.clone(),
                self.to.clone().to_recipients(),
                self.id.clone(),
            ),
            responder,
        )
    }
}

impl Trackable for WaveVariantDef<SignalCore> {
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
            Substance::Wave(wave) => {
                format!("Wave({})", wave.track_key_fmt())
            }
            _ => self.track_payload(),
        }
    }
}

impl WaveVariantDef<SignalCore> {
    pub fn to_wave(self) -> Wave {
        Wave::Signal(self)
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
        signal.body(Substance::Wave(Box::new(self.to_wave())));
        signal.to(to);
        signal
    }

    pub fn unwrap_from_hop(self) -> anyhow::Result<WaveVariantDef<SignalCore>> {
        if self.method != Method::Hyp(HypMethod::Hop) {
            return Err(err(
                "expected signal wave to have method Hop",
            ));
        }
        if let Substance::Wave(wave) = &self.body {
            Ok((*wave.clone()).to_signal()?)
        } else {
            Err(err(
                "expected body substance to be of type Wave for a transport signal",
            ))
        }
    }

    pub fn unwrap_from_transport(self) -> anyhow::Result<Wave> {
        if self.method != Method::Hyp(HypMethod::Transport) {
            return Err(err(
                "expected signal wave to have method Transport",
            ));
        }
        if let Substance::Wave(wave) = &self.body {
            Ok(*wave.clone())
        } else {
            Err(err(
                "expected body substance to be of type Wave for a transport signal",
            ))
        }
    }

    pub fn to_singular_directed(self) -> SingularDirectedWave {
        SingularDirectedWave::Signal(self)
    }
}

impl<S> ToSubstance<S> for SignalCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> anyhow::Result<S> {
        self.core.to_substance()
    }

    fn to_substance_ref(&self) -> anyhow::Result<&S> {
        self.core.to_substance_ref()
    }
}

impl Trackable for WaveVariantDef<PingCore> {
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
            Substance::Wave(wave) => {
                format!("Wave({})", wave.track_key_fmt())
            }
            _ => self.track_payload(),
        }
    }
}

impl WaveVariantDef<PingCore> {
    pub fn to_wave(self) -> Wave {
        Wave::Ping(self)
    }

    pub fn to_directed(self) -> DirectedWave {
        DirectedWave::Ping(self)
    }

    pub fn err(&self, err: SpaceErr, responder: Surface) -> WaveVariantDef<PongCore> {
        WaveVariantDef::new(
            PongCore::new(
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

impl WaveVariantDef<PongCore> {
    pub fn to_wave(self) -> Wave {
        Wave::Pong(self)
    }

    pub fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Pong(self)
    }
}

impl ToReflected for WaveVariantDef<PongCore> {
    fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Pong(self)
    }

    fn from_reflected(reflected: ReflectedWave) -> anyhow::Result<Self> {
        match reflected {
            ReflectedWave::Pong(pong) => Ok(pong),
            _ => Err(SpaceErr::bad_request("expected wave to be a Pong")),
        }
    }
}

impl ToReflected for WaveVariantDef<EchoCore> {
    fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Echo(self)
    }

    fn from_reflected(reflected: ReflectedWave) -> anyhow::Result<Self> {
        match reflected {
            ReflectedWave::Echo(echo) => Ok(echo),
            _ => Err(SpaceErr::bad_request("expected Wave to be an Echo")),
        }
    }
}

impl WaveVariantDef<EchoCore> {
    pub fn to_wave(self) -> Wave {
        Wave::Echo(self)
    }

    pub fn to_reflected(self) -> ReflectedWave {
        ReflectedWave::Echo(self)
    }
}

impl TryFrom<ReflectedWave> for WaveVariantDef<PongCore> {
    type Error = SpaceErr;

    fn try_from(wave: ReflectedWave) -> Result<Self, Self::Error> {
        match wave {
            ReflectedWave::Pong(pong) => Ok(pong),
            _ => Err(SpaceErr::bad_request("Expected Wave to be a Pong")),
        }
    }
}

impl<V> WaveVariantDef<V> {
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

    pub fn replace<V2>(self, variant: V2) -> WaveVariantDef<V2>
    where
        V2: WaveVariant,
    {
        WaveVariantDef {
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

impl WaveVariant for PingCore {
    fn kind(&self) -> WaveKind {
        WaveKind::Ping
    }
}

impl WaveVariant for PongCore {
    fn kind(&self) -> WaveKind {
        WaveKind::Pong
    }
}

impl<T> WaveVariant for RippleCoreDef<T>
where
    T: ToRecipients + Clone,
{
    fn kind(&self) -> WaveKind {
        WaveKind::Ripple
    }
}

impl WaveVariant for EchoCore {
    fn kind(&self) -> WaveKind {
        WaveKind::Echo
    }
}

impl WaveVariantDef<PingCore> {
    pub fn pong(&self) -> ReflectedProto {
        let mut pong = ReflectedProto::new();
        pong.kind(ReflectedKind::Pong);
        pong.fill(self);
        pong
    }
}

impl<T> WaveVariantDef<RippleCoreDef<T>>
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

impl WaveVariantDef<SingularRipple> {
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

impl<V> Deref for WaveVariantDef<V> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.variant
    }
}

impl<V> DerefMut for WaveVariantDef<V> {
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
    pub on: Selector,
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
    fn from_reflected_aggregate(agg: ReflectedAggregate) -> anyhow::Result<Self>
    where
        Self: Sized;
}

impl TryInto<WaveVariantDef<PongCore>> for ReflectedAggregate {
    type Error = SpaceErr;
    fn try_into(self) -> Result<WaveVariantDef<PongCore>, Self::Error> {
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

impl TryInto<Vec<WaveVariantDef<EchoCore>>> for ReflectedAggregate {
    type Error = SpaceErr;
    fn try_into(self) -> Result<Vec<WaveVariantDef<EchoCore>>, Self::Error> {
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
    wave: Wave,
}
