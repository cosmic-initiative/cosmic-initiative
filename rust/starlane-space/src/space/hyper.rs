use std::collections::HashSet;
use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use starlane_primitive_macros::Autobox;

use crate::space::command::common::StateSrc;
use crate::space::config::mechtron::MechtronConfig;
use crate::space::err;
use crate::space::err::err;
use crate::space::kind::{Kind, KindParts, StarSub};
use crate::space::loc::{StarKey, Surface, ToSurface};
use crate::space::log::Log;
use crate::space::particle::{Details, Status, Stub};
use crate::space::point::Point;
use crate::space::selector::KindSelector;
use crate::space::substance::Substance;
use crate::space::wave::core::hyp::HypMethod;
use crate::space::wave::core::{DirectedCore, ReflectedCore};
use crate::space::wave::{
    PingCore, ReflectedKind, ReflectedProto, Wave, WaveId, WaveKind, WaveVariantDef,
};
use crate::Agent;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
pub enum AssignmentKind {
    Create,
    // eventually we will have Move as well as Create
}

#[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
pub enum ChildRegistry {
    Shell,
    Core,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Location {
    Central,
    Nowhere,
    Somewhere(Point),
}

impl ToString for Location {
    fn to_string(&self) -> String {
        match self {
            Location::Nowhere => "Unassigned".to_string(),
            Location::Somewhere(point) => point.to_string(),
            Location::Central => Point::central().to_string(),
        }
    }
}

impl Location {
    pub fn new(point: Point) -> Self {
        Location::Somewhere(point)
    }

    pub fn ok_or(&self) -> err::Result<Point> {
        match self {
            Location::Nowhere => Err(err!("Particle is presently nowhere")),
            Location::Somewhere(point) => Ok(point.clone()),

            Location::Central => Ok(Point::central()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParticleRecord {
    pub details: Details,
    pub location: ParticleLocation,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ParticleLocation {
    pub star: Option<Point>,
    pub host: Option<Point>,
}

impl ParticleLocation {
    pub fn new(star: Option<Point>, host: Option<Point>) -> Self {
        Self { star, host }
    }
}

impl From<ParticleLocation> for ReflectedCore {
    fn from(location: ParticleLocation) -> Self {
        let location = Substance::Location(location);
        ReflectedCore::ok_body(location)
    }
}

impl Default for ParticleLocation {
    fn default() -> Self {
        ParticleLocation::new(None, None)
    }
}

impl Default for ParticleRecord {
    fn default() -> Self {
        Self::root()
    }
}

impl ParticleRecord {
    pub fn new(details: Details, location: ParticleLocation) -> Self {
        ParticleRecord { details, location }
    }

    pub fn root() -> Self {
        Self {
            details: Details {
                stub: Stub {
                    point: Point::root(),
                    kind: Kind::Root,
                    status: Status::Ready,
                },
                properties: Default::default(),
            },
            location: Default::default(),
        }
    }
}

impl Into<Stub> for ParticleRecord {
    fn into(self) -> Stub {
        self.details.stub
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Provision {
    pub point: Point,
    pub state: StateSrc,
}

impl Provision {
    pub fn new(point: Point, state: StateSrc) -> Self {
        Self { point, state }
    }
}

impl TryFrom<PingCore> for Provision {
    type Error = err::Error;

    fn try_from(request: PingCore) -> Result<Self, Self::Error> {
        if let Substance::Hyper(HyperSubstance::Provision(provision)) = request.core.body {
            Ok(provision)
        } else {
            Err(err!( "expecting a Provision HyperSubstance" ))
        }
    }
}

impl Into<Substance> for Provision {
    fn into(self) -> Substance {
        Substance::Hyper(HyperSubstance::Provision(self))
    }
}

impl Into<DirectedCore> for Provision {
    fn into(self) -> DirectedCore {
        DirectedCore::new(HypMethod::Assign.into())
            .with_body(Substance::Hyper(HyperSubstance::Provision(self)))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Assign {
    pub kind: AssignmentKind,
    pub details: Details,
    pub state: StateSrc,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct HostCmd {
    pub kind: AssignmentKind,
    pub details: Details,
    pub state: StateSrc,
    pub config: MechtronConfig,
}

impl HostCmd {
    pub fn kind(&self) -> &Kind {
        &self.details.stub.kind
    }

    pub fn new(
        kind: AssignmentKind,
        details: Details,
        state: StateSrc,
        config: MechtronConfig,
    ) -> Self {
        Self {
            kind,
            details,
            state,
            config,
        }
    }
}

impl Assign {
    pub fn kind(&self) -> &Kind {
        &self.details.stub.kind
    }

    pub fn new(kind: AssignmentKind, details: Details, state: StateSrc) -> Self {
        Self {
            kind,
            details,
            state,
        }
    }

    pub fn to_host_cmd(self, config: MechtronConfig) -> HostCmd {
        HostCmd {
            kind: self.kind,
            details: self.details,
            state: self.state,
            config,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Autobox)]
pub enum HyperSubstance {
    // I don't really like having a HyperSubstance::Empty, but sometimes HypMethod
    // seems to dictate a Hyp Substance (even in the case of HypMethod::Init)
    // I hope to remove this hack one day
    Empty,
    Provision(Provision),
    Assign(Assign),
    Host(HostCmd),
    Event(HyperEvent),
    Log(Log),
    Search(Search),
    Discoveries(Discoveries),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Search {
    Star(StarKey),
    StarKind(StarSub),
    Kinds,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Discovery {
    pub star_kind: StarSub,
    pub hops: u16,
    pub star_key: StarKey,
    pub kinds: HashSet<KindSelector>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Discoveries {
    pub vec: Vec<Discovery>,
}

impl Default for Discoveries {
    fn default() -> Self {
        Self {
            vec: Default::default()
        }
    }
}

impl Discoveries {
    pub fn new() -> Self {
        Self { vec: vec![] }
    }
}

impl Deref for Discoveries {
    type Target = Vec<Discovery>;

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl DerefMut for Discoveries {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
}

impl TryFrom<PingCore> for Assign {
    type Error = err::Error;

    fn try_from(request: PingCore) -> Result<Self, Self::Error> {
        if let Substance::Hyper(HyperSubstance::Assign(assign)) = request.core.body {
            Ok(assign)
        } else {
            Err(err!("expecting an Assign HyperSubstance"))
        }
    }
}

impl Into<Substance> for Assign {
    fn into(self) -> Substance {
        Substance::Hyper(HyperSubstance::Assign(self))
    }
}

impl Into<DirectedCore> for Assign {
    fn into(self) -> DirectedCore {
        DirectedCore::new(HypMethod::Assign.into())
            .with_body(Substance::Hyper(HyperSubstance::Assign(self)))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Autobox)]
pub enum HyperEvent {
    Created(Created),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Created {
    pub point: Point,
    pub kind: KindParts,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Hash)]
pub enum InterchangeKind {
    Singleton,
    DefaultControl,
    Control(ControlPattern),
    Portal(ControlPattern),
    Star(StarKey),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display, Hash)]
pub enum ControlPattern {
    Any,
    Star(Point),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Knock {
    pub kind: InterchangeKind,
    pub auth: Box<Substance>,
    pub remote: Option<Surface>,
}

impl Knock {
    pub fn new(kind: InterchangeKind, remote: Surface, auth: Substance) -> Self {
        Self {
            kind,
            remote: Some(remote),
            auth: Box::new(auth),
        }
    }
}

impl Default for Knock {
    fn default() -> Self {
        Self {
            kind: InterchangeKind::DefaultControl,
            auth: Box::new(Substance::Empty),
            remote: None,
        }
    }
}

impl Into<WaveVariantDef<PingCore>> for Knock {
    fn into(self) -> WaveVariantDef<PingCore> {
        let mut core = DirectedCore::new(HypMethod::Knock.into());
        core.body = Substance::Knock(self);
        let wave = WaveVariantDef::new(
            PingCore::new(core, Point::local_endpoint().to_surface()),
            Point::remote_endpoint().to_surface(),
        );
        wave
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Greet {
    pub surface: Surface,
    pub agent: Agent,
    pub hop: Surface,
    pub transport: Surface,
}

impl Greet {
    pub fn new(agent: Agent, surface: Surface, hop: Surface, transport: Surface) -> Self {
        Self {
            agent,
            surface,
            hop,
            transport,
        }
    }
}

impl Into<Wave> for Greet {
    fn into(self) -> Wave {
        let mut proto = ReflectedProto::new();
        proto.kind(ReflectedKind::Pong);
        proto.agent(Agent::HyperUser);
        proto.from(self.transport.clone());
        proto.to(self.surface.clone());
        proto.intended(self.hop.clone());
        proto.reflection_of(WaveId::new(WaveKind::Ping)); // this is just randomly created since this pong reflection will not be traditionally handled on the receiving end
        proto.status(200u16);
        proto.body(self.into());
        proto.build().unwrap().to_wave()
    }
}

#[derive(Clone)]
pub enum MountKind {
    Control,
    Portal,
}

impl MountKind {
    pub fn kind(&self) -> Kind {
        match self {
            MountKind::Control => Kind::Control,
            MountKind::Portal => Kind::Portal,
        }
    }
}
