use core::fmt::Formatter;
use core::str::FromStr;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use convert_case::{Case, Casing};
use nom::combinator::all_consuming;
use serde::de::{Error, Visitor};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use cosmic_nom::{new_span, Trace, Tw};

use crate::err::ParseErrs;
use crate::hyper::ChildRegistry;
use crate::kind::KindParts;
use crate::log::{SpanLogger, Trackable};
use crate::parse::error::result;
use crate::parse::{
    CamelCase, consume_point, consume_point_ctx, Domain, Env,
    kind_parts, parse_star_key, point_and_kind, point_route_segment, point_selector, point_var, ResolverErr,
    SkewerCase,
};
use crate::particle::traversal::TraversalPlan;
use crate::selector::{Pattern, Selector, SpecificSelector, VersionReq};
use crate::util::{ToResolved, uuid, ValueMatcher, ValuePattern};
use crate::wave::exchange::asynch::Exchanger;
use crate::wave::{
    DirectedWave, Ping, Pong, Recipients, ReflectedWave, SingularDirectedWave, ToRecipients,
    UltraWave, Wave,
};
use crate::Agent::Anonymous;
use crate::{Agent, ANONYMOUS, BaseKind, HYPERUSER, Kind, KindTemplate, ParticleRecord, SpaceErr};
use crate::point::{Point, PointDef, PointSeg, PointSegCtx, PointSegKind, PointSegPairDef, PointSegVar, RouteSeg, RouteSegVar};

lazy_static! {
    pub static ref CENTRAL: Point = StarKey::central().to_point();
    pub static ref GLOBAL_LOGGER: Point = Point::from_str("GLOBAL::logger").unwrap();
    pub static ref GLOBAL_REGISTRY: Point = Point::from_str("GLOBAL::registry").unwrap();
    pub static ref GLOBAL_EXEC: Point = Point::from_str("GLOBAL::executor").unwrap();
    pub static ref LOCAL_STAR: Point = Point::from_str("LOCAL::star").unwrap();
    pub static ref LOCAL_PORTAL: Point = Point::from_str("LOCAL::portal").unwrap();
    pub static ref LOCAL_HYPERGATE: Point = Point::from_str("LOCAL::hypergate").unwrap();
    pub static ref LOCAL_ENDPOINT: Point = Point::from_str("LOCAL::endpoint").unwrap();
    pub static ref REMOTE_ENDPOINT: Point = Point::from_str("REMOTE::endpoint").unwrap();
    pub static ref STD_WAVE_TRAVERSAL_PLAN: TraversalPlan =
        TraversalPlan::new(vec![Layer::Field, Layer::Shell, Layer::Core]);
    pub static ref MECHTRON_WAVE_TRAVERSAL_PLAN: TraversalPlan = TraversalPlan::new(vec![
        Layer::Field,
        Layer::Shell,
        Layer::Portal,
        Layer::Host,
        Layer::Guest,
        Layer::Core
    ]);
    pub static ref PORTAL_WAVE_TRAVERSAL_PLAN: TraversalPlan = TraversalPlan::new(vec![
        Layer::Field,
        Layer::Shell,
        Layer::Portal,
        Layer::Host,
        Layer::Guest,
        Layer::Core
    ]);
    pub static ref CONTROL_WAVE_TRAVERSAL_PLAN: TraversalPlan = TraversalPlan::new(vec![
        Layer::Field,
        Layer::Shell,
        Layer::Portal,
        Layer::Host,
        Layer::Guest,
        Layer::Core
    ]);
    pub static ref STAR_WAVE_TRAVERSAL_PLAN: TraversalPlan =
        TraversalPlan::new(vec![Layer::Field, Layer::Shell, Layer::Core]);
}

pub trait ToBaseKind {
    fn to_base(&self) -> BaseKind;
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct Uuid {
    uuid: String,
}

impl Uuid {
    pub fn rnd() -> Self {
        //Self::new( uuid::Uuid::new_v4() )
        uuid()
    }
    /*
    pub fn new(uuid: uuid::Uuid) -> Self {
        Self {
            uuid: uuid.to_string()
        }
    }
     */

    pub fn from<S: ToString>(uuid: S) -> Result<Self, SpaceErr> {
        //Ok(Self::new(uuid::Uuid::from_str(uuid.to_string().as_str()).map_err(|e| UniErr::server_error(format!("'{}' is not a valid uuid",uuid.to_string())))?))
        Ok(Self {
            uuid: uuid.to_string(),
        })
    }

    pub fn from_unwrap<S: ToString>(uuid: S) -> Self {
        Self {
            uuid: uuid.to_string(),
        }
    }
}

impl ToString for Uuid {
    fn to_string(&self) -> String {
        self.uuid.clone()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum ProvisionAffinity {
    Local,
    Wrangle,
}

pub type Meta = HashMap<String, String>;
pub type HostKey = String;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Version {
    pub version: semver::Version,
}

impl Deref for Version {
    type Target = semver::Version;

    fn deref(&self) -> &Self::Target {
        &self.version
    }
}

impl Serialize for Version {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.version.to_string().as_str())
    }
}

struct VersionVisitor;

impl<'de> Visitor<'de> for VersionVisitor {
    type Value = Version;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("SemVer version")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match Version::from_str(v) {
            Ok(version) => Ok(version),
            Err(error) => {
                //Err(de::Error::custom(error.to_string() ))
                Err(de::Error::invalid_type(de::Unexpected::Str(v), &self))
            }
        }
    }
}

impl<'de> Deserialize<'de> for Version {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(VersionVisitor)
    }
}

impl ToString for Version {
    fn to_string(&self) -> String {
        self.version.to_string()
    }
}

impl TryInto<semver::Version> for Version {
    type Error = SpaceErr;

    fn try_into(self) -> Result<semver::Version, Self::Error> {
        Ok(self.version)
    }
}

impl FromStr for Version {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let version = semver::Version::from_str(s)?;
        Ok(Self { version })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct Variable {
    pub name: String,
    pub trace: Trace,
}

impl Variable {
    pub fn new(name: String, trace: Trace) -> Self {
        Self { name, trace }
    }
}

pub enum VarVal<V> {
    Var(Tw<SkewerCase>),
    Val(Tw<V>),
}

impl<V> ToResolved<V> for VarVal<V>
where
    V: FromStr<Err = SpaceErr>,
{
    fn to_resolved(self, env: &Env) -> Result<V, SpaceErr> {
        match self {
            VarVal::Var(var) => match env.val(var.as_str()) {
                Ok(val) => {
                    let val: String = val.clone().try_into()?;
                    Ok(V::from_str(val.as_str())?)
                }
                Err(err) => {
                    let trace = var.trace.clone();
                    match err {
                        ResolverErr::NotAvailable => Err(ParseErrs::from_range(
                            "variables not available in this context",
                            "variables not available",
                            trace.range,
                            trace.extra,
                        )),
                        ResolverErr::NotFound => Err(ParseErrs::from_range(
                            format!("variable '{}' not found", var.unwrap().to_string()).as_str(),
                            "not found",
                            trace.range,
                            trace.extra,
                        )),
                    }
                }
            },
            VarVal::Val(val) => Ok(val.unwrap()),
        }
    }
}

pub trait RouteSegQuery {
    fn is_local(&self) -> bool;
    fn is_global(&self) -> bool;
}

pub trait PointSegQuery {
    fn is_filesystem_root(&self) -> bool;
    fn kind(&self) -> PointSegKind;
}

pub trait PointSegment {}

pub type PointSegPair = PointSegPairDef<PointSeg>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Topic {
    None,
    Not,
    Any,
    Cli,
    Uuid(Uuid),
    Path(Vec<SkewerCase>),
}

impl ToString for Topic {
    fn to_string(&self) -> String {
        match self {
            Topic::None => "".to_string(),
            Topic::Not => "Topic<!>".to_string(),
            Topic::Any => "Topic<*>".to_string(),
            Topic::Uuid(uuid) => format!("Topic<Uuid>({})", uuid.to_string()),
            Topic::Path(segs) => {
                let segments: Vec<String> = segs.into_iter().map(|s| s.to_string()).collect();
                let mut rtn = String::new();
                for (index, segment) in segments.iter().enumerate() {
                    rtn.push_str(segment.as_str());
                    if index < segments.len() - 1 {
                        rtn.push_str(":")
                    }
                }
                return format!("Topic<Path>({})", rtn);
            }
            Topic::Cli => "Topic<Cli>".to_string(),
        }
    }
}

impl Topic {
    pub fn uuid() -> Self {
        Topic::Uuid(Uuid::rnd())
    }
}

impl ValueMatcher<Topic> for Topic {
    fn is_match(&self, x: &Topic) -> Result<(), ()> {
        if *x == *self {
            Ok(())
        } else {
            Err(())
        }
    }
}

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
    Ordinalize,
)]
#[repr(u8)]
pub enum Layer {
    Gravity = 0,
    Field,
    Shell,
    Portal,
    Host,
    Guest,
    Core,
}

impl Layer {
    pub fn has_state(&self) -> bool {
        match self {
            Layer::Gravity => false,
            Layer::Field => true,
            Layer::Shell => true,
            Layer::Portal => false,
            Layer::Host => false,
            Layer::Guest => false,
            Layer::Core => false,
        }
    }
}

impl Default for Layer {
    fn default() -> Self {
        Layer::Core
    }
}

impl Default for Topic {
    fn default() -> Self {
        Topic::None
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct Surface {
    pub point: Point,
    pub layer: Layer,
    pub topic: Topic,
}

impl Surface {
    pub fn new(point: Point, layer: Layer, topic: Topic) -> Self {
        Self {
            point,
            layer,
            topic,
        }
    }

    fn postfix(&self) -> String {
        let point = self.clone().to_point();
        match &self.topic {
            Topic::None => {
                format!("{}@{}", self.point.postfix(), self.layer.to_string())
            }
            topic => {
                format!(
                    "{}@{}+{}",
                    self.point.postfix(),
                    self.layer.to_string(),
                    topic.to_string()
                )
            }
        }
    }
}

impl Into<Recipients> for Surface {
    fn into(self) -> Recipients {
        Recipients::Single(self)
    }
}

impl ToRecipients for Surface {
    fn to_recipients(self) -> Recipients {
        Recipients::Single(self)
    }
}

impl ToString for Surface {
    fn to_string(&self) -> String {
        let point = self.clone().to_point();
        match &self.topic {
            Topic::None => {
                format!("{}@{}", self.point.to_string(), self.layer.to_string())
            }
            topic => {
                format!(
                    "{}@{}+{}",
                    self.point.to_string(),
                    self.layer.to_string(),
                    topic.to_string()
                )
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SurfaceSelector {
    pub point: ValuePattern<Point>,
    pub topic: ValuePattern<Topic>,
    pub layer: ValuePattern<Layer>,
}

impl Into<SurfaceSelector> for Surface {
    fn into(self) -> SurfaceSelector {
        let point = ValuePattern::Pattern(self.point);
        let topic = match self.topic {
            Topic::None => ValuePattern::Any,
            Topic::Not => ValuePattern::None,
            Topic::Any => ValuePattern::Any,
            Topic::Uuid(uuid) => ValuePattern::Pattern(Topic::Uuid(uuid)),
            Topic::Path(path) => ValuePattern::Pattern(Topic::Path(path)),
            Topic::Cli => ValuePattern::Pattern(Topic::Cli),
        };
        let layer = ValuePattern::Pattern(self.layer);
        SurfaceSelector {
            point,
            topic,
            layer,
        }
    }
}

impl ValueMatcher<Surface> for SurfaceSelector {
    fn is_match(&self, surface: &Surface) -> Result<(), ()> {
        match &self.point {
            ValuePattern::Any => {}
            ValuePattern::None => return Err(()),
            ValuePattern::Pattern(point) if *point != surface.point => return Err(()),
            _ => {}
        }

        match &self.layer {
            ValuePattern::Any => {}
            ValuePattern::None => return Err(()),
            ValuePattern::Pattern(layer) if *layer != surface.layer => return Err(()),
            _ => {}
        }

        match &self.topic {
            ValuePattern::Any => {}
            ValuePattern::None => return Err(()),
            ValuePattern::Pattern(topic) if *topic != surface.topic => return Err(()),
            _ => {}
        }

        Ok(())
    }
}

impl Surface {
    pub fn with_topic(&self, topic: Topic) -> Self {
        Self {
            point: self.point.clone(),
            layer: self.layer.clone(),
            topic,
        }
    }

    pub fn with_layer(&self, layer: Layer) -> Self {
        Self {
            point: self.point.clone(),
            layer,
            topic: self.topic.clone(),
        }
    }
}

impl Deref for Surface {
    type Target = Point;

    fn deref(&self) -> &Self::Target {
        &self.point
    }
}

impl ToPoint for Surface {
    fn to_point(&self) -> Point {
        self.point.clone()
    }
}

impl ToSurface for Surface {
    fn to_surface(&self) -> Surface {
        self.clone()
    }
}

pub trait ToPoint {
    fn to_point(&self) -> Point;
}

pub trait ToSurface {
    fn to_surface(&self) -> Surface;
}

pub type MachineName = String;
pub type ConstellationName = String;

#[derive(PartialEq, Eq, Ord, PartialOrd, Hash, Debug, Clone, Serialize, Deserialize)]
pub struct StarKey {
    pub constellation: ConstellationName,
    pub name: String,
    pub index: u16,
}

impl StarKey {
    pub fn sql_name(&self) -> String {
        format!(
            "star_{}_{}_{}",
            self.constellation.to_lowercase().replace("-", "_"),
            self.name.to_lowercase().replace("-", "_"),
            self.index
        )
    }
}

impl Into<Point> for StarKey {
    fn into(self) -> Point {
        self.to_point()
    }
}

impl Into<Surface> for StarKey {
    fn into(self) -> Surface {
        self.to_surface()
    }
}

impl TryFrom<Point> for StarKey {
    type Error = SpaceErr;

    fn try_from(point: Point) -> Result<Self, Self::Error> {
        match point.route {
            RouteSeg::Star(star) => StarKey::from_str(star.as_str()),
            _ => Err("can only extract StarKey from Mesh point routes".into()),
        }
    }
}

impl ToPoint for StarKey {
    fn to_point(&self) -> Point {
        Point::from_str(format!("<<{}>>::star", self.to_string()).as_str()).unwrap()
    }
}

impl ToSurface for StarKey {
    fn to_surface(&self) -> Surface {
        self.clone().to_point().to_surface()
    }
}

pub struct StarHandle {
    pub name: String,
    pub index: u16,
}

impl StarHandle {
    pub fn name<S: ToString>(name: S) -> Self {
        Self {
            name: name.to_string(),
            index: 0,
        }
    }

    pub fn new<S: ToString>(name: S, index: u16) -> Self {
        Self {
            name: name.to_string(),
            index,
        }
    }
}

impl StarKey {
    pub fn new(constellation: &ConstellationName, handle: &StarHandle) -> Self {
        Self {
            constellation: constellation.clone(),
            name: handle.name.clone(),
            index: handle.index.clone(),
        }
    }

    pub fn machine(machine_name: MachineName) -> Self {
        StarKey::new(
            &"machine".to_string(),
            &StarHandle::name(machine_name.as_str()),
        )
    }

    pub fn central() -> Self {
        StarKey {
            constellation: "central".to_string(),
            name: "central".to_string(),
            index: 0,
        }
    }
}

impl ToString for StarKey {
    fn to_string(&self) -> String {
        format!("STAR::{}:{}[{}]", self.constellation, self.name, self.index)
    }
}

impl StarKey {
    pub fn to_sql_name(&self) -> String {
        format!("STAR::{}_{}_{}", self.constellation, self.name, self.index)
    }
}

impl FromStr for StarKey {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(result(all_consuming(parse_star_key)(new_span(s)))?)
    }
}

#[async_trait]
pub trait PointFactory: Send + Sync {
    async fn create(&self) -> Result<Point, SpaceErr>;
}

#[cfg(test)]
pub mod test {
    use crate::point::Point;
    use core::str::FromStr;

    #[test]
    pub fn test_root_routes() {
        let point = Point::from_str("GLOBAL::star").unwrap();
        let parent = point.parent().unwrap();
        assert!(!parent.is_local_root());
        assert_eq!(parent.to_string(), "GLOBAL::ROOT".to_string())
    }

    #[test]
    pub fn test_push_fs() {
        let point = Point::from_str("repo:some:1.0.0:/blah").unwrap();
        let bundle = point.to_bundle().unwrap();
        println!("{}", bundle.to_string());
        let root = bundle.clone().push(":/").unwrap();
        println!("{}", root.to_string());
    }
}
