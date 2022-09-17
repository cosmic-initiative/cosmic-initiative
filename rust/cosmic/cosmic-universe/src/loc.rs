use crate::err::ParseErrs;
use crate::hyper::ChildRegistry;
use crate::log::{SpanLogger, Trackable};
use crate::parse::error::result;
use crate::parse::{
    CamelCase, consume_point, consume_point_ctx, Domain, Env,
    kind_parts, parse_star_key, point_and_kind, point_route_segment, point_selector, ResolverErr, SkewerCase,
};
use crate::selector::{Pattern, Selector, SpecificSelector, VersionReq};
use crate::util::{ToResolved, ValueMatcher, ValuePattern};
use crate::wave::{
    DirectedWave, Ping, Pong, Recipients, ReflectedWave, SingularDirectedWave,
    ToRecipients, UltraWave, Wave,
};
use crate::{Agent, ANONYMOUS, BaseKind, cosmic_uuid, HYPERUSER, Kind, KindTemplate, ParticleRecord, UniErr};
use convert_case::{Case, Casing};
use core::fmt::Formatter;
use core::str::FromStr;
use cosmic_nom::{new_span, Trace, Tw};
use nom::combinator::all_consuming;
use serde::de::{Error, Visitor};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use crate::kind::KindParts;
use crate::particle::traversal::TraversalPlan;
use crate::wave::exchange::Exchanger;
lazy_static! {
    pub static ref GLOBAL_CENTRAL: Point = Point::from_str("GLOBAL::central").unwrap();
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

pub type Uuid = String;


pub trait ToBaseKind {
    fn to_base(&self) -> BaseKind;
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum ProvisionAffinity {
    Local,
    Wrangle,
}

pub type PointKind = PointKindDef<Point>;
pub type PointKindCtx = PointKindDef<PointCtx>;
pub type PointKindVar = PointKindDef<PointVar>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct PointKindDef<Pnt> {
    pub point: Pnt,
    pub kind: Kind,
}

impl ToResolved<PointKindCtx> for PointKindVar {
    fn to_resolved(self, env: &Env) -> Result<PointKindCtx, UniErr> {
        Ok(PointKindCtx {
            point: self.point.to_resolved(env)?,
            kind: self.kind,
        })
    }
}

impl ToResolved<PointKind> for PointKindVar {
    fn to_resolved(self, env: &Env) -> Result<PointKind, UniErr> {
        Ok(PointKind {
            point: self.point.to_resolved(env)?,
            kind: self.kind,
        })
    }
}

impl ToResolved<PointKind> for PointKindCtx {
    fn to_resolved(self, env: &Env) -> Result<PointKind, UniErr> {
        Ok(PointKind {
            point: self.point.to_resolved(env)?,
            kind: self.kind,
        })
    }
}

impl PointKind {
    pub fn new(point: Point, kind: Kind) -> Self {
        Self { point, kind }
    }
}

impl ToString for PointKind {
    fn to_string(&self) -> String {
        format!("{}<{}>", self.point.to_string(), self.kind.to_string())
    }
}

impl FromStr for PointKind {
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let point_and_kind: PointKindVar = result(all_consuming(point_and_kind)(new_span(s)))?;
        let point_and_kind = point_and_kind.collapse()?;
        Ok(point_and_kind)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct AddressAndType {
    pub point: Point,
    pub resource_type: BaseKind,
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
    type Error = UniErr;

    fn try_into(self) -> Result<semver::Version, Self::Error> {
        Ok(self.version)
    }
}

impl FromStr for Version {
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let version = semver::Version::from_str(s)?;
        Ok(Self { version })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Hash)]
pub struct Specific {
    pub provider: Domain,
    pub vendor: Domain,
    pub product: SkewerCase,
    pub variant: SkewerCase,
    pub version: Version,
}

impl Specific {
    pub fn to_selector(&self) -> SpecificSelector {
        SpecificSelector::from_str(self.to_string().as_str()).unwrap()
    }
}

impl ToString for Specific {
    fn to_string(&self) -> String {
        format!(
            "{}:{}:{}:{}:{}",
            self.provider,
            self.vendor,
            self.product,
            self.variant,
            self.version.to_string()
        )
    }
}

impl FromStr for Specific {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

impl TryInto<SpecificSelector> for Specific {
    type Error = UniErr;

    fn try_into(self) -> Result<SpecificSelector, Self::Error> {
        Ok(SpecificSelector {
            provider: Pattern::Exact(self.provider),
            vendor: Pattern::Exact(self.vendor),
            product: Pattern::Exact(self.product),
            variant: Pattern::Exact(self.variant),
            version: VersionReq::from_str(self.version.to_string().as_str())?,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum RouteSeg {
    This,
    Local,
    Remote,
    Global,
    Domain(String),
    Tag(String),
    Star(String),
}

impl RouteSegQuery for RouteSeg {
    fn is_local(&self) -> bool {
        match self {
            RouteSeg::This => true,
            _ => false,
        }
    }

    fn is_global(&self) -> bool {
        match self {
            RouteSeg::Global => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum RouteSegVar {
    This,
    Local,
    Remote,
    Global,
    Domain(String),
    Tag(String),
    Star(String),
    Var(Variable),
}

impl RouteSegQuery for RouteSegVar {
    fn is_local(&self) -> bool {
        match self {
            RouteSegVar::This => true,
            _ => false,
        }
    }

    fn is_global(&self) -> bool {
        match self {
            RouteSegVar::Global => true,
            _ => false,
        }
    }
}

impl TryInto<RouteSeg> for RouteSegVar {
    type Error = UniErr;

    fn try_into(self) -> Result<RouteSeg, Self::Error> {
        match self {
            RouteSegVar::This => Ok(RouteSeg::This),
            RouteSegVar::Local => Ok(RouteSeg::Local),
            RouteSegVar::Global => Ok(RouteSeg::Global),
            RouteSegVar::Domain(domain) => Ok(RouteSeg::Domain(domain)),
            RouteSegVar::Tag(tag) => Ok(RouteSeg::Tag(tag)),
            RouteSegVar::Star(star) => Ok(RouteSeg::Star(star)),
            RouteSegVar::Var(var) => Err(ParseErrs::from_range(
                "variables not allowed in this context",
                "variable not allowed here",
                var.trace.range,
                var.trace.extra,
            )),
            RouteSegVar::Remote => Ok(RouteSeg::Remote),
        }
    }
}

impl Into<RouteSegVar> for RouteSeg {
    fn into(self) -> RouteSegVar {
        match self {
            RouteSeg::This => RouteSegVar::This,
            RouteSeg::Local => RouteSegVar::Local,
            RouteSeg::Remote => RouteSegVar::Remote,
            RouteSeg::Global => RouteSegVar::Global,
            RouteSeg::Domain(domain) => RouteSegVar::Domain(domain),
            RouteSeg::Tag(tag) => RouteSegVar::Tag(tag),
            RouteSeg::Star(mesh) => RouteSegVar::Star(mesh),
        }
    }
}

impl ToString for RouteSegVar {
    fn to_string(&self) -> String {
        match self {
            Self::This => ".".to_string(),
            Self::Local => "LOCAL".to_string(),
            Self::Remote => "REMOTE".to_string(),
            Self::Global => "GLOBAL".to_string(),
            Self::Domain(domain) => domain.clone(),
            Self::Tag(tag) => {
                format!("[{}]", tag)
            }
            Self::Star(mesh) => {
                format!("<<{}>>", mesh)
            }
            Self::Var(var) => {
                format!("${{{}}}", var.name)
            }
        }
    }
}

impl FromStr for RouteSeg {
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = new_span(s);
        Ok(all_consuming(point_route_segment)(s)?.1)
    }
}

impl ToString for RouteSeg {
    fn to_string(&self) -> String {
        match self {
            RouteSeg::This => ".".to_string(),
            RouteSeg::Domain(domain) => domain.clone(),
            RouteSeg::Tag(tag) => {
                format!("[{}]", tag)
            }
            RouteSeg::Star(sys) => {
                format!("<<{}>>", sys)
            }
            RouteSeg::Global => "GLOBAL".to_string(),
            RouteSeg::Local => "LOCAL".to_string(),
            RouteSeg::Remote => "REMOTE".to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum PointSegKind {
    Root,
    Space,
    Base,
    FilesystemRootDir,
    Dir,
    File,
    Version,
    Pop,
    Working,
    Var,
}

impl PointSegKind {
    pub fn preceding_delim(&self, post_fileroot: bool) -> &'static str {
        match self {
            Self::Space => "",
            Self::Base => ":",
            Self::Dir => "",
            Self::File => "",
            Self::Version => ":",
            Self::FilesystemRootDir => ":",
            Self::Root => "",
            Self::Pop => match post_fileroot {
                true => "",
                false => ":",
            },
            Self::Working => match post_fileroot {
                true => "",
                false => ":",
            },
            Self::Var => match post_fileroot {
                true => "",
                false => ":",
            },
        }
    }

    pub fn is_normalized(&self) -> bool {
        match self {
            Self::Pop => false,
            Self::Working => false,
            Self::Var => false,
            _ => true,
        }
    }

    pub fn is_version(&self) -> bool {
        match self {
            Self::Version => true,
            _ => false,
        }
    }

    pub fn is_file(&self) -> bool {
        match self {
            Self::File => true,
            _ => false,
        }
    }

    pub fn is_dir(&self) -> bool {
        match self {
            Self::Dir => true,
            _ => false,
        }
    }

    pub fn is_filesystem_seg(&self) -> bool {
        match self {
            PointSegKind::Root => false,
            PointSegKind::Space => false,
            PointSegKind::Base => false,
            PointSegKind::FilesystemRootDir => true,
            PointSegKind::Dir => true,
            PointSegKind::File => true,
            PointSegKind::Version => false,
            PointSegKind::Pop => true,
            PointSegKind::Working => true,
            PointSegKind::Var => true,
        }
    }

    pub fn is_mesh_seg(&self) -> bool {
        match self {
            PointSegKind::Root => true,
            PointSegKind::Space => true,
            PointSegKind::Base => true,
            PointSegKind::FilesystemRootDir => false,
            PointSegKind::Dir => false,
            PointSegKind::File => false,
            PointSegKind::Version => true,
            PointSegKind::Pop => true,
            PointSegKind::Working => true,
            PointSegKind::Var => true,
        }
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
    V: FromStr<Err = UniErr>,
{
    fn to_resolved(self, env: &Env) -> Result<V, UniErr> {
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

impl PointSegQuery for PointSeg {
    fn is_filesystem_root(&self) -> bool {
        match self {
            Self::FilesystemRootDir => true,
            _ => false,
        }
    }
    fn kind(&self) -> PointSegKind {
        match self {
            PointSeg::Root => PointSegKind::Root,
            PointSeg::Space(_) => PointSegKind::Space,
            PointSeg::Base(_) => PointSegKind::Base,
            PointSeg::FilesystemRootDir => PointSegKind::FilesystemRootDir,
            PointSeg::Dir(_) => PointSegKind::Dir,
            PointSeg::File(_) => PointSegKind::File,
            PointSeg::Version(_) => PointSegKind::Version,
        }
    }
}

impl PointSegQuery for PointSegCtx {
    fn is_filesystem_root(&self) -> bool {
        match self {
            Self::FilesystemRootDir => true,
            _ => false,
        }
    }

    fn kind(&self) -> PointSegKind {
        match self {
            Self::Root => PointSegKind::Root,
            Self::Space(_) => PointSegKind::Space,
            Self::Base(_) => PointSegKind::Base,
            Self::FilesystemRootDir => PointSegKind::FilesystemRootDir,
            Self::Dir(_) => PointSegKind::Dir,
            Self::File(_) => PointSegKind::File,
            Self::Version(_) => PointSegKind::Version,
            Self::Pop { .. } => PointSegKind::Pop,
            Self::Working { .. } => PointSegKind::Working,
        }
    }
}

impl PointSegQuery for PointSegVar {
    fn is_filesystem_root(&self) -> bool {
        match self {
            Self::FilesystemRootDir => true,
            _ => false,
        }
    }

    fn kind(&self) -> PointSegKind {
        match self {
            Self::Root => PointSegKind::Root,
            Self::Space(_) => PointSegKind::Space,
            Self::Base(_) => PointSegKind::Base,
            Self::FilesystemRootDir => PointSegKind::FilesystemRootDir,
            Self::Dir(_) => PointSegKind::Dir,
            Self::File(_) => PointSegKind::File,
            Self::Version(_) => PointSegKind::Version,
            Self::Pop { .. } => PointSegKind::Pop,
            Self::Working { .. } => PointSegKind::Working,
            Self::Var(_) => PointSegKind::Var,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSegCtx {
    Root,
    Space(String),
    Base(String),
    FilesystemRootDir,
    Dir(String),
    File(String),
    Version(Version),
    Working(Trace),
    Pop(Trace),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSegVar {
    Root,
    Space(String),
    Base(String),
    FilesystemRootDir,
    Dir(String),
    File(String),
    Version(Version),
    Working(Trace),
    Pop(Trace),
    Var(Variable),
}

impl ToString for PointSegVar {
    fn to_string(&self) -> String {
        match self {
            PointSegVar::Root => "".to_string(),
            PointSegVar::Space(space) => space.clone(),
            PointSegVar::Base(base) => base.clone(),
            PointSegVar::FilesystemRootDir => "/".to_string(),
            PointSegVar::Dir(dir) => dir.clone(),
            PointSegVar::File(file) => file.clone(),
            PointSegVar::Version(version) => version.to_string(),
            PointSegVar::Working(_) => ".".to_string(),
            PointSegVar::Pop(_) => "..".to_string(),
            PointSegVar::Var(var) => format!("${{{}}}", var.name),
        }
    }
}

impl PointSegVar {
    pub fn is_normalized(&self) -> bool {
        self.kind().is_normalized()
    }

    pub fn is_filesystem_seg(&self) -> bool {
        self.kind().is_filesystem_seg()
    }
}

impl Into<PointSegVar> for PointSegCtx {
    fn into(self) -> PointSegVar {
        match self {
            PointSegCtx::Root => PointSegVar::Root,
            PointSegCtx::Space(space) => PointSegVar::Space(space),
            PointSegCtx::Base(base) => PointSegVar::Base(base),
            PointSegCtx::FilesystemRootDir => PointSegVar::FilesystemRootDir,
            PointSegCtx::Dir(dir) => PointSegVar::Dir(dir),
            PointSegCtx::File(file) => PointSegVar::File(file),
            PointSegCtx::Version(version) => PointSegVar::Version(version),
            PointSegCtx::Working(trace) => PointSegVar::Working(trace),
            PointSegCtx::Pop(trace) => PointSegVar::Pop(trace),
        }
    }
}

impl TryInto<PointSegCtx> for PointSegVar {
    type Error = UniErr;

    fn try_into(self) -> Result<PointSegCtx, Self::Error> {
        match self {
            PointSegVar::Root => Ok(PointSegCtx::Root),
            PointSegVar::Space(space) => Ok(PointSegCtx::Space(space)),
            PointSegVar::Base(base) => Ok(PointSegCtx::Base(base)),
            PointSegVar::FilesystemRootDir => Ok(PointSegCtx::FilesystemRootDir),
            PointSegVar::Dir(dir) => Ok(PointSegCtx::Dir(dir)),
            PointSegVar::File(file) => Ok(PointSegCtx::File(file)),
            PointSegVar::Version(version) => Ok(PointSegCtx::Version(version)),
            PointSegVar::Working(trace) => Err(ParseErrs::from_range(
                "working point not available in this context",
                "working point not available",
                trace.range,
                trace.extra,
            )),
            PointSegVar::Pop(trace) => Err(ParseErrs::from_range(
                "point pop not available in this context",
                "point pop not available",
                trace.range,
                trace.extra,
            )),
            PointSegVar::Var(var) => Err(ParseErrs::from_range(
                "variable substitution not available in this context",
                "var subst not available",
                var.trace.range,
                var.trace.extra,
            )),
        }
    }
}

impl TryInto<PointSeg> for PointSegCtx {
    type Error = UniErr;

    fn try_into(self) -> Result<PointSeg, Self::Error> {
        match self {
            PointSegCtx::Root => Ok(PointSeg::Root),
            PointSegCtx::Space(space) => Ok(PointSeg::Space(space)),
            PointSegCtx::Base(base) => Ok(PointSeg::Base(base)),
            PointSegCtx::FilesystemRootDir => Ok(PointSeg::FilesystemRootDir),
            PointSegCtx::Dir(dir) => Ok(PointSeg::Dir(dir)),
            PointSegCtx::File(file) => Ok(PointSeg::File(file)),
            PointSegCtx::Version(version) => Ok(PointSeg::Version(version)),
            PointSegCtx::Working(trace) => Err(ParseErrs::from_range(
                "working point not available in this context",
                "working point not available",
                trace.range,
                trace.extra,
            )),
            PointSegCtx::Pop(trace) => Err(ParseErrs::from_range(
                "point pop not available in this context",
                "point pop not available",
                trace.range,
                trace.extra,
            )),
        }
    }
}

impl PointSegCtx {
    pub fn is_normalized(&self) -> bool {
        self.kind().is_normalized()
    }

    pub fn is_filesystem_seg(&self) -> bool {
        self.kind().is_filesystem_seg()
    }
}

pub trait PointSegment {}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSeg {
    Root,
    Space(String),
    Base(String),
    FilesystemRootDir,
    Dir(String),
    File(String),
    Version(Version),
}

impl PointSegment for PointSeg {}

impl PointSegment for PointSegCtx {}

impl PointSegment for PointSegVar {}

impl Into<PointSegCtx> for PointSeg {
    fn into(self) -> PointSegCtx {
        match self {
            PointSeg::Root => PointSegCtx::Root,
            PointSeg::Space(space) => PointSegCtx::Space(space),
            PointSeg::Base(base) => PointSegCtx::Base(base),
            PointSeg::FilesystemRootDir => PointSegCtx::FilesystemRootDir,
            PointSeg::Dir(dir) => PointSegCtx::Dir(dir),
            PointSeg::File(file) => PointSegCtx::File(file),
            PointSeg::Version(version) => PointSegCtx::Version(version),
        }
    }
}

impl PointSeg {
    pub fn is_file(&self) -> bool {
        self.kind().is_file()
    }

    pub fn is_normalized(&self) -> bool {
        self.kind().is_normalized()
    }

    pub fn is_version(&self) -> bool {
        self.kind().is_version()
    }

    pub fn is_filesystem_seg(&self) -> bool {
        self.kind().is_filesystem_seg()
    }
    pub fn preceding_delim(&self, post_fileroot: bool) -> &str {
        self.kind().preceding_delim(post_fileroot)
    }
}

impl ToString for PointSeg {
    fn to_string(&self) -> String {
        match self {
            PointSeg::Space(space) => space.clone(),
            PointSeg::Base(base) => base.clone(),
            PointSeg::Dir(dir) => dir.clone(),
            PointSeg::File(file) => file.clone(),
            PointSeg::Version(version) => version.to_string(),
            PointSeg::FilesystemRootDir => "/".to_string(),
            PointSeg::Root => "".to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PointSegDelim {
    Empty,
    Mesh,
    File,
}

impl ToString for PointSegDelim {
    fn to_string(&self) -> String {
        match self {
            PointSegDelim::Empty => "".to_string(),
            PointSegDelim::Mesh => ":".to_string(),
            PointSegDelim::File => "/".to_string(),
        }
    }
}

pub type PointSegPair = PointSegPairDef<PointSeg>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PointSegPairDef<Seg> {
    pub delim: PointSegDelim,
    pub seg: Seg,
}

impl<Seg> PointSegPairDef<Seg> {
    pub fn new(delim: PointSegDelim, seg: Seg) -> Self {
        Self { delim, seg }
    }
}

impl<Seg> ToString for PointSegPairDef<Seg>
where
    Seg: ToString,
{
    fn to_string(&self) -> String {
        format!("{}{}", self.delim.to_string(), self.seg.to_string())
    }
}

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
            Topic::Uuid(uuid) => format!("Topic<Uuid>({})", uuid),
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
        Self::Uuid(unsafe { cosmic_uuid() })
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

impl Into<Surface> for Point {
    fn into(self) -> Surface {
        Surface {
            point: self,
            topic: Default::default(),
            layer: Default::default(),
        }
    }
}

impl ToRecipients for Point {
    fn to_recipients(self) -> Recipients {
        self.to_surface().to_recipients()
    }
}

pub type Point = PointDef<RouteSeg, PointSeg>;
pub type PointCtx = PointDef<RouteSeg, PointSegCtx>;
pub type PointVar = PointDef<RouteSegVar, PointSegVar>;

impl PointVar {
    pub fn to_point(self) -> Result<Point, UniErr> {
        self.collapse()
    }

    pub fn to_point_ctx(self) -> Result<PointCtx, UniErr> {
        self.collapse()
    }
}

impl ToPoint for Point {
    fn to_point(&self) -> Point {
        self.clone()
    }
}

impl ToSurface for Point {
    fn to_surface(&self) -> Surface {
        self.clone().into()
    }
}

impl ToResolved<Point> for PointVar {
    fn to_resolved(self, env: &Env) -> Result<Point, UniErr> {
        let point_ctx: PointCtx = self.to_resolved(env)?;
        point_ctx.to_resolved(env)
    }
}

impl Into<Selector> for Point {
    fn into(self) -> Selector {
        let string = self.to_string();
        let rtn = result(all_consuming(point_selector)(new_span(string.as_str()))).unwrap();
        string;
        rtn
    }
}

impl PointCtx {
    pub fn to_point(self) -> Result<Point, UniErr> {
        self.collapse()
    }
}

impl ToResolved<PointCtx> for PointVar {
    fn collapse(self) -> Result<PointCtx, UniErr> {
        let route = self.route.try_into()?;
        let mut segments = vec![];
        for segment in self.segments {
            segments.push(segment.try_into()?);
        }
        Ok(PointCtx { route, segments })
    }

    fn to_resolved(self, env: &Env) -> Result<PointCtx, UniErr> {
        let mut rtn = String::new();
        let mut after_fs = false;
        let mut errs = vec![];

        match &self.route {
            RouteSegVar::Var(var) => match env.val(var.name.clone().as_str()) {
                Ok(val) => {
                    let val: String = val.clone().try_into()?;
                    rtn.push_str(format!("{}::", val.as_str()).as_str());
                }
                Err(err) => match err {
                    ResolverErr::NotAvailable => {
                        errs.push(ParseErrs::from_range(
                            format!(
                                "variables not available in this context '{}'",
                                var.name.clone()
                            )
                            .as_str(),
                            "Not Available",
                            var.trace.range.clone(),
                            var.trace.extra.clone(),
                        ));
                    }
                    ResolverErr::NotFound => {
                        errs.push(ParseErrs::from_range(
                            format!("variable could not be resolved '{}'", var.name.clone())
                                .as_str(),
                            "Not Found",
                            var.trace.range.clone(),
                            var.trace.extra.clone(),
                        ));
                    }
                },
            },

            RouteSegVar::This => {}
            RouteSegVar::Domain(domain) => {
                rtn.push_str(format!("{}::", domain).as_str());
            }
            RouteSegVar::Tag(tag) => {
                rtn.push_str(format!("[{}]::", tag).as_str());
            }
            RouteSegVar::Star(mesh) => {
                rtn.push_str(format!("<{}>::", mesh).as_str());
            }
            RouteSegVar::Global => {
                rtn.push_str("GLOBAL::");
            }
            RouteSegVar::Local => {
                rtn.push_str("LOCAL::");
            }
            RouteSegVar::Remote => {
                rtn.push_str("REMOTE::");
            }
        };

        if self.segments.len() == 0 {
            rtn.push_str("ROOT");
            return consume_point_ctx(rtn.as_str());
        }
        for (index, segment) in self.segments.iter().enumerate() {
            if let PointSegVar::Var(ref var) = segment {
                match env.val(var.name.clone().as_str()) {
                    Ok(val) => {
                        if index > 1 {
                            if after_fs {
                                //                                    rtn.push_str("/");
                            } else {
                                rtn.push_str(":");
                            }
                        }
                        let val: String = val.clone().try_into()?;
                        rtn.push_str(val.as_str());
                    }
                    Err(err) => match err {
                        ResolverErr::NotAvailable => {
                            errs.push(ParseErrs::from_range(
                                format!(
                                    "variables not available in this context '{}'",
                                    var.name.clone()
                                )
                                .as_str(),
                                "Not Available",
                                var.trace.range.clone(),
                                var.trace.extra.clone(),
                            ));
                        }
                        ResolverErr::NotFound => {
                            errs.push(ParseErrs::from_range(
                                format!("variable could not be resolved '{}'", var.name.clone())
                                    .as_str(),
                                "Not Found",
                                var.trace.range.clone(),
                                var.trace.extra.clone(),
                            ));
                        }
                    },
                }
            } else if PointSegVar::FilesystemRootDir == *segment {
                after_fs = true;
                rtn.push_str(":/");
            } else {
                if index > 0 {
                    if after_fs {
                        //rtn.push_str("/");
                    } else {
                        rtn.push_str(":");
                    }
                }
                rtn.push_str(segment.to_string().as_str());
            }
        }
        if self.is_dir() {
            //rtn.push_str("/");
        }

        if !errs.is_empty() {
            let errs = ParseErrs::fold(errs);
            return Err(errs.into());
        }
        consume_point_ctx(rtn.as_str())
    }
}

impl ToResolved<Point> for PointCtx {
    fn collapse(self) -> Result<Point, UniErr> {
        let mut segments = vec![];
        for segment in self.segments {
            segments.push(segment.try_into()?);
        }
        Ok(Point {
            route: self.route,
            segments,
        })
    }

    fn to_resolved(self, env: &Env) -> Result<Point, UniErr> {
        if self.segments.is_empty() {
            return Ok(Point {
                route: self.route,
                segments: vec![],
            });
        }

        let mut old = self;
        let mut point = Point::root();

        for (index, segment) in old.segments.iter().enumerate() {
            match segment {
                PointSegCtx::Working(trace) => {
                    if index > 1 {
                        return Err(ParseErrs::from_range(
                            "working point can only be referenced in the first point segment",
                            "first segment only",
                            trace.range.clone(),
                            trace.extra.clone(),
                        ));
                    }
                    point = match env.point_or() {
                        Ok(point) => point.clone(),
                        Err(_) => {
                            return Err(ParseErrs::from_range(
                                "working point is not available in this context",
                                "not available",
                                trace.range.clone(),
                                trace.extra.clone(),
                            ));
                        }
                    };
                }
                PointSegCtx::Pop(trace) => {
                    if index <= 1 {
                        point = match env.point_or() {
                            Ok(point) => point.clone(),
                            Err(_) => {
                                return Err(ParseErrs::from_range(
                                    "cannot pop because working point is not available in this context",
                                    "not available",
                                    trace.range.clone(),
                                    trace.extra.clone(),
                                ));
                            }
                        };
                    }
                    if point.segments.pop().is_none() {
                        return Err(ParseErrs::from_range(
                            format!(
                                "Too many point pops. working point was: '{}'",
                                env.point_or().unwrap().to_string()
                            )
                            .as_str(),
                            "too many point pops",
                            trace.range.clone(),
                            trace.extra.clone(),
                        ));
                    }
                }
                PointSegCtx::FilesystemRootDir => {
                    point = point.push(":/".to_string())?;
                }
                PointSegCtx::Root => {
                    //segments.push(PointSeg::Root)
                }
                PointSegCtx::Space(space) => point = point.push(space.clone())?,
                PointSegCtx::Base(base) => point = point.push(base.clone())?,
                PointSegCtx::Dir(dir) => point = point.push(dir.clone())?,
                PointSegCtx::File(file) => point = point.push(file.clone())?,
                PointSegCtx::Version(version) => point = point.push(version.to_string())?,
            }
        }

        Ok(point)
    }
}

impl TryInto<Point> for PointCtx {
    type Error = UniErr;

    fn try_into(self) -> Result<Point, Self::Error> {
        let mut rtn = vec![];
        for segment in self.segments {
            rtn.push(segment.try_into()?);
        }
        Ok(Point {
            route: self.route,
            segments: rtn,
        })
    }
}

impl TryFrom<String> for Point {
    type Error = UniErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        consume_point(value.as_str())
    }
}

impl TryFrom<&str> for Point {
    type Error = UniErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        consume_point(value)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct PointDef<Route, Seg> {
    pub route: Route,
    pub segments: Vec<Seg>,
}

impl<Route, Seg> PointDef<Route, Seg>
where
    Route: Clone,
    Seg: Clone,
{
    pub fn parent(&self) -> Option<PointDef<Route, Seg>> {
        if self.segments.is_empty() {
            return None;
        }
        let mut segments = self.segments.clone();
        segments.remove(segments.len() - 1);
        Some(Self {
            route: self.route.clone(),
            segments,
        })
    }

    pub fn last_segment(&self) -> Option<Seg> {
        self.segments.last().cloned()
    }

    pub fn is_root(&self) -> bool {
        self.segments.is_empty()
    }
}

impl Point {
    pub fn to_agent(&self) -> Agent {
        if *self == *HYPERUSER {
            Agent::HyperUser
        } else if *self == *ANONYMOUS {
            Agent::Anonymous
        } else {
            Agent::Point(self.clone())
        }
    }

    pub fn is_global(&self) -> bool {
        match self.route {
            RouteSeg::Global => true,
            _ => false,
        }
    }

    pub fn is_parent_of(&self, point: &Point) -> bool {
        if self.segments.len() > point.segments.len() {
            return false;
        }

        if self.route != point.route {
            return false;
        }

        for i in 0..self.segments.len() {
            if *self.segments.get(i).as_ref().unwrap() != *point.segments.get(i).as_ref().unwrap() {
                return false;
            }
        }
        true
    }

    pub fn central() -> Self {
        GLOBAL_CENTRAL.clone()
    }

    pub fn global_executor() -> Self {
        GLOBAL_EXEC.clone()
    }

    pub fn local_portal() -> Self {
        LOCAL_PORTAL.clone()
    }

    pub fn local_hypergate() -> Self {
        LOCAL_HYPERGATE.clone()
    }

    pub fn local_endpoint() -> Self {
        LOCAL_ENDPOINT.clone()
    }

    pub fn remote_endpoint() -> Self {
        REMOTE_ENDPOINT.clone()
    }

    pub fn normalize(self) -> Result<Point, UniErr> {
        if self.is_normalized() {
            return Ok(self);
        }

        if !self
            .segments
            .first()
            .expect("expected first segment")
            .is_normalized()
        {
            return Err(format!("absolute point paths cannot begin with '..' (reference parent segment) because there is no working point segment: '{}'",self.to_string()).into());
        }

        let mut segments = vec![];
        for seg in &self.segments {
            match seg.is_normalized() {
                true => segments.push(seg.clone()),
                false => {
                    if segments.pop().is_none() {
                        return Err(format!(
                            "'..' too many pop segments directives: out of parents: '{}'",
                            self.to_string()
                        )
                        .into());
                    }
                }
            }
        }
        Ok(Point {
            route: self.route,
            segments,
        })
    }

    pub fn is_parent(&self, child: &Point) -> Result<(), ()> {
        if self.route != child.route {
            return Err(());
        }

        if self.segments.len() >= child.segments.len() {
            return Err(());
        }

        for (index, seg) in self.segments.iter().enumerate() {
            if *seg != *child.segments.get(index).unwrap() {
                return Err(());
            }
        }

        Ok(())
    }

    pub fn is_normalized(&self) -> bool {
        for seg in &self.segments {
            if !seg.is_normalized() {
                return false;
            }
        }
        true
    }

    pub fn to_bundle(self) -> Result<Point, UniErr> {
        if self.segments.is_empty() {
            return Err("Point does not contain a bundle".into());
        }

        if let Some(PointSeg::Version(_)) = self.segments.last() {
            return Ok(self);
        }

        return self.parent().expect("expected parent").to_bundle();
    }

    pub fn has_bundle(&self) -> bool {
        if self.segments.is_empty() {
            return false;
        }

        if let Some(PointSeg::Version(_)) = self.segments.last() {
            return true;
        }

        return self.parent().expect("expected parent").to_bundle().is_ok();
    }

    pub fn to_safe_filename(&self) -> String {
        self.to_string()
    }

    pub fn has_filesystem(&self) -> bool {
        for segment in &self.segments {
            match segment {
                PointSeg::FilesystemRootDir => {
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    pub fn is_artifact_bundle_part(&self) -> bool {
        for segment in &self.segments {
            if segment.is_version() {
                return true;
            }
        }
        return false;
    }

    pub fn is_artifact(&self) -> bool {
        if let Option::Some(segment) = self.last_segment() {
            if self.is_artifact_bundle_part() && segment.is_file() {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn is_artifact_bundle(&self) -> bool {
        if let Option::Some(segment) = self.last_segment() {
            segment.is_version()
        } else {
            false
        }
    }

    pub fn pop(&self) -> Self {
        let mut segments = self.segments.clone();
        segments.pop();
        Point {
            route: self.route.clone(),
            segments,
        }
    }
    pub fn push<S: ToString>(&self, segment: S) -> Result<Self, UniErr> {
        let segment = segment.to_string();
        if self.segments.is_empty() {
            Self::from_str(segment.as_str())
        } else {
            let last = self.last_segment().expect("expected last segment");
            let point = match last {
                PointSeg::Root => segment,
                PointSeg::Space(_) => {
                    format!("{}:{}", self.to_string(), segment)
                }
                PointSeg::Base(_) => {
                    format!("{}:{}", self.to_string(), segment)
                }
                PointSeg::FilesystemRootDir => {
                    format!("{}{}", self.to_string(), segment)
                }
                PointSeg::Dir(_) => {
                    format!("{}{}", self.to_string(), segment)
                }
                PointSeg::Version(_) => {
                    if segment != ":/" {
                        return Err(format!(
                            "expected Root filesystem artifact ':/' encountered: {}",
                            segment
                        )
                        .into());
                    }
                    format!("{}:/", self.to_string())
                }
                PointSeg::File(_) => return Err("cannot append to a file".into()),
            };
            Self::from_str(point.as_str())
        }
    }

    pub fn push_file(&self, segment: String) -> Result<Self, UniErr> {
        Self::from_str(format!("{}{}", self.to_string(), segment).as_str())
    }

    pub fn push_segment(&self, segment: PointSeg) -> Result<Self, UniErr> {
        if (self.has_filesystem() && segment.is_filesystem_seg()) || segment.kind().is_mesh_seg() {
            let mut point = self.clone();
            point.segments.push(segment);
            Ok(point)
        } else {
            if self.has_filesystem() {
                Err("cannot push a Mesh segment onto a point after the FileSystemRoot segment has been pushed".into())
            } else {
                Err("cannot push a FileSystem segment onto a point until after the FileSystemRoot segment has been pushed".into())
            }
        }
    }

    pub fn filepath(&self) -> Option<String> {
        let mut path = String::new();
        for segment in &self.segments {
            match segment {
                PointSeg::FilesystemRootDir => {
                    path.push_str("/");
                }
                PointSeg::Dir(dir) => {
                    path.push_str(dir.as_str());
                }
                PointSeg::File(file) => {
                    path.push_str(file.as_str());
                }
                _ => {}
            }
        }
        if path.is_empty() {
            None
        } else {
            Some(path)
        }
    }

    pub fn is_filesystem_ref(&self) -> bool {
        if let Option::Some(last_segment) = self.last_segment() {
            last_segment.is_filesystem_seg()
        } else {
            false
        }
    }

    pub fn truncate(self, kind: PointSegKind) -> Result<Point, UniErr> {
        let mut segments = vec![];
        for segment in &self.segments {
            segments.push(segment.clone());
            if segment.kind() == kind {
                return Ok(Self {
                    route: self.route,
                    segments,
                });
            }
        }

        Err(UniErr::Status {
            status: 404,
            message: format!(
                "Point segment kind: {} not found in point: {}",
                kind.to_string(),
                self.to_string()
            ),
        })
    }
}

impl FromStr for Point {
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        consume_point(s)
    }
}

impl Into<String> for Point {
    fn into(self) -> String {
        self.to_string()
    }
}

impl<Route, Seg> PointDef<Route, Seg>
where
    Route: ToString,
    Seg: PointSegQuery + ToString,
{
    pub fn to_string_impl(&self, show_route: bool) -> String {
        let mut rtn = String::new();

        if show_route {
            rtn.push_str(self.route.to_string().as_str());
            rtn.push_str("::");
        }

        let mut post_fileroot = false;

        if self.segments.is_empty() {
            "ROOT".to_string()
        } else {
            for (i, segment) in self.segments.iter().enumerate() {
                if segment.is_filesystem_root() {
                    post_fileroot = true;
                }
                if i > 0 {
                    rtn.push_str(segment.kind().preceding_delim(post_fileroot));
                }
                rtn.push_str(segment.to_string().as_str());
            }
            rtn.to_string()
        }
    }
}

impl<Route, Seg> ToString for PointDef<Route, Seg>
where
    Route: RouteSegQuery + ToString,
    Seg: PointSegQuery + ToString,
{
    fn to_string(&self) -> String {
        self.to_string_impl(!self.route.is_local())
    }
}

impl Point {
    pub fn root() -> Self {
        Self {
            route: RouteSeg::This,
            segments: vec![],
        }
    }

    pub fn root_with_route(route: RouteSeg) -> Self {
        Self {
            route,
            segments: vec![],
        }
    }

    pub fn is_local_root(&self) -> bool {
        self.segments.is_empty() && self.route.is_local()
    }
}

impl PointVar {
    pub fn is_dir(&self) -> bool {
        self.segments
            .last()
            .unwrap_or(&PointSegVar::Root)
            .kind()
            .is_dir()
    }
}

impl PointCtx {
    pub fn is_dir(&self) -> bool {
        self.segments
            .last()
            .unwrap_or(&PointSegCtx::Root)
            .kind()
            .is_dir()
    }
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
    type Error = UniErr;

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
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(result(all_consuming(parse_star_key)(new_span(s)))?)
    }
}

#[async_trait]
pub trait PointFactory: Send + Sync {
    async fn create(&self) -> Result<Point, UniErr>;
}
