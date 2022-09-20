use core::str::FromStr;

use convert_case::{Case, Casing};
use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};

use cosmic_nom::new_span;

use crate::{KindTemplate, UniErr};
use crate::hyper::ChildRegistry;
use crate::loc::{CONTROL_WAVE_TRAVERSAL_PLAN, MECHTRON_WAVE_TRAVERSAL_PLAN, PORTAL_WAVE_TRAVERSAL_PLAN, ProvisionAffinity, STAR_WAVE_TRAVERSAL_PLAN, StarKey, STD_WAVE_TRAVERSAL_PLAN, ToBaseKind, Version};
use crate::parse::{CamelCase, Domain, kind_parts, SkewerCase};
use crate::particle::traversal::TraversalPlan;
use crate::selector::{Pattern, SpecificSelector, VersionReq};

impl ToBaseKind for KindParts {
    fn to_base(&self) -> BaseKind {
        self.base.clone()
    }
}

impl Tks for KindParts {
    fn base(&self) -> BaseKind {
        self.base.clone()
    }

    fn sub(&self) -> Option<CamelCase> {
        self.sub.clone()
    }

    fn specific(&self) -> Option<Specific> {
        self.specific.clone()
    }

    fn matches(&self, tks: &dyn Tks) -> bool {
        self.base == tks.base() && self.sub == tks.sub() && self.specific == tks.specific()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct KindParts {
    pub base: BaseKind,
    pub sub: Option<CamelCase>,
    pub specific: Option<Specific>,
}

impl KindParts {
    pub fn root() -> Self {
        Self {
            base: BaseKind::Root,
            sub: None,
            specific: None,
        }
    }
}

impl ToString for KindParts {
    fn to_string(&self) -> String {
        if self.sub.is_some() && self.specific.is_some() {
            format!(
                "{}<{}<{}>>",
                self.base.to_string(),
                self.sub.as_ref().expect("sub").to_string(),
                self.specific.as_ref().expect("specific").to_string()
            )
        } else if self.sub.is_some() {
            format!(
                "{}<{}>",
                self.base.to_string(),
                self.sub.as_ref().expect("sub").to_string()
            )
        } else {
            self.base.to_string()
        }
    }
}

impl FromStr for KindParts {
    type Err = UniErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, kind) = all_consuming(kind_parts)(new_span(s))?;

        Ok(kind)
    }
}

impl KindParts {
    pub fn new(kind: BaseKind, sub: Option<CamelCase>, specific: Option<Specific>) -> Self {
        Self {
            base: kind,
            sub,
            specific,
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
)]
pub enum BaseKind {
    Root,
    Space,
    UserBase,
    Base,
    User,
    App,
    Mechtron,
    FileSystem,
    File,
    Database,
    Repo,
    BundleSeries,
    Bundle,
    Artifact,
    Control,
    Portal,
    Star,
    Driver,
    Global,
}

impl BaseKind {
    pub fn to_skewer(&self) -> SkewerCase {
        SkewerCase::from_str(self.to_string().to_case(Case::Kebab).as_str()).unwrap()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum Sub {
    None,
    Database(DatabaseSubKind),
    File(FileSubKind),
    Artifact(ArtifactSubKind),
    UserBase(UserBaseSubKind),
    Star(StarSub),
}

impl Sub {
    pub fn to_camel_case(&self) -> Option<CamelCase> {
        match self {
            Sub::None => None,
            Sub::Database(d) => Some(CamelCase::from_str(d.to_string().as_str()).unwrap()),
            Sub::File(x) => Some(CamelCase::from_str(x.to_string().as_str()).unwrap()),
            Sub::Artifact(x) => Some(CamelCase::from_str(x.to_string().as_str()).unwrap()),
            Sub::UserBase(x) => Some(CamelCase::from_str(x.to_string().as_str()).unwrap()),
            Sub::Star(x) => Some(CamelCase::from_str(x.to_string().as_str()).unwrap()),
        }
    }

    pub fn specific(&self) -> Option<&Specific> {
        match self {
            Sub::Database(sub) => sub.specific(),
            Sub::UserBase(sub) => sub.specific(),
            _ => None,
        }
    }
}

impl Sub {
    pub fn to_skewer(&self) -> SkewerCase {
        SkewerCase::from_str(self.to_string().to_case(Case::Kebab).as_str()).unwrap()
    }
}

impl Into<Option<CamelCase>> for Sub {
    fn into(self) -> Option<CamelCase> {
        match self {
            Sub::None => None,
            Sub::Database(d) => d.into(),
            Sub::File(f) => f.into(),
            Sub::Artifact(a) => a.into(),
            Sub::UserBase(u) => u.into(),
            Sub::Star(s) => s.into(),
        }
    }
}

impl Into<Option<String>> for Sub {
    fn into(self) -> Option<String> {
        match self {
            Sub::None => None,
            Sub::Database(d) => d.into(),
            Sub::File(f) => f.into(),
            Sub::Artifact(a) => a.into(),
            Sub::UserBase(u) => u.into(),
            Sub::Star(s) => s.into(),
        }
    }
}

impl ToBaseKind for BaseKind {
    fn to_base(&self) -> BaseKind {
        self.clone()
    }
}

impl TryFrom<CamelCase> for BaseKind {
    type Error = UniErr;

    fn try_from(base: CamelCase) -> Result<Self, Self::Error> {
        Ok(BaseKind::from_str(base.as_str())?)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum Kind {
    Root,
    Space,
    User,
    App,
    Mechtron,
    FileSystem,
    Repo,
    BundleSeries,
    Bundle,
    Control,
    Portal,
    Driver,
    File(FileSubKind),
    Artifact(ArtifactSubKind),
    Database(DatabaseSubKind),
    Base,
    UserBase(UserBaseSubKind),
    Star(StarSub),
    Global,
}

impl ToBaseKind for Kind {
    fn to_base(&self) -> BaseKind {
        match self {
            Kind::Root => BaseKind::Root,
            Kind::Space => BaseKind::Space,
            Kind::User => BaseKind::User,
            Kind::App => BaseKind::App,
            Kind::Mechtron => BaseKind::Mechtron,
            Kind::FileSystem => BaseKind::FileSystem,
            Kind::BundleSeries => BaseKind::BundleSeries,
            Kind::Bundle => BaseKind::Bundle,
            Kind::Control => BaseKind::Control,
            Kind::Portal => BaseKind::Portal,
            Kind::UserBase(_) => BaseKind::UserBase,
            Kind::File(_) => BaseKind::File,
            Kind::Artifact(_) => BaseKind::Artifact,
            Kind::Database(_) => BaseKind::Database,
            Kind::Base => BaseKind::Base,
            Kind::Repo => BaseKind::Repo,
            Kind::Star(_) => BaseKind::Star,
            Kind::Driver => BaseKind::Driver,
            Kind::Global => BaseKind::Global,
        }
    }
}

impl Kind {
    pub fn to_template(&self) -> KindTemplate {
        KindTemplate {
            base: self.to_base(),
            sub: self.sub().to_camel_case(),
            specific: self.specific_selector(),
        }
    }

    pub fn provision_affinity(&self) -> ProvisionAffinity {
        match self.to_base() {
            BaseKind::Base => ProvisionAffinity::Local,
            _ => ProvisionAffinity::Wrangle,
        }
    }

    pub fn as_point_segments(&self) -> String {
        if Sub::None != self.sub() {
            if let Some(specific) = self.specific() {
                format!(
                    "{}:{}:{}",
                    self.to_base().to_skewer().to_string(),
                    self.sub().to_skewer().to_string(),
                    specific.to_string()
                )
            } else {
                format!(
                    "{}:{}",
                    self.to_base().to_skewer().to_string(),
                    self.sub().to_skewer().to_string()
                )
            }
        } else {
            format!("{}", self.to_base().to_skewer().to_string())
        }
    }

    pub fn sub(&self) -> Sub {
        match self {
            Kind::File(s) => s.clone().into(),
            Kind::Artifact(s) => s.clone().into(),
            Kind::Database(s) => s.clone().into(),
            _ => Sub::None,
        }
    }

    pub fn specific(&self) -> Option<Specific> {
        let sub = self.sub();
        sub.specific().cloned()
    }

    pub fn specific_selector(&self) -> Option<SpecificSelector> {
        match self.specific() {
            None => None,
            Some(specific) => Some(specific.to_selector()),
        }
    }

    pub fn wave_traversal_plan(&self) -> &TraversalPlan {
        match self {
            Kind::Mechtron => &MECHTRON_WAVE_TRAVERSAL_PLAN,
            Kind::Portal => &PORTAL_WAVE_TRAVERSAL_PLAN,
            Kind::Control => &CONTROL_WAVE_TRAVERSAL_PLAN,
            Kind::Star(_) => &STAR_WAVE_TRAVERSAL_PLAN,
            _ => &STD_WAVE_TRAVERSAL_PLAN,
        }
    }
}

impl TryFrom<KindParts> for Kind {
    type Error = UniErr;

    fn try_from(value: KindParts) -> Result<Self, Self::Error> {
        Ok(match value.base {
            BaseKind::Database => {
                match value.sub.ok_or("Database<?> requires a Sub Kind")?.as_str() {
                    "Relational" => Kind::Database(DatabaseSubKind::Relational(
                        value
                            .specific
                            .ok_or("Database<Relational<?>> requires a Specific")?,
                    )),
                    what => {
                        return Err(UniErr::from(format!(
                            "unexpected Database SubKind '{}'",
                            what
                        )));
                    }
                }
            }
            BaseKind::UserBase => {
                match value.sub.ok_or("UserBase<?> requires a Sub Kind")?.as_str() {
                    "OAuth" => Kind::UserBase(UserBaseSubKind::OAuth(
                        value
                            .specific
                            .ok_or("UserBase<OAuth<?>> requires a Specific")?,
                    )),
                    what => {
                        return Err(UniErr::from(format!(
                            "unexpected Database SubKind '{}'",
                            what
                        )));
                    }
                }
            }
            BaseKind::Base => Kind::Base,
            BaseKind::File => Kind::File(FileSubKind::from_str(
                value.sub.ok_or("File<?> requires a Sub Kind")?.as_str(),
            )?),
            BaseKind::Artifact => Kind::Artifact(ArtifactSubKind::from_str(
                value.sub.ok_or("Artifact<?> requires a sub kind")?.as_str(),
            )?),

            BaseKind::Star => Kind::Star(StarSub::from_str(
                value.sub.ok_or("Star<?> requires a sub kind")?.as_str(),
            )?),

            BaseKind::Root => Kind::Root,
            BaseKind::Space => Kind::Space,
            BaseKind::User => Kind::User,
            BaseKind::App => Kind::App,
            BaseKind::Mechtron => Kind::Mechtron,
            BaseKind::FileSystem => Kind::FileSystem,

            BaseKind::BundleSeries => Kind::BundleSeries,
            BaseKind::Bundle => Kind::Bundle,
            BaseKind::Control => Kind::Control,
            BaseKind::Portal => Kind::Portal,
            BaseKind::Repo => Kind::Repo,
            BaseKind::Driver => Kind::Driver,
            BaseKind::Global => Kind::Global,
        })
    }
}

/// Stands for "Type, Kind, Specific"
pub trait Tks {
    fn base(&self) -> BaseKind;
    fn sub(&self) -> Option<CamelCase>;
    fn specific(&self) -> Option<Specific>;
    fn matches(&self, tks: &dyn Tks) -> bool;
}

#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum StarSub {
    Central,
    Super, // Wrangles nearby Stars... manages Assigning Particles to Stars, Moving, Icing, etc.
    Nexus, // Relays Waves from Star to Star
    Maelstrom, // Where executables are run
    Scribe, // requires durable filesystem (Artifact Bundles, Files...)
    Jump, // for entry into the Mesh/Fabric for an external connection (client ingress... http for example)
    Fold, // exit from the Mesh.. maintains connections etc to Databases, Keycloak, etc.... Like A Space Fold out of the Fabric..
    Machine, // every Machine has one and only one Machine star... it handles messaging for the Machine
}

impl StarSub {
    pub fn is_forwarder(&self) -> bool {
        match self {
            StarSub::Nexus => true,
            StarSub::Central => false,
            StarSub::Super => true,
            StarSub::Maelstrom => true,
            StarSub::Scribe => true,
            StarSub::Jump => true,
            StarSub::Fold => true,
            StarSub::Machine => false,
        }
    }

    pub fn can_be_wrangled(&self) -> bool {
        match self {
            StarSub::Nexus => false,
            StarSub::Machine => false,
            _ => true,
        }
    }
}

impl Into<Sub> for StarSub {
    fn into(self) -> Sub {
        Sub::Star(self)
    }
}

impl Into<Option<CamelCase>> for StarSub {
    fn into(self) -> Option<CamelCase> {
        Some(CamelCase::from_str(self.to_string().as_str()).unwrap())
    }
}

impl Into<Option<String>> for StarSub {
    fn into(self) -> Option<String> {
        Some(self.to_string())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize, strum_macros::Display)]
pub enum UserBaseSubKind {
    OAuth(Specific),
}

impl UserBaseSubKind {
    pub fn specific(&self) -> Option<&Specific> {
        match self {
            UserBaseSubKind::OAuth(specific) => Option::Some(specific),
        }
    }
}

impl Into<Sub> for UserBaseSubKind {
    fn into(self) -> Sub {
        Sub::UserBase(self)
    }
}

impl Into<Option<CamelCase>> for UserBaseSubKind {
    fn into(self) -> Option<CamelCase> {
        Some(CamelCase::from_str(self.to_string().as_str()).unwrap())
    }
}

impl Into<Option<String>> for UserBaseSubKind {
    fn into(self) -> Option<String> {
        Some(self.to_string())
    }
}

#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum FileSubKind {
    File,
    Dir,
}

impl Into<Sub> for FileSubKind {
    fn into(self) -> Sub {
        Sub::File(self)
    }
}

impl Into<Option<CamelCase>> for FileSubKind {
    fn into(self) -> Option<CamelCase> {
        Some(CamelCase::from_str(self.to_string().as_str()).unwrap())
    }
}

impl Into<Option<String>> for FileSubKind {
    fn into(self) -> Option<String> {
        Some(self.to_string())
    }
}

#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum ArtifactSubKind {
    Raw,
    ParticleConfig,
    Bind,
    Wasm,
    Dir,
}

impl Into<Sub> for ArtifactSubKind {
    fn into(self) -> Sub {
        Sub::Artifact(self)
    }
}

impl Into<Option<CamelCase>> for ArtifactSubKind {
    fn into(self) -> Option<CamelCase> {
        Some(CamelCase::from_str(self.to_string().as_str()).unwrap())
    }
}

impl Into<Option<String>> for ArtifactSubKind {
    fn into(self) -> Option<String> {
        Some(self.to_string())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize, strum_macros::Display)]
pub enum DatabaseSubKind {
    Relational(Specific),
}

impl DatabaseSubKind {
    pub fn specific(&self) -> Option<&Specific> {
        match self {
            DatabaseSubKind::Relational(specific) => Some(specific),
        }
    }
}

impl Into<Sub> for DatabaseSubKind {
    fn into(self) -> Sub {
        Sub::Database(self)
    }
}

impl Into<Option<CamelCase>> for DatabaseSubKind {
    fn into(self) -> Option<CamelCase> {
        Some(CamelCase::from_str(self.to_string().as_str()).unwrap())
    }
}

impl Into<Option<String>> for DatabaseSubKind {
    fn into(self) -> Option<String> {
        Some(self.to_string())
    }
}

impl BaseKind {
    pub fn child_resource_registry_handler(&self) -> ChildRegistry {
        match self {
            Self::UserBase => ChildRegistry::Core,
            _ => ChildRegistry::Shell,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct StarStub {
    pub key: StarKey,
    pub kind: StarSub,
}

impl StarStub {
    pub fn new(key: StarKey, kind: StarSub) -> Self {
        Self { key, kind }
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