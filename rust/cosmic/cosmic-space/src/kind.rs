use crate::err::ParseErrs;
use crate::err::SpaceErr;
use crate::point::Version;
use crate::selector::VersionReq;
use cosmic_nom::Tw;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use crate::model::{CamelCase, Domain, SkewerCase};
use crate::particle::traversal::TraversalPlan;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct SubTypeDef<Part, SubType> {
    pub part: Part,
    pub sub: SubType,
    pub r#type: SubType,
}


impl<Part, SubType, IsMatchPart, IsMatchSubType> IsMatch<SubTypeDef<Part, SubType>>
    for SubTypeDef<IsMatchPart, IsMatchSubType>
where
    IsMatchPart: IsMatch<Part>,
    IsMatchSubType: IsMatch<SubType>,
    Part: Eq + PartialEq,
    SubType: Eq + PartialEq,
{
    fn is_match(&self, other: &SubTypeDef<Part, SubType>) -> bool {
        self.part.is_match(&other.part)
            && self.sub.is_match(&other.sub)
            && self.r#type.is_match(&other.r#type)
    }
}

impl<Part, SubType> SubTypeDef<Part, SubType> {
    pub fn with_sub(self, sub: SubType) -> Self {
        Self {
            part: self.part,
            r#type: self.r#type,
            sub,
        }
    }

    pub fn with_type(self, r#type: SubType) -> Self {
        Self {
            part: self.part,
            sub: self.sub,
            r#type,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct ParentChildDef<Parent, Child> {
    pub parent: Parent,
    pub child: Child,
}



impl<Parent, Child, IsMatchParent, IsMatchChild> IsMatch<ParentChildDef<Parent, Child>>
    for ParentChildDef<IsMatchParent, IsMatchChild>
where
    IsMatchParent: IsMatch<Parent>,
    IsMatchChild: IsMatch<Child>,
    Parent: Eq + PartialEq,
    Child: Eq + PartialEq,
{
    fn is_match(&self, other: &ParentChildDef<Parent, Child>) -> bool {
        self.parent.is_match(&other.parent) && self.child.is_match(&other.child)
    }
}

impl<Parent, Child> Default for ParentChildDef<Parent, Child>
where
    Parent: Default,
    Child: Default,
{
    fn default() -> Self {
        Self {
            ..Default::default()
        }
    }
}

impl Default for Kind {
    fn default() -> Self {
        todo!()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Variant {
    Native(Native),
    Artifact(Artifact),
    Star(StarVariant),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Sub {
    Artifact(ArtifactSub),
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
pub enum ArtifactSub {
    Repo,
    Series,
    Bundle,
    Dir,
    File,
}

impl Variant {
    pub fn from(kind: &KindCat, variant: &CamelCase) -> Result<Self, SpaceErr> {
        match kind {
            what => Err(format!(
                "kind '{}' does not have a variant '{}' ",
                kind.to_string(),
                variant.to_string()
            )
            .into()),
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
pub enum Native {
    Web,
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
pub enum Artifact {
    Disk,
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
pub enum StarVariant {
    Central,
    Super, // Wrangles nearby Stars... manages Assigning Particles to Stars, Moving, Icing, etc.
    Nexus, // Relays Waves from Star to Star
    Maelstrom, // Where executables are run
    Scribe, // requires durable filesystem (Artifact Bundles, Files...)
    Jump, // for entry into the Mesh/Fabric for an external connection (client ingress... http for example)
    Fold, // exit from the Mesh.. maintains connections etc to Databases, Keycloak, etc.... Like A Space Fold out of the Fabric..
    Machine, // every Machine has one and only one Machine star... it handles messaging for the Machine
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
pub enum Db {
    Rel,
}

impl Variant {
    pub fn to_sub_types(self) -> VariantSubTypes {
        VariantSubTypes {
            part: self,
            sub: None,
            r#type: None,
        }
    }

    pub fn with_specific(self, specific: Option<SpecificSubTypes>) -> VariantFull {
        VariantFull {
            parent: self.to_sub_types(),
            child: specific,
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
pub enum KindCat {
    Root,
    Space,
    Base,
    Account,
    Mechtron,
    Artifact,
    Control,
    Portal,
    Star,
    Driver,
    Global,
    Native,
}

impl KindCat {
    pub fn to_sub_types(self) -> KindSubTypes {
        KindSubTypes {
            part: self,
            sub: None,
            r#type: None,
        }
    }

    pub fn with_variant(self, variant: Option<VariantFull>) -> Kind {
        Kind {
            parent: self.to_sub_types(),
            child: variant,
        }
    }
}

impl Default for KindCat {
    fn default() -> Self {
        Self::Root
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Hash)]
pub struct SpecificDef<Domain, Skewer, Version> {
    pub provider: Domain,
    pub vendor: Domain,
    pub product: Skewer,
    pub variant: Skewer,
    pub version: Version,
}

/// A Specific is used to extend the Kind system in The Cosmic Initiative to a very exact level.
/// when a Kind has a specific it is not only referencing something general like a Database,
/// but the vendor, product and version of that database among other things.
/// The Specific def looks like this `provider.url:vendor.url:product:variant:version`
/// * **provider** - this is the domain name of the person or entity that provided the driver
///                  that this specific defines
/// * **vendor** - the vendor that provides the product which may have had nothing to do with
///                creating the driver
/// * **product** - the product
/// * **variant** - many products have variation and here it is where it is specificied
/// * **version** - this is a SemVer describing the exact version of the Specific
///
/// ## Example:
/// `mechtronhub.com:postgres.org:postgres:gis:8.0.0`
/// And the above would be embedde into the appropriate Base Kind and Sub Kind:
/// `<Database<Rel<mechtronhub.com:postgres.org:postgres:gis:8.0.0>>>`}
pub type Specific = SpecificDef<Domain, SkewerCase, Version>;

impl Specific {
    pub fn new(
        provider: Domain,
        vendor: Domain,
        product: SkewerCase,
        variant: SkewerCase,
        version: Version,
    ) -> Self {
        Self {
            provider,
            vendor,
            product,
            variant,
            version,
        }
    }

    pub fn to_full(self) -> SpecificSubTypes {
        SpecificSubTypes {
            part: self,
            sub: None,
            r#type: None,
        }
    }

    pub fn sub(self, sub: Option<CamelCase>) -> SpecificSubTypes {
        SpecificSubTypes {
            part: self,
            sub,
            r#type: None,
        }
    }

    pub fn sub_type(self, sub: Option<CamelCase>, r#type: Option<CamelCase>) -> SpecificSubTypes {
        SpecificSubTypes {
            part: self,
            sub,
            r#type,
        }
    }
}
pub type VariantSubTypes = SubTypeDef<Variant, Option<CamelCase>>;
pub type VariantFullSubTypesSelector = SubTypeDef<Pattern<Variant>, Pattern<SpecificFullSelector>>;

pub type SpecificSubTypes = SubTypeDef<Specific, Option<CamelCase>>;
pub type SpecificSubTypesSelector = SubTypeDef<Pattern<SpecificSelector>, OptPattern<CamelCase>>;

pub type VariantDef<Variant, Specific> = ParentChildDef<Variant, Specific>;
pub type VariantFull = VariantDef<VariantSubTypes, Option<SpecificSubTypes>>;
pub type ProtoVariant = VariantDef<Tw<CamelCaseSubTypes>, Tw<Option<SpecificSubTypes>>>;
pub type KindDef<Kind, Variant> = ParentChildDef<Kind, Variant>;
pub type CamelCaseSubTypes = SubTypeDef<CamelCase, Option<CamelCase>>;
pub type CamelCaseSubTypesSelector = SubTypeDef<Pattern<CamelCase>, OptPattern<CamelCase>>;
pub type KindSubTypes = SubTypeDef<KindCat, Option<CamelCase>>;
pub type Kind = KindDef<KindSubTypes, Option<VariantFull>>;
pub type ProtoKind = KindDef<Tw<CamelCaseSubTypes>, Tw<Option<ProtoVariant>>>;
pub type KindFullSubTypesSelector = SubTypeDef<Pattern<KindCat>, OptPattern<CamelCase>>;

impl Kind {
    pub fn wave_traversal_plan(&self) -> TraversalPlan {
        todo!()
    }
}

impl ToString for VariantFull {
    fn to_string(&self) -> String {
        todo!()
    }
}

impl ToString for Kind {
    fn to_string(&self) -> String {
        if let Some(child) = &self.child {
            format!("{}<{}>", self.parent.to_string(), child.to_string() )
        } else {
            format!("{}", self.parent.to_string() )
        }
    }
}

impl ToString for KindSubTypes {
    fn to_string(&self) -> String {
        if let Some(sub) = &self.sub {
            format!("{}:{}", self.part.to_string(), sub.to_string() )
        } else {
            self.part.to_string()
        }
    }
}


impl ProtoKind {
    pub fn to_full(&self) -> Result<Kind, SpaceErr> {
        let kind: KindCat = KindCat::from_str(self.parent.part.as_str())?;
        match kind {
            KindCat::Artifact => {
                let sub = self
                    .parent
                    .sub
                    .as_ref()
                    .ok_or::<SpaceErr>(ParseErrs::from_trace(
                        "Artifact expecting Sub kind:  i.e. <Artifact:File>",
                        "missing SubKind",
                        &self.parent.trace,
                    ))?;
                let sub: ArtifactSub = ArtifactSub::from_str(sub.as_str()).map_err(|e| {
                    ParseErrs::from_trace(
                        "invalid artifact SubKind",
                        "invalid artifact SubKind",
                        &self.parent.trace,
                    )
                })?;
                match &sub {
                    ArtifactSub::Repo => {
                        if let Some(variant) = self.child.as_ref() {
                            Ok(Kind {
                                parent: SubTypeDef {
                                    part: KindCat::Artifact,
                                    sub: Some(CamelCase::from_str("Repo")?),
                                    r#type: None,
                                },
                                child: Some(variant.to_full(&kind)?),
                            })
                        } else {
                            Err(ParseErrs::from_trace(
                                "Artifact:Repo is required to have a Variant [Disk,S3]",
                                "missing Variant",
                                &self.parent.trace,
                            )
                            .into())
                        }
                    }
                    _ => {
                        if self.child.is_some() {
                            Err(ParseErrs::from_trace(
                                format!("Artifact:{} does not have any Variants", sub.to_string())
                                    .as_str(),
                                "unexpected variant",
                                &self.parent.trace,
                            )
                            .into())
                        } else {
                            Ok(Kind {
                                parent: SubTypeDef {
                                    part: KindCat::Artifact,
                                    sub: Some(CamelCase::from_str(sub.to_string().as_str())?),
                                    r#type: None,
                                },
                                child: None,
                            })
                        }
                    }
                }
            }
            KindCat::Star => {
                let star = self.child.as_ref().ok_or(ParseErrs::from_trace(
                    "Star Kind expected to have a StarVariant i.e. Star<Maelstrom>",
                    "missing Variant ",
                    &self.parent.trace,
                ))?;

                Ok(Kind {
                    child: Some(star.to_full(&kind)?),
                    parent: SubTypeDef {
                        part: kind,
                        sub: None,
                        r#type: None,
                    },
                })
            }
            KindCat::Native => {
                let native = self
                    .child
                    .as_ref()
                    .ok_or("Native Kind expected to have a Native variant")?;

                let star = self.child.as_ref().ok_or(ParseErrs::from_trace(
                    "Native Kind expected to have a Native variant",
                    "missing Variant ",
                    &self.parent.trace,
                ))?;

                Ok(Kind {
                    child: Some(native.to_full(&kind)?),
                    parent: SubTypeDef {
                        part: kind,
                        sub: None,
                        r#type: None,
                    },
                })
            }
            kind => Ok(Kind {
                parent: SubTypeDef {
                    part: kind,
                    sub: None,
                    r#type: None,
                },
                child: None,
            }),
        }
    }
}

impl ProtoVariant {
    pub fn to_full(&self, kind: &KindCat) -> Result<VariantFull, SpaceErr> {
        let mut variant = match kind {
            KindCat::Artifact => {
                let artifact: Artifact = Artifact::from_str(self.parent.part.as_str())?;

                VariantFull {
                    parent: SubTypeDef {
                        part: Variant::Artifact(artifact),
                        sub: None,
                        r#type: None,
                    },
                    child: None,
                }
            }
            KindCat::Star => {
                let star: StarVariant = StarVariant::from_str(self.parent.part.as_str())?;

                VariantFull {
                    parent: SubTypeDef {
                        part: Variant::Star(star),
                        sub: None,
                        r#type: None,
                    },
                    child: None,
                }
            }
            KindCat::Native => {
                let native: Native = Native::from_str(self.parent.part.as_str())?;
                VariantFull {
                    parent: SubTypeDef {
                        part: Variant::Native(native),
                        sub: None,
                        r#type: None,
                    },
                    child: None,
                }
            }
            kind => {
                return Err(format!("{} does not have a variant", kind.to_string()).into());
            }
        };
        Ok(variant)
    }
}

pub type ParentMatcherDef<Matcher, Child, SubTypeMatcher> =
    ParentChildDef<SubTypeDef<Matcher, SubTypeMatcher>, Child>;

pub trait IsMatch<X>
where
    X: Eq + PartialEq,
{
    fn is_match(&self, other: &X) -> bool;
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub enum Pattern<X> {
    None,
    Any,
    Matches(X),
}

impl<X> IsMatch<X> for Pattern<X>
where
    X: Eq + PartialEq,
{
    fn is_match(&self, other: &X) -> bool {
        match self {
            Pattern::None => false,
            Pattern::Any => true,
            Pattern::Matches(x) => x.eq(other),
        }
    }
}

impl<X> ToString for Pattern<X>
where
    X: ToString,
{
    fn to_string(&self) -> String {
        match self {
            Pattern::None => "!".to_string(),
            Pattern::Any => "*".to_string(),
            Pattern::Matches(x) => x.to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub enum OptPattern<X> {
    None,
    Any,
    Matches(X),
}

impl<X> IsMatch<Option<X>> for OptPattern<X>
where
    X: Eq + PartialEq,
{
    fn is_match(&self, other: &Option<X>) -> bool {
        match self {
            Self::None => other.is_none(),
            Self::Any => true,
            Self::Matches(x) => match other {
                None => false,
                Some(o) => *x == *o,
            },
        }
    }
}

impl<X> ToString for OptPattern<X>
where
    X: ToString,
{
    fn to_string(&self) -> String {
        match self {
            Self::None => "!".to_string(),
            Self::Any => "*".to_string(),
            Self::Matches(x) => x.to_string(),
        }
    }
}

impl IsMatch<Version> for VersionReq {
    fn is_match(&self, other: &Version) -> bool {
        self.version.matches(&other.version)
    }
}

pub type DomainSelector = Pattern<Domain>;
pub type SkewerSelector = Pattern<SkewerCase>;
pub type VersionSelector = Pattern<VersionReq>;
pub type SpecificSelector = SpecificDef<DomainSelector, SkewerSelector, VersionSelector>;
pub type SpecificFullSelector = SubTypeDef<SpecificSelector, OptPattern<CamelCase>>;

impl SpecificSelector {
    pub fn to_full(self) -> SpecificFullSelector {
        SpecificFullSelector {
            part: self,
            sub: OptPattern::None,
            r#type: OptPattern::None,
        }
    }
}

impl IsMatch<Specific> for SpecificSelector {
    fn is_match(&self, other: &Specific) -> bool {
        self.provider.is_match(&other.provider)
            && self.vendor.is_match(&other.vendor)
            && self.product.is_match(&other.product)
            && self.variant.is_match(&other.variant)
    }
}
pub type ProtoVariantSelector =
    VariantDef<Tw<Pattern<CamelCaseSubTypesSelector>>, Tw<OptPattern<SpecificSelector>>>;

pub type ProtoKindSelector =
    KindDef<Tw<Pattern<CamelCaseSubTypesSelector>>, Tw<OptPattern<ProtoVariantSelector>>>;

pub type VariantSelector =
    VariantDef<Pattern<VariantFullSubTypesSelector>, OptPattern<SpecificSelector>>;

pub type KindSelector =
    KindDef<Pattern<KindFullSubTypesSelector>, OptPattern<VariantSelector>>;

pub mod parse {

    use crate::kind::{
        CamelCaseSubTypes, CamelCaseSubTypesSelector, KindDef, OptPattern, ParentChildDef,
        Pattern, ProtoKind, ProtoKindSelector, ProtoVariant, ProtoVariantSelector, Specific,
        SpecificDef, SpecificFullSelector, SpecificSelector, SpecificSubTypes,
        SpecificSubTypesSelector, SubTypeDef, VariantDef, VariantSelector,
    };
    use crate::parse::{camel_case, domain, skewer_case, version, version_req};
    use crate::selector::specific::ProductVariantSelector;
    use cosmic_nom::{Res, Span, tw};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::{eof, fail, opt, success, value};
    use nom::sequence::{delimited, pair, preceded, tuple};
    use std::str::FromStr;
    use crate::model::{CamelCase, Domain};

    #[cfg(test)]
    pub mod test {
        use crate::kind::{IsMatch, OptPattern, Pattern};

        use crate::err::SpaceErr;
        use crate::parse::error::result;
        use crate::parse::{
            camel_case, domain, expect, rec_version, skewer, version, version_req,
        };
        use crate::util::log;
        use crate::util::test::verify;
        use core::str::FromStr;
        use cosmic_nom::new_span;
        use nom::bytes::complete::tag;
        use nom::combinator::{all_consuming, opt};
        use nom::sequence::{delimited, pair, preceded};
        use crate::model::CamelCase;
        use crate::parse::kind::{camel_case_sub_types, camel_case_sub_types_selector, kind_selector, opt_pattern, pattern, preceded_opt_pattern, proto_kind, proto_variant, specific, specific_full_selector, specific_selector, specific_sub_types, variant_selector};

        #[test]
        pub fn test_camel_case_subtypes() {
            let r = result(expect(camel_case_sub_types)(new_span(
                "SomeCamelCase:Sub:Type",
            )))
            .unwrap();
        }

        #[test]
        pub fn test_camel_case_subtypes_selector() {
            let r = result(camel_case_sub_types_selector(new_span(
                "SomeCamelCase:*:Type",
            )))
            .unwrap();
            match r.sub {
                OptPattern::Any => {}
                _ => assert!(false),
            }
        }

        #[test]
        pub fn test_my_sub() {
            let sub = log(result(opt_pattern(camel_case)(new_span("MySub")))).unwrap();
            assert_eq!(
                sub,
                OptPattern::Matches(CamelCase::from_str("MySub").unwrap())
            );

            let sub = log(result(preceded_opt_pattern(|i| tag(":")(i), camel_case)(
                new_span(":MySub"),
            )))
            .unwrap();
            assert_eq!(
                sub,
                OptPattern::Matches(CamelCase::from_str("MySub").unwrap())
            );

            let (blah, sub) = log(result(pair(
                camel_case,
                opt(preceded(tag(":"), camel_case)),
            )(new_span("Blah:MySub"))))
            .unwrap();
            assert!(sub.is_some());

            let (blah, sub) = log(result(pair(
                camel_case,
                preceded_opt_pattern(|i| tag(":")(i), camel_case),
            )(new_span("Blah:MySub"))))
            .unwrap();
            assert!(sub.is_match(&Some(CamelCase::from_str("MySub").unwrap())))
        }

        #[test]
        pub fn test_specific() {
            let specific = result(specific(new_span(
                "my-domain.io:vendor.io:product:variant:1.0.0",
            )))
            .unwrap();
        }

        #[test]
        pub fn test_specific_selector() {
            let selector = log(result(specific_selector(new_span(
                "my-domain.io:*:product:variant:(1.0.0)",
            ))))
            .unwrap();
        }

        #[test]
        pub fn test_specific_sub_types() {
            let specific = result(specific_sub_types(new_span(
                "my-domain.io:vendor.io:product:variant:1.0.0:Sub:Type",
            )))
            .unwrap();
            assert_eq!(specific.sub, Some(CamelCase::from_str("Sub").unwrap()));
            assert_eq!(specific.r#type, Some(CamelCase::from_str("Type").unwrap()));
        }

        #[test]
        pub fn test_specific_full_selector() {
            let selector = log(result(specific_full_selector(new_span(
                "my-domain.io:*:product:variant:(1.0.0)",
            ))))
            .unwrap();

            assert_eq!(selector.sub, OptPattern::None);
            assert_eq!(selector.part.variant.to_string(), "variant".to_string());
            //            assert_eq!(selector.part.version,Pattern::Matches(VersionReq::from_str("1.0.0").unwrap()));

            let selector = log(result(specific_full_selector(new_span(
                "my-domain.io:*:product:variant:(1.0.0):MySub",
            ))))
            .unwrap();

            assert_eq!(
                selector.sub,
                OptPattern::Matches(CamelCase::from_str("MySub").unwrap())
            );
        }

        #[test]
        pub fn test_proto_variant() {
            let variant = log(result(proto_variant(new_span("Variant")))).unwrap();
            assert!(variant.child.is_none());

            let variant = log(result(proto_variant(new_span(
                "Variant<some.com:go.com:yesterday:tomorrow:1.0.0>",
            ))))
            .unwrap();
            assert!(variant.child.is_some());

            let variant = log(result(proto_variant(new_span("Variant:Sub")))).unwrap();
            assert_eq!(variant.parent.part, CamelCase::from_str("Variant").unwrap());
            assert!(variant.parent.sub.is_some());

            let variant = log(result(proto_variant(new_span(
                "Variant:Sub<some.com:go.com:yesterday:tomorrow:1.0.0>",
            ))))
            .unwrap();
            assert!(variant.child.is_some());
            assert!(variant.parent.sub.is_some());
        }

        #[test]
        pub fn test_proto_kind() {
            let kind = log(result(proto_kind(new_span("Root")))).unwrap();

            // Repo takes a Variant
            let kind = log(result(proto_kind(new_span("Artifact:Repo<Disk>")))).unwrap();
            assert!(kind.to_full().is_ok());
            kind.to_full().unwrap();

            // File does not take a Disk Variant
            let kind = log(result(proto_kind(new_span("Artifact:File<Disk>")))).unwrap();
            let err: SpaceErr = log(kind.to_full()).unwrap_err();

            verify("artifact_file_disk", &err);

            let kind = log(result(proto_kind(new_span("Artifact:File")))).unwrap();
            assert!(kind.to_full().is_ok());

            let kind = log(result(proto_kind(new_span("NotExist<Variant>")))).unwrap();
            let err: SpaceErr = log(kind.to_full()).unwrap_err();
            verify("kind_not_exist", &err);
            assert!(kind.child.is_some());

            let kind = log(result(proto_kind(new_span("Artifact")))).unwrap();
            let err: SpaceErr = log(kind.to_full()).unwrap_err();
            verify("require-sub-and-variant", &err);
        }
        #[test]
        pub fn test_variant_selector() {
            let variant = log(result(variant_selector(new_span("Variant")))).unwrap();
            let variant = log(result(variant_selector(new_span(
                "Variant<semyon.org:semyon.org:semyon:semyon:(1.0.0)>",
            ))))
            .unwrap();
            let variant = log(result(variant_selector(new_span("Variant<*>")))).unwrap();
        }

        #[test]
        pub fn test_kind_selector() {
            let kind = log(result(kind_selector(new_span("Root")))).unwrap();
            let kind = log(result(kind_selector(new_span("Root<Variant>>")))).unwrap();
            let kind = log(result(kind_selector(new_span(
                "Root<Variant<semyon.org:semyon.org:semyon:semyon:(1.0.0)>>>",
            ))))
            .unwrap();
        }

        #[test]
        pub fn test_camel_case_subtypes_err() {
            assert!(log(result(expect(camel_case_sub_types)(new_span(
                "someCamelCase:Sub:Type"
            ))))
            .is_err());
        }
    }
}

#[cfg(test)]
pub mod test {
    use crate::kind::{
        Artifact, DomainSelector, IsMatch, KindCat, OptPattern, Pattern, SkewerSelector, Specific,
        SpecificSelector, SpecificSubTypes, SubTypeDef, Variant, VariantFull, VariantSelector,
        VersionSelector,
    };
    use crate::point::Version;
    use crate::selector::VersionReq;
    use core::str::FromStr;
    use crate::model::{CamelCase, Domain, SkewerCase};

    fn create_specific() -> Specific {
        Specific::new(
            Domain::from_str("my-domain.com").unwrap(),
            Domain::from_str("my-domain.com").unwrap(),
            SkewerCase::from_str("product").unwrap(),
            SkewerCase::from_str("variant").unwrap(),
            Version::from_str("1.0.0").unwrap(),
        )
    }

    fn create_specific_sub_type() -> SpecificSubTypes {
        create_specific().sub(Some(CamelCase::from_str("Blah").unwrap()))
    }

    fn create_variant_full() -> VariantFull {
        Variant::Artifact(Artifact::Disk).with_specific(Some(create_specific_sub_type()))
    }

    #[test]
    pub fn specific() {
        let specific1 = create_specific();
        let specific2 = create_specific();
        assert_eq!(specific1, specific2);

        let spec1 = create_specific_sub_type();
        let spec2 = create_specific_sub_type();
        assert_eq!(spec1, spec2);
    }

    #[test]
    pub fn variant() {
        let var1 =
            Variant::Artifact(Artifact::Disk).with_specific(Some(create_specific_sub_type()));
        let var2 =
            Variant::Artifact(Artifact::Disk).with_specific(Some(create_specific_sub_type()));
        assert_eq!(var1, var2);
    }

    #[test]
    pub fn kind() {
        let kind1 = KindCat::Root.with_variant(Some(create_variant_full()));
        let kind2 = KindCat::Root.with_variant(Some(create_variant_full()));
        assert_eq!(kind1, kind2);
    }

    #[test]
    pub fn specific_selector() {
        let specific = create_specific();
        let selector = SpecificSelector {
            provider: DomainSelector::Any,
            vendor: DomainSelector::Matches(Domain::from_str("my-domain.com").unwrap()),
            product: SkewerSelector::Any,
            variant: SkewerSelector::Matches(SkewerCase::from_str("variant").unwrap()),
            version: VersionSelector::Matches(VersionReq::from_str("^1.0.0").unwrap()),
        };

        assert!(selector.is_match(&specific));

        let mut specific = specific.to_full();
        let mut selector = selector.to_full();

        assert!(selector.is_match(&specific));

        let specific = specific.with_sub(Some(CamelCase::from_str("Zophis").unwrap()));
        assert!(!selector.is_match(&specific));
        let selector = selector.with_sub(OptPattern::Any);
        assert!(selector.is_match(&specific));

        let selector = SpecificSelector {
            provider: DomainSelector::Any,
            vendor: DomainSelector::Matches(Domain::from_str("my-domain.com").unwrap()),
            product: SkewerSelector::Any,
            variant: SkewerSelector::Matches(SkewerCase::from_str("variant").unwrap()),
            version: VersionSelector::Matches(VersionReq::from_str("^1.0.0").unwrap()),
        };

        let specific = create_specific();

        assert!(selector.is_match(&specific));
    }
}
