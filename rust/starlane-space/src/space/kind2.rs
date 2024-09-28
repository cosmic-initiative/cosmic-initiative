use crate::space::loc::Version;
use crate::space::parse::{CamelCase, Domain, SkewerCase};
use crate::space::selector::VersionReq;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

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

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum Variant {
    Native(Native),
    Artifact(Artifact),
    Db(Db),
    Star(StarVariant),
}

impl Variant {
    pub fn from(kind: &Kind, variant: &CamelCase) -> err::Result<Self> {
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
    Repo,
    Bundle,
    Series,
    Dir,
    File,
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
pub enum Kind {
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

impl Kind {
    pub fn to_sub_types(self) -> KindSubTypes {
        KindSubTypes {
            part: self,
            sub: None,
            r#type: None,
        }
    }

    pub fn with_variant(self, variant: Option<VariantFull>) -> KindFull {
        KindFull {
            parent: self.to_sub_types(),
            child: variant,
        }
    }
}

impl Default for Kind {
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

pub type SpecificSubTypes = SubTypeDef<Specific, Option<CamelCase>>;
pub type SpecificSubTypesSelector = SubTypeDef<Pattern<SpecificSelector>, OptPattern<CamelCase>>;

pub type VariantDef<Variant, Specific> = ParentChildDef<Variant, Specific>;
pub type VariantFull = VariantDef<VariantSubTypes, Option<SpecificSubTypes>>;
pub type ProtoVariant = VariantDef<CamelCaseSubTypes, Option<SpecificSubTypes>>;
pub type ProtoVariantSelector = VariantDef<CamelCaseSubTypes, SpecificSelector>;
pub type KindDef<Kind, Variant> = ParentChildDef<Kind, Variant>;
pub type CamelCaseSubTypes = SubTypeDef<CamelCase, Option<CamelCase>>;
pub type CamelCaseSubTypesSelector = SubTypeDef<Pattern<CamelCase>, OptPattern<CamelCase>>;
pub type KindSubTypes = SubTypeDef<Kind, Option<CamelCase>>;
pub type KindFull = KindDef<KindSubTypes, Option<VariantFull>>;
pub type ProtoKind = KindDef<CamelCaseSubTypes, Option<ProtoVariant>>;

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

pub type VariantFullSelector =
    ParentMatcherDef<Pattern<Variant>, OptPattern<SpecificSubTypes>, OptPattern<CamelCase>>;
pub type KindFullSelector =
    ParentMatcherDef<Pattern<Kind>, OptPattern<VariantFullSelector>, OptPattern<CamelCase>>;

pub mod parse {
    use crate::space::kind2::{
        CamelCaseSubTypes, CamelCaseSubTypesSelector, KindDef, OptPattern, ParentChildDef, Pattern,
        ProtoKind, ProtoVariant, Specific, SpecificDef, SpecificFullSelector, SpecificSelector,
        SpecificSubTypes, SpecificSubTypesSelector, SubTypeDef, VariantDef,
    };
    use crate::space::parse::{camel_case, domain, skewer_case, version, version_req};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::combinator::{opt, value};
    use nom::sequence::{delimited, pair, preceded, tuple};
    use starlane_parse::{Res, Span};
    use std::str::FromStr;

    pub fn pattern<I, FnX, X>(mut f: FnX) -> impl FnMut(I) -> Res<I, Pattern<X>> + Copy
    where
        I: Span,
        FnX: FnMut(I) -> Res<I, X> + Copy,
        X: Clone,
    {
        move |input| {
            alt((
                value(Pattern::Any, tag("*")),
                value(Pattern::None, tag("!")),
                |i| f(i).map(|(next, x)| (next, Pattern::Matches(x))),
            ))(input)
        }
    }

    pub fn opt_pattern<I, FnX, X>(mut f: FnX) -> impl FnMut(I) -> Res<I, OptPattern<X>> + Copy
    where
        I: Span,
        FnX: FnMut(I) -> Res<I, X> + Copy,
        X: Clone,
    {
        move |input| {
            alt((
                value(OptPattern::Any, tag("*")),
                value(OptPattern::None, tag("!")),
                |i| f(i).map(|(next, x)| (next, OptPattern::Matches(x))),
            ))(input)
        }
    }

    pub fn preceded_opt_pattern<I, FnX, X, FnPrec>(
        prec: FnPrec,
        mut f: FnX,
    ) -> impl FnMut(I) -> Res<I, OptPattern<X>> + Copy
    where
        I: Span,
        FnX: FnMut(I) -> Res<I, X> + Copy,
        X: Clone,
        FnPrec: FnMut(I) -> Res<I, I> + Copy,
    {
        move |input| {
            alt((preceded(prec, opt_pattern(f)), |i| {
                Ok((i, OptPattern::None))
            }))(input)
        }
    }

    fn sub_types<I, FnPart, Part, FnCamel, Camel>(
        fn_part: FnPart,
        fn_camel: FnCamel,
    ) -> impl FnMut(I) -> Res<I, SubTypeDef<Part, Camel>>
    where
        FnPart: FnMut(I) -> Res<I, Part> + Copy,
        FnCamel: FnMut(I) -> Res<I, Camel> + Copy,
        I: Span,
    {
        move |input: I| {
            tuple((fn_part, fn_camel, fn_camel))(input)
                .map(|(next, (part, sub, r#type))| (next, SubTypeDef { part, sub, r#type }))
        }
    }

    fn parent_child_def<I, FnParent, Parent, FnChild, Child>(
        fn_parent: FnParent,
        fn_child: FnChild,
    ) -> impl FnMut(I) -> Res<I, ParentChildDef<Parent, Child>>
    where
        FnParent: FnMut(I) -> Res<I, Parent> + Copy,
        FnChild: FnMut(I) -> Res<I, Child> + Copy,
        I: Span,
    {
        move |input: I| {
            pair(fn_parent, fn_child)(input)
                .map(|(next, (parent, child))| (next, ParentChildDef { parent, child }))
        }
    }

    pub fn specific_def<I, FnDomain, FnSkewer, FnVersion, Domain, Skewer, Version>(
        fn_domain: FnDomain,
        fn_skewer: FnSkewer,
        fn_version: FnVersion,
    ) -> impl FnMut(I) -> Res<I, SpecificDef<Domain, Skewer, Version>> + Copy
    where
        I: Span,
        FnDomain: FnMut(I) -> Res<I, Domain> + Copy,
        FnSkewer: FnMut(I) -> Res<I, Skewer> + Copy,
        FnVersion: FnMut(I) -> Res<I, Version> + Copy,
    {
        move |input: I| {
            tuple((
                fn_domain,
                tag(":"),
                fn_domain,
                tag(":"),
                fn_skewer,
                tag(":"),
                fn_skewer,
                tag(":"),
                fn_version,
            ))(input)
            .map(
                |(next, (provider, _, vendor, _, product, _, variant, _, version))| {
                    (
                        next,
                        SpecificDef {
                            provider,
                            vendor,
                            product,
                            variant,
                            version,
                        },
                    )
                },
            )
        }
    }

    pub fn specific<I>(input: I) -> Res<I, Specific>
    where
        I: Span,
    {
        specific_def(domain, skewer_case, version)(input)
    }

    pub fn specific_sub_types<I>(input: I) -> Res<I, SpecificSubTypes>
    where
        I: Span,
    {
        sub_types(specific, |i| opt(preceded(tag(":"), camel_case))(i))(input)
    }

    pub fn specific_selector<I>(input: I) -> Res<I, SpecificSelector>
    where
        I: Span,
    {
        specific_def(
            pattern(domain),
            pattern(skewer_case),
            pattern(|i| delimited(tag("("), version_req, tag(")"))(i)),
        )(input)
    }

    pub fn specific_sub_types_selector<I>(input: I) -> Res<I, SpecificSubTypesSelector>
    where
        I: Span,
    {
        sub_types(
            pattern(specific_selector),
            preceded_opt_pattern(|i| tag(":")(i), camel_case),
        )(input)
    }

    pub fn specific_full_selector<I>(input: I) -> Res<I, SpecificFullSelector>
    where
        I: Span,
    {
        sub_types(
            specific_selector,
            preceded_opt_pattern(|i| tag(":")(i), camel_case), //                  preceded_opt_pattern(|i|tag(":")(i), camel_case),
        )(input)
    }

    pub fn variant_def<I, FnVariant, Variant, FnSpecific, Specific>(
        variant: FnVariant,
        specific: FnSpecific,
    ) -> impl FnMut(I) -> Res<I, VariantDef<Variant, Specific>>
    where
        I: Span,
        FnVariant: FnMut(I) -> Res<I, Variant> + Copy,
        FnSpecific: FnMut(I) -> Res<I, Specific> + Copy,
    {
        move |input: I| parent_child_def(variant, specific)(input)
    }

    pub fn kind_def<I, FnKind, Kind, FnVariant, Variant>(
        fn_kind: FnKind,
        fn_variant: FnVariant,
    ) -> impl FnMut(I) -> Res<I, KindDef<Kind, Variant>>
    where
        I: Span,
        FnKind: FnMut(I) -> Res<I, Kind> + Copy,
        FnVariant: FnMut(I) -> Res<I, Variant> + Copy,
    {
        move |input: I| parent_child_def(fn_kind, fn_variant)(input)
    }

    pub fn camel_case_sub_types<I>(input: I) -> Res<I, CamelCaseSubTypes>
    where
        I: Span,
    {
        sub_types(camel_case, |i| opt(preceded(tag(":"), camel_case))(i))(input)
    }

    pub fn camel_case_sub_types_selector<I>(input: I) -> Res<I, CamelCaseSubTypesSelector>
    where
        I: Span,
    {
        sub_types(
            pattern(camel_case),
            preceded_opt_pattern(|i| tag(":")(i), camel_case),
        )(input)
    }

    pub fn child<I, F, R>(mut f: F) -> impl FnMut(I) -> Res<I, R>
    where
        I: Span,
        F: FnMut(I) -> Res<I, R> + Copy,
    {
        move |input: I| delimited(tag("<"), f, tag(">"))(input)
    }

    pub fn proto_variant<I>(input: I) -> Res<I, ProtoVariant>
    where
        I: Span,
    {
        variant_def(camel_case_sub_types, |i| opt(child(specific_sub_types))(i))(input)
    }

    pub fn proto_kind<I>(input: I) -> Res<I, ProtoKind>
    where
        I: Span,
    {
        kind_def(camel_case_sub_types, |i| opt(child(proto_variant))(i))(input)
    }

    #[cfg(test)]
    pub mod test {
        use crate::space::kind2::parse::{
            camel_case_sub_types, camel_case_sub_types_selector, opt_pattern,
            preceded_opt_pattern, proto_kind, proto_variant, specific, specific_full_selector,
            specific_selector, specific_sub_types,
        };
        use crate::space::kind2::{IsMatch, OptPattern};

        use crate::space::parse::error::result;
        use crate::space::parse::{
            camel_case, expect, CamelCase,
        };
        use crate::space::util::log;
        use core::str::FromStr;
        use nom::bytes::complete::tag;
        use nom::combinator::opt;
        use nom::sequence::{pair, preceded};
        use starlane_parse::new_span;

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

            let kind = log(result(proto_kind(new_span("Db:Sub")))).unwrap();
            assert!(kind.parent.sub.is_some());

            let kind = log(result(proto_kind(new_span("Db<Variant>")))).unwrap();
            assert!(kind.child.is_some());
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
    use crate::space::kind2::{
        Artifact, DomainSelector, IsMatch, Kind, OptPattern, Pattern, SkewerSelector, Specific,
        SpecificSelector, SpecificSubTypes, SubTypeDef, Variant, VariantFull, VariantFullSelector,
        VersionSelector,
    };
    use crate::space::loc::Version;
    use crate::space::parse::{CamelCase, Domain, SkewerCase};
    use crate::space::selector::VersionReq;
    use core::str::FromStr;

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
        Variant::Artifact(Artifact::Bundle).with_specific(Some(create_specific_sub_type()))
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
            Variant::Artifact(Artifact::Bundle).with_specific(Some(create_specific_sub_type()));
        let var2 =
            Variant::Artifact(Artifact::Bundle).with_specific(Some(create_specific_sub_type()));
        assert_eq!(var1, var2);
    }

    #[test]
    pub fn kind() {
        let kind1 = Kind::Root.with_variant(Some(create_variant_full()));
        let kind2 = Kind::Root.with_variant(Some(create_variant_full()));
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

    #[test]
    pub fn variant_selector() {
        let variant = create_variant_full();
        let mut selector = VariantFullSelector {
            parent: SubTypeDef {
                part: Pattern::Matches(Variant::Artifact(Artifact::Bundle)),
                sub: OptPattern::None,
                r#type: OptPattern::None,
            },
            child: OptPattern::None,
        };

        assert!(!selector.is_match(&variant));

        let mut selector = VariantFullSelector {
            parent: SubTypeDef {
                part: Pattern::Matches(Variant::Artifact(Artifact::Bundle)),
                sub: OptPattern::None,
                r#type: OptPattern::None,
            },
            child: OptPattern::Any,
        };

        assert!(selector.is_match(&variant));
    }
}
