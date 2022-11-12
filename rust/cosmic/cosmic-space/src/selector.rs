use core::fmt::Formatter;
use core::str::FromStr;
use std::marker::PhantomData;
use std::ops::Deref;

use nom::combinator::all_consuming;
use serde::de::{Error, Visitor};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};

use cosmic_nom::{new_span, Trace};
use specific::{ProductSelector, ProductVariantSelector, ProviderSelector, VendorSelector};

use crate::kind2::{Kind, ProtoKindSelector, Specific};
use crate::loc::{
    Layer, PointCtx, PointSeg, PointSegCtx, PointSegVar, PointVar, RouteSeg, ToBaseKind, Topic,
    Variable, VarVal, Version,
};
use crate::parse::error::result;
use crate::parse::point_segment_selector;
use crate::substance::{
    CallWithConfigDef, Substance, SubstanceFormat, SubstanceKind, SubstancePattern,
    SubstancePatternCtx, SubstancePatternDef,
};
use crate::util::{ToResolved, ValueMatcher, ValuePattern};
use crate::{Point, SpaceErr};
use crate::kind2::parse::specific_selector;
use crate::model::{CamelCase, Env};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct VersionReq {
    pub version: semver::VersionReq,
}

impl Deref for VersionReq {
    type Target = semver::VersionReq;

    fn deref(&self) -> &Self::Target {
        &self.version
    }
}

impl Serialize for VersionReq {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.version.to_string().as_str())
    }
}

impl<'de> Deserialize<'de> for VersionReq {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(VersionReqVisitor)
    }
}

struct VersionReqVisitor;

impl<'de> Visitor<'de> for VersionReqVisitor {
    type Value = VersionReq;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("SemVer version requirement")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match VersionReq::from_str(v) {
            Ok(version) => Ok(version),
            Err(error) => {
                //Err(de::Error::custom(error.to_string() ))
                Err(de::Error::invalid_type(de::Unexpected::Str(v), &self))
            }
        }
    }
}

impl ToString for VersionReq {
    fn to_string(&self) -> String {
        self.version.to_string()
    }
}

impl TryInto<semver::VersionReq> for VersionReq {
    type Error = SpaceErr;

    fn try_into(self) -> Result<semver::VersionReq, Self::Error> {
        Ok(self.version)
    }
}

impl FromStr for VersionReq {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let version = semver::VersionReq::from_str(s)?;
        Ok(Self { version })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSegSelector {
    InclusiveAny,       // +:*  // includes Root if it's the first segment
    InclusiveRecursive, // +:** // includes Root if its the first segment
    Any,                // *
    Recursive,          // **
    Exact(ExactPointSeg),
    Version(VersionReq),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSegSelectorVar {
    InclusiveAny,       // +:*  // includes Root if it's the first segment
    InclusiveRecursive, // +:** // includes Root if its the first segment
    Any,                // *
    Recursive,          // **
    Exact(ExactPointSeg),
    Version(VersionReq),
    Var(Variable),
    Working(Trace),
    Pop(Trace),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PointSegSelectorCtx {
    InclusiveAny,       // +:*  // includes Root if it's the first segment
    InclusiveRecursive, // +:** // includes Root if its the first segment
    Any,                // *
    Recursive,          // **
    Exact(ExactPointSeg),
    Version(VersionReq),
    Working(Trace),
    Pop(Trace),
}

impl FromStr for PointSegSelector {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        result(all_consuming(point_segment_selector)(new_span(s)))
    }
}

impl PointSegSelector {
    pub fn is_exact(&self) -> bool {
        match self {
            PointSegSelector::Exact(_) => true,
            _ => false,
        }
    }

    pub fn matches(&self, segment: &PointSeg) -> bool {
        match self {
            PointSegSelector::InclusiveAny => true,
            PointSegSelector::InclusiveRecursive => true,
            PointSegSelector::Any => true,
            PointSegSelector::Recursive => true,
            PointSegSelector::Exact(exact) => match exact {
                ExactPointSeg::PointSeg(pattern) => *pattern == *segment,
                ExactPointSeg::Version(a) => {
                    if let PointSeg::Version(b) = segment {
                        *a == *b
                    } else {
                        false
                    }
                }
            },
            PointSegSelector::Version(req) => {
                if let PointSeg::Version(b) = segment {
                    req.matches(b)
                } else {
                    false
                }
            }
        }
    }

    pub fn is_recursive(&self) -> bool {
        match self {
            PointSegSelector::InclusiveAny => false,
            PointSegSelector::InclusiveRecursive => true,
            PointSegSelector::Any => false,
            PointSegSelector::Recursive => true,
            PointSegSelector::Exact(_) => false,
            PointSegSelector::Version(_) => false,
        }
    }
}

impl ToString for PointSegSelector {
    fn to_string(&self) -> String {
        match self {
            PointSegSelector::InclusiveAny => "+:*".to_string(),
            PointSegSelector::InclusiveRecursive => "+:**".to_string(),
            PointSegSelector::Any => "*".to_string(),
            PointSegSelector::Recursive => "**".to_string(),
            PointSegSelector::Exact(exact) => exact.to_string(),
            PointSegSelector::Version(version) => version.to_string(),
        }
    }
}

pub type KeySegment = String;

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize, Hash)]
pub enum ExactPointSeg {
    PointSeg(PointSeg),
    Version(Version),
}

impl ExactPointSeg {
    pub fn matches(&self, segment: &PointSeg) -> bool {
        match self {
            ExactPointSeg::PointSeg(s) => *s == *segment,
            ExactPointSeg::Version(a) => {
                if let PointSeg::Version(b) = segment {
                    *a == *b
                } else {
                    false
                }
            }
        }
    }
}

impl ToString for ExactPointSeg {
    fn to_string(&self) -> String {
        match self {
            ExactPointSeg::PointSeg(point) => point.to_string(),
            ExactPointSeg::Version(version) => version.to_string(),
        }
    }
}

/// Provides ability to Select on a Specific.  This means wildcards can be applied when any match will do:
/// `mechtronhub.io:postgres.org:postgres:*:(9.0.0)` will select ANY variant of postgres version 9.0.0.
/// (notice the version MUST be delimited by Parenthesis.
/// A more useful example is when performing some type of version selection it follows SemVer Req rules:
/// `mechtronhub.io:postgres.org:postgres:gis:(>=10.2.3 <12.3.0)`
/// which would match on any version of postgres:gis with a version in that range
pub type SpecificSelector = SpecificSelectorDef<
    ProviderSelector,
    VendorSelector,
    ProductSelector,
    ProductVariantSelector,
    VersionReq,
>;



#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct SpecificSelectorDef<
    ProviderSelector,
    VendorSelector,
    ProductSelector,
    VariantSelector,
    VersionReq,
> {
    pub provider: ProviderSelector,
    pub vendor: VendorSelector,
    pub product: ProductSelector,
    pub variant: VariantSelector,
    pub version: VersionReq,
}

impl ValueMatcher<Specific> for SpecificSelector {
    fn is_match(&self, specific: &Specific) -> Result<(), ()> {
        if self.provider.matches(&specific.provider)
            && self.vendor.matches(&specific.vendor)
            && self.product.matches(&specific.product)
            && self.variant.matches(&specific.variant)
            && self.version.matches(&specific.version)
        {
            Ok(())
        } else {
            Err(())
        }
    }
}

impl ToString for SpecificSelector {
    fn to_string(&self) -> String {
        format!(
            "{}:{}:{}:({})",
            self.vendor.to_string(),
            self.product.to_string(),
            self.variant.to_string(),
            self.version.to_string()
        )
    }
}

pub mod specific {
    use alloc::string::String;
    use core::ops::Deref;
    use core::str::FromStr;

    use crate::err::SpaceErr;
    use crate::model::{Domain, SkewerCase};
    use crate::selector::Pattern;

    pub struct VersionReq {
        pub req: semver::VersionReq,
    }

    impl Deref for VersionReq {
        type Target = semver::VersionReq;

        fn deref(&self) -> &Self::Target {
            &self.req
        }
    }

    impl FromStr for VersionReq {
        type Err = SpaceErr;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            Ok(VersionReq {
                req: semver::VersionReq::from_str(s)?,
            })
        }
    }

    pub type ProviderSelector = Pattern<Domain>;
    pub type VendorSelector = Pattern<Domain>;
    pub type ProductSelector = Pattern<SkewerCase>;
    pub type ProductVariantSelector = Pattern<SkewerCase>;
    pub type VersionPattern = Pattern<VersionReq>;
}

pub type LabeledPrimitiveType = LabeledPrimitiveTypeDef<Point>;
pub type LabeledPrimitiveTypeCtx = LabeledPrimitiveTypeDef<PointCtx>;
pub type LabeledPrimitiveTypeVar = LabeledPrimitiveTypeDef<PointVar>;

pub struct LabeledPrimitiveTypeDef<Pnt> {
    pub label: String,
    pub def: PayloadType2Def<Pnt>,
}

pub type PayloadType2 = PayloadType2Def<Point>;
pub type PayloadType2Ctx = PayloadType2Def<PointCtx>;
pub type PayloadType2Var = PayloadType2Def<PointVar>;

pub struct PayloadType2Def<Pnt> {
    pub primitive: SubstanceKind,
    pub format: Option<SubstanceFormat>,
    pub verifier: Option<CallWithConfigDef<Pnt>>,
}

#[derive(Debug, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq)]
pub enum Format {
    #[strum(serialize = "json")]
    Json,
    #[strum(serialize = "image")]
    Image,
}

#[derive(Debug, Clone, strum_macros::Display, strum_macros::EnumString, Eq, PartialEq)]
pub enum PipelineKind {
    Rc,
    Ext,
    Http,
}

pub struct ParsedPipelineBlock {}

pub type MapEntryPatternVar = MapEntryPatternDef<PointVar>;
pub type MapEntryPatternCtx = MapEntryPatternDef<PointCtx>;
pub type MapEntryPattern = MapEntryPatternDef<Point>;

#[derive(Clone)]
pub struct MapEntryPatternDef<Pnt> {
    pub key: String,
    pub payload: ValuePattern<SubstancePatternDef<Pnt>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum Pattern<P> {
    Any,
    Exact(P),
}

impl<I: ToString> Pattern<I> {
    pub fn to_string_version(self) -> Pattern<String> {
        match self {
            Pattern::Any => Pattern::Any,
            Pattern::Exact(exact) => Pattern::Exact(exact.to_string()),
        }
    }
}

impl<P> Pattern<P>
where
    P: Eq + PartialEq,
{
    pub fn is_any(&self) -> bool {
        match self {
            Pattern::Any => true,
            Pattern::Exact(_) => false,
        }
    }

    pub fn matches(&self, t: &P) -> bool {
        match self {
            Self::Any => true,
            Self::Exact(p) => *p == *t,
        }
    }
    pub fn matches_opt(&self, other: Option<&P>) -> bool {
        match self {
            Self::Any => true,
            Self::Exact(exact) => {
                if let Option::Some(other) = other {
                    *exact == *other
                } else {
                    false
                }
            }
        }
    }

    pub fn convert<To>(self) -> Result<Pattern<To>, SpaceErr>
    where
        P: TryInto<To, Error = SpaceErr> + Eq + PartialEq,
    {
        Ok(match self {
            Pattern::Any => Pattern::Any,
            Pattern::Exact(exact) => Pattern::Exact(exact.try_into()?),
        })
    }
}

impl<P> ToString for Pattern<P>
where
    P: ToString,
{
    fn to_string(&self) -> String {
        match self {
            Pattern::Any => "*".to_string(),
            Pattern::Exact(exact) => exact.to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EmptyPattern<P> {
    Any,
    Pattern(P),
}

impl<P> EmptyPattern<P>
where
    P: Eq + PartialEq,
{
    pub fn matches(&self, t: &P) -> bool {
        match self {
            Self::Any => true,
            Self::Pattern(p) => *p == *t,
        }
    }
    pub fn matches_opt(&self, other: Option<&P>) -> bool {
        match self {
            Self::Any => true,
            Self::Pattern(exact) => {
                if let Option::Some(other) = other {
                    *exact == *other
                } else {
                    false
                }
            }
        }
    }

    pub fn convert<To>(self) -> Result<EmptyPattern<To>, SpaceErr>
    where
        P: TryInto<To, Error = SpaceErr> + Eq + PartialEq,
    {
        Ok(match self {
            EmptyPattern::Any => EmptyPattern::Any,
            EmptyPattern::Pattern(exact) => EmptyPattern::Pattern(exact.try_into()?),
        })
    }
}

impl Into<EmptyPattern<String>> for EmptyPattern<&str> {
    fn into(self) -> EmptyPattern<String> {
        match self {
            EmptyPattern::Any => EmptyPattern::Any,
            EmptyPattern::Pattern(f) => EmptyPattern::Pattern(f.to_string()),
        }
    }
}

impl<P> ToString for EmptyPattern<P>
where
    P: ToString,
{
    fn to_string(&self) -> String {
        match self {
            EmptyPattern::Any => "".to_string(),
            EmptyPattern::Pattern(exact) => exact.to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PortHierarchy {
    pub topic: Topic,
    pub layer: Layer,
    pub point_hierarchy: PointHierarchy,
}

impl PortHierarchy {
    pub fn new(point_hierarchy: PointHierarchy, layer: Layer, topic: Topic) -> Self {
        Self {
            topic,
            layer,
            point_hierarchy,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PointHierarchy {
    pub route: RouteSeg,
    pub segments: Vec<PointKindSeg>,
}

impl PointHierarchy {
    pub fn new(route: RouteSeg, segments: Vec<PointKindSeg>) -> Self {
        Self { route, segments }
    }
}

impl PointHierarchy {
    pub fn push(&self, segment: PointKindSeg) -> PointHierarchy {
        if let PointSeg::Root = segment.segment {
            println!("pushing ROOT");
        }
        let mut segments = self.segments.clone();
        segments.push(segment);
        Self {
            route: self.route.clone(),
            segments,
        }
    }
}

impl PointHierarchy {
    pub fn consume(&self) -> Option<PointHierarchy> {
        if self.segments.len() <= 1 {
            return Option::None;
        }
        let mut segments = self.segments.clone();
        segments.remove(0);
        Option::Some(PointHierarchy {
            route: self.route.clone(),
            segments,
        })
    }

    pub fn is_root(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn is_final(&self) -> bool {
        self.segments.len() == 1
    }
}

impl ToString for PointHierarchy {
    fn to_string(&self) -> String {
        let mut rtn = String::new();
        match &self.route {
            RouteSeg::This => {}
            route => {
                rtn.push_str(route.to_string().as_str());
                rtn.push_str("::");
            }
        }

        let mut post_fileroot = false;
        for (index, segment) in self.segments.iter().enumerate() {
            if let PointSeg::FilesystemRootDir = segment.segment {
                post_fileroot = true;
            }
            rtn.push_str(segment.segment.preceding_delim(post_fileroot));
            rtn.push_str(segment.to_string().as_str());
        }

        rtn
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct PointKindSeg {
    pub segment: PointSeg,
    pub kind: Kind,
}

impl ToString for PointKindSeg {
    fn to_string(&self) -> String {
        format!("{}<{}>", self.segment.to_string(), self.kind.to_string())
    }
}


pub type PayloadBlock = PayloadBlockDef<Point>;
pub type PayloadBlockCtx = PayloadBlockDef<PointCtx>;
pub type PayloadBlockVar = PayloadBlockDef<PointVar>;

impl ToResolved<PayloadBlockCtx> for PayloadBlockVar {
    fn to_resolved(self, env: &Env) -> Result<PayloadBlockCtx, SpaceErr> {
        match self {
            PayloadBlockVar::DirectPattern(block) => Ok(PayloadBlockCtx::DirectPattern(
                block.modify(move |block| {
                    let block: SubstancePatternCtx = block.to_resolved(env)?;
                    Ok(block)
                })?,
            )),
            PayloadBlockVar::ReflectPattern(block) => Ok(PayloadBlockCtx::ReflectPattern(
                block.modify(move |block| block.to_resolved(env))?,
            )),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UploadBlock {
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateBlock {
    pub payload: Substance,
}

pub type PatternBlock = PatternBlockDef<Point>;
pub type PatternBlockCtx = PatternBlockDef<PointCtx>;
pub type PatternBlockVar = PatternBlockDef<PointVar>;
pub type PatternBlockDef<Pnt> = ValuePattern<SubstancePatternDef<Pnt>>;

impl ToResolved<PatternBlock> for PatternBlockCtx {
    fn to_resolved(self, env: &Env) -> Result<PatternBlock, SpaceErr> {
        match self {
            PatternBlockCtx::Any => Ok(PatternBlock::Any),
            PatternBlockCtx::None => Ok(PatternBlock::None),
            PatternBlockCtx::Pattern(pattern) => {
                Ok(PatternBlock::Pattern(pattern.to_resolved(env)?))
            }
        }
    }
}

impl ToResolved<PatternBlockCtx> for PatternBlockVar {
    fn to_resolved(self, env: &Env) -> Result<PatternBlockCtx, SpaceErr> {
        match self {
            PatternBlockVar::Any => Ok(PatternBlockCtx::Any),
            PatternBlockVar::None => Ok(PatternBlockCtx::None),
            PatternBlockVar::Pattern(pattern) => {
                Ok(PatternBlockCtx::Pattern(pattern.to_resolved(env)?))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PayloadBlockDef<Pnt> {
    DirectPattern(PatternBlockDef<Pnt>),
    ReflectPattern(PatternBlockDef<Pnt>),
}

impl ToResolved<PayloadBlock> for PayloadBlockCtx {
    fn to_resolved(self, env: &Env) -> Result<PayloadBlock, SpaceErr> {
        match self {
            PayloadBlockCtx::DirectPattern(block) => {
                Ok(PayloadBlock::DirectPattern(block.modify(move |block| {
                    let block: SubstancePattern = block.to_resolved(env)?;
                    Ok(block)
                })?))
            }
            PayloadBlockCtx::ReflectPattern(block) => Ok(PayloadBlock::ReflectPattern(
                block.modify(move |block| block.to_resolved(env))?,
            )),
        }
    }
}

impl ToResolved<PayloadBlock> for PayloadBlockVar {
    fn to_resolved(self, env: &Env) -> Result<PayloadBlock, SpaceErr> {
        let block: PayloadBlockCtx = self.to_resolved(env)?;
        block.to_resolved(env)
    }
}

pub type PointSelector = PointSelectorDef<PointSeg>;
pub type PointSelectorCtx = PointSelectorDef<PointSegCtx>;
pub type PointSelectorVar = PointSelectorDef<PointSegVar>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct PointSelectorDef<Seg> {
    pub route: RouteSeg,
    pub segments: Vec<Seg>,
}
impl ToResolved<PointSelector> for PointSelectorCtx {
    fn to_resolved(self, env: &Env) -> Result<PointSelector, SpaceErr> {
        todo!()
    }
}

impl ToResolved<PointSelectorCtx> for PointSelectorVar {
    fn to_resolved(self, env: &Env) -> Result<PointSelectorCtx, SpaceErr> {
        todo!()
    }
}

impl PointSelector {
    pub fn matches(&self, point: &Point) -> bool {
        todo!()
    }
}

impl<Seg> FromStr for PointSelectorDef<Seg> {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        todo!()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct SelectorDef<Hop> {
    phantom: PhantomData<Hop>,
}
