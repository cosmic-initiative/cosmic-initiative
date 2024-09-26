use core::str::FromStr;
use std::collections::HashMap;

use nom::bytes::complete::tag;
use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};

use starlane_parse::{new_span, Res, Span};

use crate::space::kind::{Kind, KindParts};
use crate::space::parse::error::result;
use crate::space::parse::{parse_alpha1_str, point_and_kind, Env};
use crate::space::point::{Point, PointCtx, PointVar};
use crate::space::substance::Substance;
use crate::space::util::ToResolved;
use crate::{BaseKind, SpaceErr};

pub mod property;
pub mod traversal;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StatusUpdate {
    pub from: Point,
    pub status: Status,
}

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    Eq,
    PartialEq,
    strum_macros::Display,
    strum_macros::EnumString,
)]
pub enum Status {
    Unknown,  // initial status or when we status cannot be determined
    Pending,  // initial status
    Init, // undergoing custom initialization...This particle can send requests but not receive requests.
    Panic, // something is wrong... all requests are blocked and responses are cancelled.
    Fatal, // unrecoverable panic
    Ready, // ready to take requests
    Paused, // can not receive requests (probably because it is waiting for some other particle to make updates)...
    Resuming, // like Initializing but triggered after a pause is lifted, the particle may be doing something before it is ready to accept requests again.
    Done, // this particle had a life span and has now completed succesfully it can no longer receive requests.
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum Code {
    Ok,
    Error(i32),
}

impl ToString for Code {
    fn to_string(&self) -> String {
        match self {
            Code::Ok => "Ok".to_string(),
            Code::Error(code) => {
                format!("Err({})", code)
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Progress {
    pub step: u16,
    pub total: u16,
}

impl ToString for Progress {
    fn to_string(&self) -> String {
        format!("{}/{}", self.step, self.total)
    }
}

pub fn ok_code<I: Span>(input: I) -> Res<I, Code> {
    tag("Ok")(input).map(|(next, code)| (next, Code::Ok))
}

pub fn status<I: Span>(input: I) -> Res<I, Status> {
    parse_alpha1_str(input)
}

pub type Properties = HashMap<String, Property>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Property {
    pub key: String,
    pub value: String,
    pub locked: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Archetype {
    pub kind: KindParts,
    pub properties: Properties,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Details {
    pub stub: Stub,
    pub properties: Properties,
}

impl Default for Details {
    fn default() -> Self {
        Self {
            stub: Default::default(),
            properties: Default::default(),
        }
    }
}

impl Details {
    pub fn new(stub: Stub, properties: Properties) -> Self {
        Self { stub, properties }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Stub {
    pub point: Point,
    pub kind: Kind,
    pub status: Status,
}

impl Default for Stub {
    fn default() -> Self {
        Self {
            point: Point::root(),
            kind: Kind::Root,
            status: Status::Unknown,
        }
    }
}

impl Stub {
    pub fn point_and_kind(self) -> PointKind {
        PointKind {
            point: self.point,
            kind: self.kind,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Particle {
    pub stub: Stub,
    pub state: Box<Substance>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Particle2 {
    pub stub: Stub,
    pub state: Substance,
}

impl Particle2 {
    pub fn new(stub: Stub, state: Substance) -> Particle2 {
        Particle2 { stub, state }
    }

    pub fn point(&self) -> Point {
        self.stub.point.clone()
    }

    pub fn state_src(&self) -> Substance {
        self.state.clone()
    }
}

pub mod particle {
    use nom::Parser;
    use nom_supreme::ParserExt;

    /*
    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct StatusDetails<C>
    where
        C: Condition,
    {
        pub status: Status,
        pub conditions: HashSet<C>,
    }

    pub trait Condition: ToString {
        fn status(&self) -> Status;
        fn desc(&self) -> String;
    }
     */

    /*
    pub fn error_code<I:Span>(input: I) -> Res<I, Code> {
        let (next, err_code) = delimited(tag("Err("), digit1, tag(")"))(input.clone())?;
        Ok((
            next,
            Code::Error(match err_code.parse() {
                Ok(i) => i,
                Err(err) => {
                    return Err(nom::Err::Error(ErrorTree::from_error_kind(
                        input,
                        ErrorKind::Tag,
                    )))
                }
            }),
        ))
    }


    pub fn code<I:Span>(input: I) -> Res<I, Code> {
        alt((error_code, ok_code))(input)
    }
     */
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Watch {
    pub point: Point,
    pub aspect: Aspect,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize, strum_macros::Display)]
pub enum Aspect {
    Log,
    State,
    Property,
    Child,
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
    fn to_resolved(self, env: &Env) -> anyhow::Result<PointKindCtx> {
        Ok(PointKindCtx {
            point: self.point.to_resolved(env)?,
            kind: self.kind,
        })
    }
}

impl ToResolved<PointKind> for PointKindVar {
    fn to_resolved(self, env: &Env) -> anyhow::Result<PointKind> {
        Ok(PointKind {
            point: self.point.to_resolved(env)?,
            kind: self.kind,
        })
    }
}

impl ToResolved<PointKind> for PointKindCtx {
    fn to_resolved(self, env: &Env) -> anyhow::Result<PointKind> {
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
    type Err = SpaceErr;

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
