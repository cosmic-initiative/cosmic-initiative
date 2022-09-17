use crate::id::id::Point;
use crate::particle::particle::Stub;
use crate::substance::substance::Substance;

pub mod particle {
    use std::collections::{HashMap, HashSet};
    use std::str::FromStr;

    use nom::branch::alt;
    use nom::bytes::complete::{is_a, tag};
    use nom::character::complete::{alpha1, digit1};
    use nom::combinator::{not, recognize};
    use nom::error::{ErrorKind, ParseError, VerboseError};
    use nom::sequence::{delimited, tuple};
    use nom::CompareResult::Incomplete;
    use nom::Parser;
    use nom_supreme::error::ErrorTree;
    use nom_supreme::{parse_from_str, ParserExt};
    use serde::{Deserialize, Serialize};

    use crate::error::UniErr;
    use crate::id::id::{BaseKind, Kind, KindParts, Point, PointKind};
    use crate::parse::parse_alpha1_str;
    use crate::security::Permissions;
    use crate::substance::substance::{Substance, SubstanceMap};
    use cosmic_nom::{Res, Span};

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
}

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Particle {
    pub stub: Stub,
    pub state: Substance,
}

impl Particle {
    pub fn new(stub: Stub, state: Substance) -> Particle {
        Particle { stub, state }
    }

    pub fn point(&self) -> Point {
        self.stub.point.clone()
    }

    pub fn state_src(&self) -> Substance {
        self.state.clone()
    }
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