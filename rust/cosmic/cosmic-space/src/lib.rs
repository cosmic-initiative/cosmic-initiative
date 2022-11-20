#![allow(warnings)]
extern crate alloc;
#[macro_use]
extern crate async_trait;
extern crate core;
#[macro_use]
extern crate enum_ordinalize;
//# ! [feature(unboxed_closures)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate strum_macros;

use core::str::FromStr;
use std::cmp::Ordering;
use std::ops::Deref;
use std::sync::Arc;

use dashmap::{DashMap, DashSet};
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;

use artifact::asynch::ArtifactFetcher;
use command::common::{SetProperties, SetRegistry};
use command::direct::create::{KindTemplate, Strategy};
use command::direct::delete::Delete;
use command::direct::query::{Query, QueryResult};
use command::direct::select::{Select, SubSelect};
use config::bind::BindConfig;
use config::Document;
use kind::{ArtifactSubKind, BaseKind, FileSubKind, Kind, Specific, StarSub, UserBaseSubKind};
use loc::{Point, Surface, Uuid};
use particle::{Details, Properties, Status, Stub};
use selector::Selector;
use substance::Bin;
use substance::{Substance, SubstanceList, ToSubstance, Token};
use wave::core::ReflectedCore;

use crate::err::SpaceErr;
use crate::hyper::ParticleRecord;
use crate::security::{Access, AccessGrant};
use crate::wave::Agent;

pub mod artifact;
pub mod command;
pub mod config;
pub mod err;
pub mod fail;
pub mod frame;
pub mod hyper;
pub mod kind;
pub mod loc;
pub mod log;
pub mod parse;
pub mod particle;
pub mod path;
pub mod security;
pub mod selector;
pub mod selector2;
pub mod settings;
pub mod substance;
pub mod util;
pub mod wasm;
pub mod wave;
pub mod kind2;
pub mod point;

lazy_static! {
    pub static ref VERSION: semver::Version =
        semver::Version::from_str(include_str!("VERSION").trim()).unwrap();
    pub static ref HYPERUSER: Point = Point::from_str("hyperspace:users:hyperuser").expect("point");
    pub static ref ANONYMOUS: Point = Point::from_str("hyperspace:users:anonymous").expect("point");
}

/*
pub fn cosmic_uuid() -> Uuid {
    uuid::Uuid::new_v4().to_string()
}
pub fn cosmic_timestamp() -> DateTime<Utc> {
    Utc::now()
}

 */

#[cfg(test)]
pub mod tests {
    use crate::VERSION;

    #[test]
    fn version() {
        println!("VERSION: {}", VERSION.to_string());
    }
}
