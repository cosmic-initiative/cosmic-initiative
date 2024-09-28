#![allow(warnings)]
extern crate alloc;
#[macro_use]
extern crate async_trait;
extern crate core;
#[macro_use]
extern crate enum_ordinalize; //# ! [feature(unboxed_closures)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate strum_macros;

use core::str::FromStr;
use std::ops::Deref;

use serde::{Deserialize, Serialize};

use space::artifact::asynch::ArtifactFetcher;
use space::command::common::SetProperties;
use space::command::direct::create::KindTemplate;
use space::command::direct::delete::Delete;
use space::command::direct::select::Select;
use space::config::bind::BindConfig;
use space::kind::{BaseKind, Kind};
use space::loc::Surface;
use space::particle::{Details, Status, Stub};
use space::substance::Bin;
use space::substance::{Substance, ToSubstance};
use space::wave::core::ReflectedCore;

use space::hyper::ParticleRecord;
use space::wave::Agent;

pub mod space;

#[cfg(test)]
pub mod test;
/*
pub fn starlane_uuid() -> Uuid {
    uuid::Uuid::new_v4().to_string()
}
pub fn starlane_timestamp() -> DateTime<Utc> {
    Utc::now()
}

 */

#[cfg(test)]
pub mod tests {
    use crate::space::VERSION;

    #[test]
    fn version() {
        println!("VERSION: {}", VERSION.to_string());
    }
}
