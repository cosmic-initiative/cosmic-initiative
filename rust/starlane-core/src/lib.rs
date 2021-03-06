#![feature(fn_traits)]
#![feature(unboxed_closures)]
#![feature(trivial_bounds)]


#[macro_use]
extern crate actix_web;
#[macro_use]
extern crate async_trait;
#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate futures;
#[macro_use]
extern crate k8s_openapi;
#[macro_use]
extern crate kube;
#[macro_use]
extern crate kube_derive;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate log;
#[macro_use]
extern crate nom;
#[macro_use]
extern crate schemars;
#[macro_use]
extern crate strum_macros;
#[macro_use]
extern crate tracing;
#[macro_use]
extern crate validate;
#[macro_use]
extern crate wasmer;
#[macro_use]
extern crate async_recursion;


use std::str::FromStr;

use semver;
use uuid::Uuid;

pub mod actor;
pub mod artifact;
pub mod cache;
pub mod config;
pub mod constellation;
pub mod crypt;
pub mod data;
pub mod error;
pub mod file_access;
pub mod filesystem;
pub mod frame;
pub mod id;
pub mod lane;
pub mod logger;
pub mod message;
pub mod names;
pub mod proto;
pub mod resource;
pub mod server;
pub mod service;
pub mod space;
pub mod star;
pub mod starlane;
pub mod template;
pub mod util;
pub mod watch;
pub mod parse;
pub mod html;
pub mod security;
pub mod pattern;
pub mod fail;
pub mod command;
pub mod user;
pub mod mechtron;

lazy_static! {
    static ref VERSION: semver::Version = {
        semver::Version::from_str("0.2.0-rc1")
            .expect("expected starlane::VERSION semver string to parse.")
    };
}

#[no_mangle]
pub extern "C" fn mesh_portal_unique_id() -> String
{
    Uuid::new_v4().to_string()
}

#[cfg(test)]
pub mod test {

    #[test]
    pub fn test() {
        println!("It Works!");
    }

}
