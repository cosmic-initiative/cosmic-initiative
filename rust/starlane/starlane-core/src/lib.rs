#![allow(warnings)]

#[macro_use]
extern crate actix_web;
#[macro_use]
extern crate async_recursion;
#[macro_use]
extern crate async_trait;
extern crate core;
#[macro_use]
extern crate cosmic_macros;
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

use std::str::FromStr;
use std::time::SystemTime;

use chrono::{DateTime, Utc};
use semver;
use uuid::Uuid;

pub mod actor;
pub mod artifact;
pub mod cache;
pub mod command;
pub mod config;
pub mod constellation;
pub mod crypt;
pub mod data;
pub mod databases;
pub mod error;
pub mod fail;
pub mod file_access;
pub mod filesystem;
pub mod frame;
pub mod global;
pub mod html;
pub mod id;
pub mod lane;
pub mod logger;
pub mod mechtron;
pub mod message;
pub mod names;
pub mod parse;
pub mod particle;
pub mod pattern;
pub mod proto;
pub mod registry;
pub mod server;
pub mod service;
pub mod space;
pub mod star;
pub mod starlane;
pub mod template;
pub mod user;
pub mod util;
pub mod watch;

lazy_static! {
    static ref VERSION: semver::Version = {
        semver::Version::from_str("0.2.0-rc1")
            .expect("expected starlane::VERSION semver string to parse.")
    };
}

#[no_mangle]
pub extern "C" fn mesh_portal_uuid() -> String {
    Uuid::new_v4().to_string()
}

#[no_mangle]
pub extern "C" fn mesh_portal_timestamp() -> DateTime<Utc> {
    Utc::now()
}
