use crate::space::point::Point;
use core::str::FromStr;
use once_cell::sync::Lazy;

pub mod artifact;
pub mod config;
pub mod parse;
pub mod particle;
pub mod wave;
pub mod asynch;
pub mod command;
pub mod err;
pub mod fail;
pub mod frame;
pub mod hyper;
pub mod kind;
pub mod kind2;
pub mod loc;
pub mod log;
pub mod path;
pub mod point;
pub mod security;
pub mod selector;
pub mod settings;
pub mod substance;
pub mod util;
pub mod wasm;
mod thiserr;

pub static VERSION: Lazy<semver::Version> =
    Lazy::new(|| semver::Version::from_str(include_str!("../VERSION").trim()).unwrap());
pub static HYPERUSER: Lazy<Point> =
    Lazy::new(|| Point::from_str("hyperspace:users:hyperuser").expect("point"));
pub static ANONYMOUS: Lazy<Point> =
    Lazy::new(|| Point::from_str("hyperspace:users:anonymous").expect("point"));