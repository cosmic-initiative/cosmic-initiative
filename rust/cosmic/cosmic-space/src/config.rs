use std::ops::Deref;

use serde::{Deserialize, Serialize};

use crate::config::mechtron::MechtronConfig;
use crate::point::Point;
use crate::particle::{Details, Stub};
use crate::BindConfig;

pub mod bind;
pub mod mechtron;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PortalKind {
    Mechtron,
    Portal,
}

impl ToString for PortalKind {
    fn to_string(&self) -> String {
        match self {
            PortalKind::Mechtron => "Mechtron".to_string(),
            PortalKind::Portal => "Portal".to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Info {
    pub stub: Stub,
    pub kind: PortalKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortalConfig {
    pub max_payload_size: u32,
    pub init_timeout: u64,
    pub frame_timeout: u64,
    pub response_timeout: u64,
}

impl Default for PortalConfig {
    fn default() -> Self {
        Self {
            max_payload_size: 128 * 1024,
            init_timeout: 30,
            frame_timeout: 5,
            response_timeout: 15,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PointConfig<Body> {
    pub point: Point,
    pub body: Body,
}

impl<Body> Deref for PointConfig<Body> {
    type Target = Body;

    fn deref(&self) -> &Self::Target {
        &self.body
    }
}

#[derive(Clone)]
pub enum Document {
    BindConfig(BindConfig),
    MechtronConfig(MechtronConfig),
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ParticleConfigBody {
    pub details: Details,
}
