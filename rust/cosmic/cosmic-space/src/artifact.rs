use core::borrow::Borrow;
use dashmap::DashMap;
use std::cell::Cell;
use std::ops::Deref;
use std::sync::Arc;

use serde::{Deserialize, Serialize};
use tokio::sync::{watch, RwLock};

use crate::config::mechtron::MechtronConfig;
use crate::point::{Point, ToSurface};
use crate::particle::Stub;
use crate::substance::Bin;
use crate::wave::core::cmd::CmdMethod;
use crate::wave::exchange::asynch::ProtoTransmitter;
use crate::wave::exchange::asynch::ProtoTransmitterBuilder;
use crate::wave::{DirectedProto, Pong, Wave};
use crate::{BindConfig, SpaceErr, Substance};

pub mod asynch;
pub mod synch;

#[derive(Clone)]
pub struct ArtRef<A> {
    artifact: Arc<A>,
    pub point: Point,
}

impl<A> ArtRef<A> {
    pub fn new(artifact: Arc<A>, point: Point) -> Self {
        Self { artifact, point }
    }
}

impl<A> ArtRef<A>
where
    A: Clone,
{
    pub fn contents(&self) -> A {
        (*self.artifact).clone()
    }
}

impl<A> ArtRef<A> {
    pub fn bundle(&self) -> Point {
        self.point.clone().to_bundle().unwrap()
    }
    pub fn point(&self) -> &Point {
        &self.point
    }
}

impl<A> Deref for ArtRef<A> {
    type Target = Arc<A>;

    fn deref(&self) -> &Self::Target {
        &self.artifact
    }
}

impl<A> Drop for ArtRef<A> {
    fn drop(&mut self) {
        //
    }
}

pub struct FetchErr {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    pub point: Point,
    pub bin: Bin,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArtifactRequest {
    pub point: Point,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArtifactResponse {
    pub to: Point,
    pub payload: Bin,
}
