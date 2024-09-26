use core::borrow::Borrow;
use std::ops::Deref;
use std::sync::Arc;
use nom_supreme::final_parser::ExtractContext;
use serde::{Deserialize, Serialize};

use crate::space::loc::ToSurface;
use crate::space::point::Point;
use crate::space::substance::Bin;

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
        self.point.clone().to_bundle().extract_context()
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
