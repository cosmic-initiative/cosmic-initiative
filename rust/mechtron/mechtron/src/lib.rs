#![allow(warnings)]

pub mod err;
pub mod guest;
pub mod membrane;
pub mod space;
#[cfg(test)]
pub mod test;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate cosmic_macros;

#[macro_use]
extern crate cosmic_macros_primitive;

extern crate alloc;
extern crate core;

use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use core::str::FromStr;
use cosmic_macros::handler;
use cosmic_macros::route;
use cosmic_macros::DirectedHandler;
use cosmic_space::err::SpaceErr;
use cosmic_space::hyper::HyperSubstance;
use cosmic_space::loc::{Layer, Point, ToSurface, Uuid};
use cosmic_space::log::{LogSource, NoAppender, PointLogger, RootLogger};
use cosmic_space::model::SkewerCase;
use cosmic_space::particle::{Details, Stub};
use cosmic_space::wasm::Timestamp;
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{Agent, DirectedWave, ReflectedAggregate, ReflectedWave, UltraWave};
use cosmic_space::{loc, VERSION};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::mpsc::Sender;
use std::sync::{mpsc, MutexGuard};

use cosmic_space::wave::Bounce;

use cosmic_space::artifact::synch::ArtifactApi;
use cosmic_space::artifact::ArtRef;
use cosmic_space::wave::exchange::synch::{
    DirectedHandler, DirectedHandlerProxy, DirectedHandlerShell, ExchangeRouter, InCtx,
    ProtoTransmitter, ProtoTransmitterBuilder,
};
use std::sync::RwLock;

use crate::err::{GuestErr, MechErr};
use crate::guest::GuestCtx;
use crate::membrane::{mechtron_frame_to_host, mechtron_timestamp, mechtron_uuid};

#[no_mangle]
extern "C" {
    pub fn mechtron_guest(details: Details) -> Result<Arc<dyn Guest>, GuestErr>;
}

pub trait Guest: Send + Sync {
    fn handler(&self, point: &Point) -> Result<DirectedHandlerShell, GuestErr>;
    fn logger(&self) -> &PointLogger;
}

pub trait Platform: Clone + Send + Sync
where
    Self::Err: MechErr,
{
    type Err;
    fn factories(&self) -> Result<MechtronFactories<Self>, Self::Err>
    where
        Self: Sized,
    {
        Ok(MechtronFactories::new())
    }
}

pub struct MechtronFactories<P>
where
    P: Platform,
{
    factories: HashMap<String, RwLock<Box<dyn MechtronFactory<P>>>>,
    phantom: PhantomData<P>,
}

impl<P> MechtronFactories<P>
where
    P: Platform,
{
    pub fn new() -> Self {
        Self {
            factories: HashMap::new(),
            phantom: Default::default(),
        }
    }
    pub fn add<F>(&mut self, factory: F)
    where
        F: MechtronFactory<P>,
    {
        SkewerCase::from_str(factory.name().as_str() ).expect("Mechtron Name must be valid kebab (skewer) case (all lower case alphanumeric and dashes with leading letter)");
        self.factories
            .insert(factory.name(), RwLock::new(Box::new(factory)));
    }

    pub fn get<S>(&self, name: S) -> Option<&RwLock<Box<dyn MechtronFactory<P>>>>
    where
        S: ToString,
    {
        self.factories.get(&name.to_string())
    }
}

pub trait MechtronFactory<P>: Sync + Send + 'static
where
    P: Platform,
{
    fn name(&self) -> String;
    fn new(&mut self, skel: MechtronSkel<P>) -> Result<(), P::Err>;
    fn lifecycle(&self, skel: MechtronSkel<P>) -> Result<Box<dyn MechtronLifecycle<P>>, P::Err>;
    fn handler(&self, skel: MechtronSkel<P>) -> Result<Box<dyn DirectedHandler>, P::Err>;
}

/// The MechtronSkel holds the common static elements of the Mechtron together
/// Since a Mechtron is always an instance created to handle a single
/// Directed Wave or Init, the Skel is cloned and passed to each
/// Mechtron instance.
///
#[derive(Clone)]
pub struct MechtronSkel<P>
where
    P: Platform,
{
    pub details: Details,
    pub logger: PointLogger,
    pub artifacts: ArtifactApi,
    phantom: PhantomData<P>,
}

impl<P> MechtronSkel<P>
where
    P: Platform,
{
    pub fn new(
        details: Details,
        logger: PointLogger,
        phantom: PhantomData<P>,
        artifacts: ArtifactApi,
    ) -> Self {
        let logger = logger.point(details.stub.point.clone());
        Self {
            details,
            logger,
            phantom,
            artifacts,
        }
    }
    pub fn bundle(&self) -> Result<Point, P::Err> {
        let config = self
            .details
            .properties
            .get("config")
            .ok_or::<P::Err>("expecting mechtron to have config property set".into())?;
        let config = Point::from_str(config.value.as_str())?;
        let bundle = config.to_bundle()?.push(":/")?;
        Ok(bundle)
    }

    pub fn raw_from_bundle<S: ToString>(&self, path: S) -> Result<ArtRef<Vec<u8>>, P::Err> {
        let point = self.bundle()?.push(path)?;
        Ok(self.artifacts.raw(&point)?)
    }
}

/// MechtronLifecycle is the interface used by Guest
/// to make important calls to the Mechtron
pub trait MechtronLifecycle<P>: DirectedHandler + Sync + Send
where
    P: Platform,
{
    fn create(&self, _skel: MechtronSkel<P>) -> Result<(), P::Err> {
        Ok(())
    }
}

/// Create a Mechtron by implementing this trait.
/// Mechtrons are created per request and disposed of afterwards...
/// Implementers of this trait should only hold references to
/// Mechtron::Skel, Mechtron::Cache & Mechtron::State at most.
pub trait Mechtron<P>: MechtronLifecycle<P> + Sync + Send + 'static
where
    P: Platform,
{
    /// it is recommended to implement MechtronSkel or some derivative
    /// of MechtronSkel. Skel holds info about the Mechtron (like it's Point,
    /// exact Kind & Properties)  The Skel may also provide access to other
    /// services within the Guest. If your Mechtron doesn't use the Skel
    /// then implement ```type Skel=()```
    type Skel;

    /// Is any static data (templates, config files) that does not change
    /// and may need to be reused. If your Mechtron doesn't need a Cache
    /// then implement ```type Cache=()``
    type Cache;

    /// State is the aspect of the Mechtron that is changeable.  It is recommended
    /// to wrap State in a Mutex or RwLock if used.  If you are implementing
    /// a stateless mechtron then implement ```type State=();```
    type State;

    /// This method is called by a companion MechtronFactory implementation
    /// to bring this Mechtron back to life to handle an Init or a Directed Wave
    fn restore(skel: Self::Skel, cache: Self::Cache, state: Self::State) -> Self;

    /// create the Cache for this Mechtron (templates, configs & static content)
    /// the cache should hold any static content that is expected to be unchanging
    fn cache(_skel: Self::Skel) -> Result<Option<Self::Cache>, P::Err> {
        Ok(None)
    }
}
