use crate::artifact::ArtRef;
use crate::config::mechtron::MechtronConfig;
use crate::point::ToSurface;
use crate::wave::core::cmd::CmdMethod;
use crate::wave::exchange::asynch::ProtoTransmitter;
use crate::wave::DirectedProto;
use crate::{Bin, BindConfig, Point, SpaceErr, Stub, Substance};
use dashmap::DashMap;
use serde::Serialize;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::watch;

#[derive(Clone)]
pub struct ArtifactApi {
    binds: Arc<DashMap<Point, Arc<BindConfig>>>,
    mechtrons: Arc<DashMap<Point, Arc<MechtronConfig>>>,
    wasm: Arc<DashMap<Point, Bin>>,
    fetcher_tx: Arc<watch::Sender<Arc<dyn ArtifactFetcher>>>,
    fetcher_rx: watch::Receiver<Arc<dyn ArtifactFetcher>>,
}

impl ArtifactApi {
    pub fn no_fetcher() -> Self {
        let fetcher = Arc::new(NoDiceArtifactFetcher);
        Self::new(fetcher)
    }

    pub fn new(fetcher: Arc<dyn ArtifactFetcher>) -> Self {
        let (fetcher_tx, fetcher_rx) = watch::channel(fetcher);
        let fetcher_tx = Arc::new(fetcher_tx);
        Self {
            binds: Arc::new(DashMap::new()),
            mechtrons: Arc::new(DashMap::new()),
            wasm: Arc::new(DashMap::new()),
            fetcher_tx,
            fetcher_rx,
        }
    }

    pub async fn set_fetcher(&self, fetcher: Arc<dyn ArtifactFetcher>) {
        self.fetcher_tx.send(fetcher);
    }

    fn get_fetcher(&self) -> Arc<dyn ArtifactFetcher> {
        self.fetcher_rx.borrow().clone()
    }

    pub async fn mechtron(&self, point: &Point) -> Result<ArtRef<MechtronConfig>, SpaceErr> {
        {
            if self.mechtrons.contains_key(point) {
                let mechtron = self.mechtrons.get(point).unwrap().clone();
                return Ok(ArtRef::new(mechtron, point.clone()));
            }
        }

        let mechtron: Arc<MechtronConfig> = Arc::new(self.fetch(point).await?);
        self.mechtrons.insert(point.clone(), mechtron.clone());
        return Ok(ArtRef::new(mechtron, point.clone()));
    }

    pub async fn bind(&self, point: &Point) -> Result<ArtRef<BindConfig>, SpaceErr> {
        {
            if self.binds.contains_key(point) {
                let bind = self.binds.get(point).unwrap().clone();
                return Ok(ArtRef::new(bind, point.clone()));
            }
        }

        let bind: Arc<BindConfig> = Arc::new(self.fetch(point).await?);
        {
            self.binds.insert(point.clone(), bind.clone());
        }
        return Ok(ArtRef::new(bind, point.clone()));
    }

    pub async fn wasm(&self, point: &Point) -> Result<ArtRef<Bin>, SpaceErr> {
        {
            if self.wasm.contains_key(point) {
                let wasm = self.wasm.get(point).unwrap().clone();
                return Ok(ArtRef::new(Arc::new(wasm), point.clone()));
            }
        }

        let wasm = self.get_fetcher().fetch(point).await?;
        {
            self.wasm.insert(point.clone(), wasm.clone());
        }
        return Ok(ArtRef::new(Arc::new(wasm), point.clone()));
    }

    async fn fetch<A>(&self, point: &Point) -> Result<A, SpaceErr>
    where
        A: TryFrom<Bin, Error = SpaceErr>,
    {
        if !point.has_bundle() {
            return Err("point is not from a bundle".into());
        }
        let bin = self.get_fetcher().fetch(point).await?;
        Ok(A::try_from(bin)?)
    }
}

pub struct FetchChamber {
    pub fetcher: Box<dyn ArtifactFetcher>,
}

impl FetchChamber {
    pub fn set(&mut self, fetcher: Box<dyn ArtifactFetcher>) {
        self.fetcher = fetcher;
    }
}

#[async_trait]
pub trait ArtifactFetcher: Send + Sync {
    async fn stub(&self, point: &Point) -> Result<Stub, SpaceErr>;
    async fn fetch(&self, point: &Point) -> Result<Bin, SpaceErr>;
}

pub struct NoDiceArtifactFetcher;

#[async_trait]
impl ArtifactFetcher for NoDiceArtifactFetcher {
    async fn stub(&self, point: &Point) -> Result<Stub, SpaceErr> {
        Err("cannot pull artifacts right now".into())
    }

    async fn fetch(&self, point: &Point) -> Result<Bin, SpaceErr> {
        Err("cannot pull artifacts right now".into())
    }
}

pub struct ReadArtifactFetcher {
    transmitter: ProtoTransmitter,
}

impl ReadArtifactFetcher {
    pub fn new(transmitter: ProtoTransmitter) -> Self {
        Self { transmitter }
    }
}

#[async_trait]
impl ArtifactFetcher for ReadArtifactFetcher {
    async fn stub(&self, point: &Point) -> Result<Stub, SpaceErr> {
        Err(SpaceErr::from_status(404u16))
    }

    async fn fetch(&self, point: &Point) -> Result<Bin, SpaceErr> {
        let mut directed = DirectedProto::ping();
        directed.to(point.clone().to_surface());
        directed.method(CmdMethod::Read);
        let pong = self.transmitter.ping(directed).await?;
        pong.core.ok_or()?;
        match pong.variant.core.body {
            Substance::Bin(bin) => Ok(bin),
            other => Err(SpaceErr::server_error(format!(
                "expected Bin, encountered unexpected substance {} when fetching Artifact",
                other.kind().to_string()
            ))),
        }
    }
}

pub struct MapFetcher {
    pub map: HashMap<Point, Bin>,
}

#[async_trait]
impl ArtifactFetcher for MapFetcher {
    async fn stub(&self, point: &Point) -> Result<Stub, SpaceErr> {
        todo!()
    }

    async fn fetch(&self, point: &Point) -> Result<Bin, SpaceErr> {
        let rtn = self.map.get(point).ok_or(SpaceErr::not_found(format!(
            "could not find {}",
            point.to_string()
        )))?;
        Ok(rtn.clone())
    }
}

impl MapFetcher {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    pub fn ser<S: Serialize>(&mut self, point: &Point, bin: S) {
        let bin = Arc::new(bincode::serialize(&bin).unwrap());
        self.map.insert(point.clone(), bin);
    }

    pub fn str<S: ToString>(&mut self, point: &Point, string: S) {
        let bin = Arc::new(string.to_string().into_bytes());
        self.map.insert(point.clone(), bin);
    }
}
