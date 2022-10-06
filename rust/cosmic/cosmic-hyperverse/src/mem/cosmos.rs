use crate::driver::artifact::{
    ArtifactDriverFactory, BundleDriverFactory, BundleSeriesDriverFactory, RepoDriverFactory,
};
use crate::driver::base::BaseDriverFactory;
use crate::driver::control::ControlDriverFactory;
use crate::driver::mechtron::{HostDriverFactory, MechtronDriverFactory};
use crate::driver::root::RootDriverFactory;
use crate::driver::space::SpaceDriverFactory;
use crate::driver::DriverAvail;
use crate::mem::registry::{MemRegApi, MemRegCtx};
use crate::{Cosmos, DriversBuilder, MachineTemplate};
use cosmic_hyperlane::{AnonHyperAuthenticator, HyperGate, LocalHyperwayGateJumper};
use cosmic_universe::artifact::{ArtifactApi, ReadArtifactFetcher};
use cosmic_universe::err::UniErr;
use cosmic_universe::kind::{BaseKind, Kind, StarSub};
use cosmic_universe::loc::{MachineName, StarKey, ToBaseKind};
use cosmic_universe::particle::property::{PropertiesConfig, PropertiesConfigBuilder};
use mechtron_host::err::HostErr;
use std::io;
use std::io::Error;
use std::str::Utf8Error;
use std::string::FromUtf8Error;
use std::sync::Arc;
use tokio::sync::oneshot;
use tokio::sync::oneshot::error::RecvError;
use tokio::time::error::Elapsed;
use wasmer::{CompileError, ExportError, InstantiationError, RuntimeError};
use crate::err::{CosmicErr, HyperErr};
use crate::reg::Registry;

impl MemCosmos {
    pub fn new() -> Self {
        Self {
            ctx: MemRegCtx::new(),
        }
    }
}

#[derive(Clone)]
pub struct MemCosmos {
    pub ctx: MemRegCtx,
}

#[async_trait]
impl Cosmos for MemCosmos {
    type Err = CosmicErr;
    type RegistryContext = MemRegCtx;
    type StarAuth = AnonHyperAuthenticator;
    type RemoteStarConnectionFactory = LocalHyperwayGateJumper;

    fn star_auth(&self, star: &StarKey) -> Result<Self::StarAuth, Self::Err> {
        Ok(AnonHyperAuthenticator::new())
    }

    fn remote_connection_factory_for_star(
        &self,
        star: &StarKey,
    ) -> Result<Self::RemoteStarConnectionFactory, Self::Err> {
        todo!()
    }

    fn machine_template(&self) -> MachineTemplate {
        MachineTemplate::default()
    }

    fn machine_name(&self) -> MachineName {
        "mem".to_string()
    }

    fn properties_config(&self, kind: &Kind) -> PropertiesConfig {
        let mut builder = PropertiesConfigBuilder::new();
        builder.kind(kind.clone());
        match kind.to_base() {
            BaseKind::Mechtron => {
                builder.add_point("config", true, true).unwrap();
                builder.build().unwrap()
            }
            _ => builder.build().unwrap(),
        }
    }

    fn drivers_builder(&self, kind: &StarSub) -> DriversBuilder<Self> {
        let mut builder = DriversBuilder::new(kind.clone());

        // only allow external Base wrangling external to Super
        if *kind == StarSub::Super {
            builder.add_post(Arc::new(BaseDriverFactory::new(DriverAvail::External)));
        } else {
            builder.add_post(Arc::new(BaseDriverFactory::new(DriverAvail::Internal)));
        }

        match kind {
            StarSub::Central => {
                builder.add_post(Arc::new(RootDriverFactory::new()));
            }
            StarSub::Super => {
                builder.add_post(Arc::new(SpaceDriverFactory::new()));
            }
            StarSub::Nexus => {}
            StarSub::Maelstrom => {
                builder.add_post(Arc::new(HostDriverFactory::new()));
                builder.add_post(Arc::new(MechtronDriverFactory::new()));
            }
            StarSub::Scribe => {
                builder.add_post(Arc::new(RepoDriverFactory::new()));
                builder.add_post(Arc::new(BundleSeriesDriverFactory::new()));
                builder.add_post(Arc::new(BundleDriverFactory::new()));
                builder.add_post(Arc::new(ArtifactDriverFactory::new()));
            }
            StarSub::Jump => {
                //                builder.add_post(Arc::new(ControlDriverFactory::new()));
            }
            StarSub::Fold => {}
            StarSub::Machine => {
                builder.add_post(Arc::new(ControlDriverFactory::new()));
            }
        }

        builder
    }

    async fn global_registry(&self) -> Result<Registry<Self>, Self::Err> {
        Ok(Arc::new(MemRegApi::new(self.ctx.clone())))
    }

    async fn star_registry(&self, star: &StarKey) -> Result<Registry<Self>, Self::Err> {
        todo!()
    }

    fn artifact_hub(&self) -> ArtifactApi {
        ArtifactApi::no_fetcher()
    }

    /*
    fn artifact_hub(&self) -> ArtifactApi {
        ArtifactApi::new(Arc::new(ReadArtifactFetcher::new()))
    }

     */

    fn start_services(&self, gate: &Arc<dyn HyperGate>) {}
}
