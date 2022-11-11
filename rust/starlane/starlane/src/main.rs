#![allow(warnings)]
use cosmic_hyperlane_tcp::CertGenerator;
use std::fs;
pub mod err;
pub mod properties;

#[macro_use]
extern crate async_trait;
#[macro_use]
extern crate lazy_static;

use std::collections::HashSet;

use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use chrono::{DateTime, Utc};
use tokio::io;
use tokio::runtime::Runtime;
use uuid::Uuid;

use crate::err::StarErr;
use cosmic_hyperlane::{
    AnonHyperAuthenticator, HyperGate, HyperGateSelector, LocalHyperwayGateJumper,
};
use cosmic_hyperspace::driver::artifact::{
    ArtifactDriverFactory, BundleDriverFactory, BundleSeriesDriverFactory, RepoDriverFactory,
};
use cosmic_hyperspace::driver::base::BaseDriverFactory;
use cosmic_hyperspace::driver::control::ControlDriverFactory;
use cosmic_hyperspace::driver::mechtron::{HostDriverFactory, MechtronDriverFactory};
use cosmic_hyperspace::driver::root::RootDriverFactory;
use cosmic_hyperspace::driver::space::SpaceDriverFactory;
use cosmic_hyperspace::driver::{DriverAvail, DriversBuilder};
use cosmic_hyperspace::machine::{Machine, MachineTemplate};
use cosmic_hyperspace::reg::{Registry, RegistryApi};
use cosmic_hyperspace::Cosmos;

#[cfg(feature = "postgres")]
use cosmic_registry_postgres::err::PostErr;

#[cfg(feature = "postgres")]
use cosmic_registry_postgres::{
    PostgresDbInfo, PostgresPlatform, PostgresRegistry, PostgresRegistryContext,
    PostgresRegistryContextHandle,
};

use cosmic_space::artifact::asynch::ArtifactApi;
use cosmic_space::artifact::asynch::ReadArtifactFetcher;
use cosmic_space::command::direct::create::KindTemplate;
use cosmic_space::err::SpaceErr;
use cosmic_space::loc::{MachineName, StarKey};
use cosmic_space::loc::{Point, ToBaseKind};
use cosmic_space::log::RootLogger;
use cosmic_space::particle::property::{
    AnythingPattern, BoolPattern, EmailPattern, PointPattern, PropertiesConfig,
    PropertiesConfigBuilder, PropertyPermit, PropertySource, U64Pattern, UsernamePattern,
};
use cosmic_space::substance::Token;

use cosmic_hyperlane_tcp::HyperlaneTcpServer;
use cosmic_hyperspace::driver::web::WebDriverFactory;
use cosmic_hyperspace::mem::registry::{MemRegApi, MemRegCtx};
use cosmic_space::loc;
use cosmic_space::wasm::Timestamp;

fn main() -> Result<(), StarErr> {
    ctrlc::set_handler(move || {
        std::process::exit(1);
    });

    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?;
    runtime.block_on(async move {
        let starlane = Starlane::new().await.unwrap();
        let machine_api = starlane.machine();
        tokio::time::timeout(Duration::from_secs(30), machine_api.wait_ready())
            .await
            .unwrap();
        println!("> STARLANE Ready!");
        // this is a dirty hack which is good enough for a 0.3.0 release...
        loop {
            tokio::time::sleep(Duration::from_secs(60)).await;
        }
        let cl = machine_api.clone();
        machine_api.await_termination().await.unwrap();
        cl.terminate();
    });
    Ok(())
}

lazy_static! {
    pub static ref STARLANE_CONTROL_PORT: u16 = std::env::var("STARLANE_PORT")
        .unwrap_or("4343".to_string())
        .parse::<u16>()
        .unwrap_or(4343);
    pub static ref STARLANE_DATA_DIR: String =
        std::env::var("STARLANE_DATA_DIR").unwrap_or("./data/".to_string());
    pub static ref STARLANE_CACHE_DIR: String =
        std::env::var("STARLANE_CACHE_DIR").unwrap_or("cache".to_string());
    pub static ref STARLANE_TOKEN: String =
        std::env::var("STARLANE_TOKEN").unwrap_or(Uuid::new_v4().to_string());
    pub static ref STARLANE_REGISTRY_URL: String =
        std::env::var("STARLANE_REGISTRY_URL").unwrap_or("localhost".to_string());
    pub static ref STARLANE_REGISTRY_USER: String =
        std::env::var("STARLANE_REGISTRY_USER").unwrap_or("postgres".to_string());
    pub static ref STARLANE_REGISTRY_PASSWORD: String =
        std::env::var("STARLANE_REGISTRY_PASSWORD").unwrap_or("password".to_string());
    pub static ref STARLANE_REGISTRY_DATABASE: String =
        std::env::var("STARLANE_REGISTRY_DATABASE").unwrap_or("postgres".to_string());
}

/*
#[no_mangle]
pub extern "C" fn cosmic_uuid() -> loc::Uuid {
    loc::Uuid::from(uuid::Uuid::new_v4()).unwrap()
}

#[no_mangle]
pub extern "C" fn cosmic_timestamp() -> Timestamp {
    Timestamp { millis: Utc::now().timestamp_millis() }
}

 */

#[derive(Clone)]
pub struct Starlane {
    //pub handle: PostgresRegistryContextHandle<Self>,
    pub ctx: MemRegCtx,
}

impl Starlane {
    pub async fn new() -> Result<Self, StarErr> {
        /*
        let db = <Self as PostgresPlatform>::lookup_registry_db()?;
        let mut set = HashSet::new();
        set.insert(db.clone());
        let ctx = Arc::new(PostgresRegistryContext::new(set).await?);
        let handle = PostgresRegistryContextHandle::new(&db, ctx);

         */
        let ctx = MemRegCtx::new();
        Ok(Self { ctx })
    }
}

#[async_trait]
impl Cosmos for Starlane {
    type Err = StarErr;
    #[cfg(feature = "postgres")]
    type RegistryContext = PostgresRegistryContextHandle<Self>;

    #[cfg(not(feature = "postgres"))]
    type RegistryContext = MemRegCtx;

    type StarAuth = AnonHyperAuthenticator;
    type RemoteStarConnectionFactory = LocalHyperwayGateJumper;

    fn data_dir(&self) -> String {
        STARLANE_DATA_DIR.clone()
    }

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
        "starlane".to_string()
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
                builder.add_post(Arc::new(WebDriverFactory::new()));
                // builder.add_post(Arc::new(ControlDriverFactory::new()));
            }
            StarSub::Fold => {}
            StarSub::Machine => {
                builder.add_post(Arc::new(ControlDriverFactory::new()));
            }
        }

        builder
    }

    async fn global_registry(&self) -> Result<Registry<Self>, Self::Err> {
        let logger = RootLogger::default();
        let logger = logger.point(Point::global_registry());
        /*
        Ok(Arc::new(
            PostgresRegistry::new(self.handle.clone(), self.clone(), logger).await?,
        ))
         */

        Ok(Arc::new(MemRegApi::new(self.ctx.clone())))
    }

    async fn star_registry(&self, star: &StarKey) -> Result<Registry<Self>, Self::Err> {
        todo!()
    }

    fn artifact_hub(&self) -> ArtifactApi {
        ArtifactApi::no_fetcher()
    }

    async fn start_services(&self, gate: &Arc<HyperGateSelector>) {
        let dir = match dirs::home_dir() {
            None => ".starlane/localhost/certs".to_string(),
            Some(path) => format!("{}/.starlane/localhost/certs", path.display()),
        };
        fs::create_dir_all(dir.as_str());

        let cert = format!("{}/cert.der", dir.as_str());
        let key = format!("{}/key.der", dir.as_str());
        let cert_path = Path::new(&cert);
        let key_path = Path::new(&key);

        if !cert_path.exists() || !key_path.exists() {
            CertGenerator::gen(vec!["localhost".to_string()])
                .unwrap()
                .write_to_dir(dir.clone())
                .await
                .unwrap();
        };

        let logger = self
            .logger()
            .point(Point::from_str("control-server").unwrap());
        let server =
            HyperlaneTcpServer::new(STARLANE_CONTROL_PORT.clone(), dir, gate.clone(), logger)
                .await
                .unwrap();
        server.start().unwrap();
    }
}

#[cfg(feature = "postgres")]
impl PostgresPlatform for Starlane {
    fn lookup_registry_db() -> Result<PostgresDbInfo, Self::Err> {
        Ok(PostgresDbInfo::new(
            STARLANE_REGISTRY_URL.to_string(),
            STARLANE_REGISTRY_USER.to_string(),
            STARLANE_REGISTRY_PASSWORD.to_string(),
            STARLANE_REGISTRY_DATABASE.to_string(),
        ))
    }

    fn lookup_star_db(star: &StarKey) -> Result<PostgresDbInfo, Self::Err> {
        Ok(PostgresDbInfo::new_with_schema(
            STARLANE_REGISTRY_URL.to_string(),
            STARLANE_REGISTRY_USER.to_string(),
            STARLANE_REGISTRY_PASSWORD.to_string(),
            STARLANE_REGISTRY_DATABASE.to_string(),
            star.to_sql_name(),
        ))
    }
}

#[cfg(test)]
pub mod test {
    #[test]
    pub fn test() {}
}
