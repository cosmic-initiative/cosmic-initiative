#![allow(warnings)]

#[macro_use]
extern crate async_recursion;
#[macro_use]
extern crate async_trait;
#[macro_use]
extern crate cosmic_macros;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate strum_macros;

extern crate inflector;
use cosmic_space::wasm::Timestamp;
use inflector::Inflector;

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::str::FromStr;
use std::sync::Arc;

use chrono::{DateTime, Utc};
use tokio::io;
use tokio::runtime::{Handle, Runtime};
use tokio::sync::{mpsc, oneshot};
use tracing::error;
use uuid::Uuid;

use cosmic_hyperlane::{HyperAuthenticator, HyperGate, HyperGateSelector, HyperwayEndpointFactory};
use cosmic_space::artifact::asynch::ArtifactApi;
use cosmic_space::command::common::{SetProperties, SetRegistry};
use cosmic_space::command::direct::create::{KindTemplate, Strategy};
use cosmic_space::command::direct::delete::Delete;
use cosmic_space::command::direct::query::{Query, QueryResult};
use cosmic_space::command::direct::select::{Select, SubSelect};
use cosmic_space::err::SpaceErr;
use cosmic_space::fail::Timeout;
use cosmic_space::hyper::{ParticleLocation, ParticleRecord};
use cosmic_space::loc::{
    Layer, MachineName, Point, RouteSeg, StarKey, Surface, ToBaseKind, ToSurface,
};
use cosmic_space::log::RootLogger;
use cosmic_space::particle::property::{PropertiesConfig, PropertiesConfigBuilder};
use cosmic_space::particle::{Details, Properties, Status, Stub};
use cosmic_space::security::IndexedAccessGrant;
use cosmic_space::security::{Access, AccessGrant};
use cosmic_space::selector::Selector;
use cosmic_space::settings::Timeouts;
use cosmic_space::substance::{Substance, SubstanceList, Token};
use cosmic_space::wave::core::http2::StatusCode;
use cosmic_space::wave::core::ReflectedCore;
use cosmic_space::wave::UltraWave;
use err::HyperErr;
use mechtron_host::err::HostErr;
use reg::Registry;

use crate::driver::{DriverFactory, DriversBuilder};
use crate::machine::{Machine, MachineApi, MachineTemplate};

pub mod driver;
pub mod err;
pub mod global;
pub mod layer;
pub mod machine;
pub mod mem;
pub mod reg;
pub mod star;

#[cfg(test)]
pub mod tests;

#[no_mangle]
pub extern "C" fn cosmic_uuid() -> String {
    Uuid::new_v4().to_string()
}

#[no_mangle]
pub extern "C" fn cosmic_timestamp() -> Timestamp {
    Timestamp::new(Utc::now().timestamp_millis())
}

#[async_trait]
pub trait Cosmos: Send + Sync + Sized + Clone
where
    Self::Err: HyperErr,
    Self: 'static,
    Self::RegistryContext: Send + Sync,
    Self::StarAuth: HyperAuthenticator,
    Self::RemoteStarConnectionFactory: HyperwayEndpointFactory,
    Self::Err: HyperErr,
{
    type Err;
    type RegistryContext;
    type StarAuth;
    type RemoteStarConnectionFactory;

    fn machine(&self) -> MachineApi<Self> {
        Machine::new(self.clone())
    }

    fn star_auth(&self, star: &StarKey) -> Result<Self::StarAuth, Self::Err>;
    fn remote_connection_factory_for_star(
        &self,
        star: &StarKey,
    ) -> Result<Self::RemoteStarConnectionFactory, Self::Err>;

    fn machine_template(&self) -> MachineTemplate;
    fn machine_name(&self) -> MachineName;

    fn properties_config(&self, kind: &Kind) -> PropertiesConfig {
        let mut builder = PropertiesConfigBuilder::new();
        builder.kind(kind.clone());
        match kind.to_base() {
            BaseKind::Mechtron => {
                builder.add_point("config", true, true).unwrap();
                builder.build().unwrap()
            }
            BaseKind::Host => {
                builder.add_point("bin", true, true).unwrap();
                builder.build().unwrap()
            }
            _ => builder.build().unwrap(),
        }
    }

    fn drivers_builder(&self, kind: &StarSub) -> DriversBuilder<Self>;
    async fn global_registry(&self) -> Result<Registry<Self>, Self::Err>;
    async fn star_registry(&self, star: &StarKey) -> Result<Registry<Self>, Self::Err>;
    fn artifact_hub(&self) -> ArtifactApi;
    async fn start_services(&self, gate: &Arc<HyperGateSelector>) {}
    fn logger(&self) -> RootLogger {
        Default::default()
    }

    fn web_port(&self) -> Result<u16, Self::Err> {
        Ok(8080u16)
    }

    fn data_dir(&self) -> String {
        "./data/".to_string()
    }

    fn select_kind(&self, template: &KindTemplate) -> Result<Kind, SpaceErr> {
        let base: BaseKind = BaseKind::from_str(template.base.to_string().as_str())?;
        Ok(match base {
            BaseKind::Root => Kind::Root,
            BaseKind::Space => Kind::Space,
            BaseKind::Base => Kind::Base,
            BaseKind::User => Kind::User,
            BaseKind::App => Kind::App,
            BaseKind::Mechtron => Kind::Mechtron,
            BaseKind::FileSystem => Kind::FileSystem,
            BaseKind::File => match &template.sub {
                None => return Err("expected kind for File".into()),
                Some(kind) => {
                    let file_kind = FileSubKind::from_str(kind.as_str())?;
                    return Ok(Kind::File(file_kind));
                }
            },
            BaseKind::Database => {
                unimplemented!("need to write a SpecificPattern matcher...")
            }
            BaseKind::BundleSeries => Kind::BundleSeries,
            BaseKind::Bundle => Kind::Bundle,
            BaseKind::Artifact => match &template.sub {
                None => {
                    return Err("expected Sub for Artirtact".into());
                }
                Some(sub) => {
                    let artifact_kind = ArtifactSubKind::from_str(sub.as_str())?;
                    return Ok(Kind::Artifact(artifact_kind));
                }
            },
            BaseKind::Control => Kind::Control,
            BaseKind::UserBase => match &template.sub {
                None => {
                    return Err("SubKind must be set for UserBase<?>".into());
                }
                Some(sub) => {
                    let specific =
                        Specific::from_str("starlane.io:redhat.com:keycloak:community:18.0.0")?;
                    let sub = UserBaseSubKind::OAuth(specific);
                    Kind::UserBase(sub)
                }
            },
            BaseKind::Repo => Kind::Repo,
            BaseKind::Portal => Kind::Portal,
            BaseKind::Star => {
                unimplemented!()
            }
            BaseKind::Driver => Kind::Driver,
            BaseKind::Global => Kind::Global,
            BaseKind::Host => Kind::Host,
            BaseKind::Guest => Kind::Guest,
            BaseKind::Native => Kind::Native(NativeSub::Web),
        })
    }

    fn log<R>(result: Result<R, Self::Err>) -> Result<R, Self::Err> {
        if let Err(err) = result {
            println!("ERR: {}", err.to_string());
            Err(err)
        } else {
            result
        }
    }

    fn log_ctx<R>(ctx: &str, result: Result<R, Self::Err>) -> Result<R, Self::Err> {
        if let Err(err) = result {
            println!("{}: {}", ctx, err.to_string());
            Err(err)
        } else {
            result
        }
    }

    fn log_deep<R, E: ToString>(
        ctx: &str,
        result: Result<Result<R, Self::Err>, E>,
    ) -> Result<Result<R, Self::Err>, E> {
        match &result {
            Ok(Err(err)) => {
                println!("{}: {}", ctx, err.to_string());
            }
            Err(err) => {
                println!("{}: {}", ctx, err.to_string());
            }
            Ok(_) => {}
        }
        result
    }
}

pub struct Settings {
    pub timeouts: Timeouts,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            timeouts: Default::default(),
        }
    }
}

/*
#[derive(strum_macros::Display)]
pub enum Anatomy {
    FromHyperlane,
    ToGravity,
}

 */
