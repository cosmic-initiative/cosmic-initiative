use std::str::FromStr;
use std::sync::Arc;
use chrono::{DateTime, Utc};
use uuid::Uuid;
use cosmic_api::id::{ArtifactSubKind, BaseSubKind, FileSubKind, StarSub, UserBaseSubKind};
use cosmic_api::{ArtifactApi};
use cosmic_api::command::request::create::KindTemplate;
use cosmic_api::error::MsgErr;
use cosmic_api::id::id::{BaseKind, Kind, Specific, ToBaseKind};
use cosmic_api::property::{AnythingPattern, BoolPattern, EmailPattern, PointPattern, PropertiesConfig, PropertyPermit, PropertySource, U64Pattern, UsernamePattern};
use cosmic_api::substance::substance::Token;
use cosmic_artifact::Artifacts;
use cosmic_hyperlane::InterchangeEntryRouter;
use cosmic_platform::driver::DriversBuilder;
use cosmic_platform::machine::MachineTemplate;
use cosmic_platform::Platform;
use cosmic_platform::{Registry, RegistryApi};
use cosmic_registry_postgres::{PostErr, PostgresRegistry};

#[macro_use]
extern crate lazy_static;

lazy_static! {
    pub static ref STARLANE_PORT: usize = std::env::var("STARLANE_PORT").unwrap_or("4343".to_string()).parse::<usize>().unwrap_or(4343);
    pub static ref STARLANE_DATA_DIR: String= std::env::var("STARLANE_DATA_DIR").unwrap_or("data".to_string());
    pub static ref STARLANE_CACHE_DIR: String = std::env::var("STARLANE_CACHE_DIR").unwrap_or("data".to_string());
    pub static ref STARLANE_TOKEN: String = std::env::var("STARLANE_TOKEN").unwrap_or(Uuid::new_v4().to_string());
}
#[no_mangle]
pub extern "C" fn cosmic_uuid() -> String
{
    Uuid::new_v4().to_string()
}


#[no_mangle]
pub extern "C" fn cosmic_timestamp() -> DateTime<Utc>{
    Utc::now()
}


fn main() {
    println!("Hello, world!");
}

#[derive(Clone)]
pub struct Starlane {
   registry: Registry<Self>,
   artifacts: ArtifactApi
}

impl Platform for Starlane {
    type Err = PostErr;

    fn machine_template(&self) -> MachineTemplate {
        MachineTemplate::default()
    }

    fn properties_config<K: ToBaseKind>(&self, base:&K) -> &'static PropertiesConfig {
        match base.to_base(){
            BaseKind::Space => &UNREQUIRED_BIND_AND_CONFIG_PROERTIES_CONFIG,
            BaseKind::UserBase => &USER_BASE_PROPERTIES_CONFIG,
            BaseKind::User => &USER_PROPERTIES_CONFIG,
            BaseKind::App => &MECHTRON_PROERTIES_CONFIG,
            BaseKind::Mechtron => &MECHTRON_PROERTIES_CONFIG,
            _ => &DEFAULT_PROPERTIES_CONFIG
        }
    }

    fn drivers_builder(&self, kind: &StarSub) -> DriversBuilder {
        match kind {
            StarSub::Central => {}
            StarSub::Super => {}
            StarSub::Nexus => {}
            StarSub::Maelstrom => {}
            StarSub::Scribe => {}
            StarSub::Jump => {}
            StarSub::Fold => {}
        }
        DriversBuilder::new()
    }

    fn token(&self) -> Token {
        Token::new(STARLANE_TOKEN.to_string())
    }

    fn registry(&self) -> Registry<Self>  {
        self.registry.clone()
    }

    fn artifacts(&self) -> ArtifactApi {
       self.artifacts.clone()
    }

    fn start_services(&self, entry_router: &mut InterchangeEntryRouter) {
        todo!()
    }

    fn default_implementation(&self, template: &KindTemplate) -> Result<Kind, MsgErr> {
        let base: BaseKind = BaseKind::from_str(template.base.to_string().as_str())?;
        match base {
            BaseKind::UserBase => match &template.sub{
                None => {
                    return Err("SubKind must be set for UserBase<?>".into());
                }
                Some(sub) => {
                    match sub.as_str() {
                        "OAuth" => {
                            let specific = Specific::from_str("starlane.io:redhat.com:keycloak:community:18.0.0")?;
                            let sub = UserBaseSubKind::OAuth(specific);
                            Ok(Kind::UserBase(sub))
                        }
                        what => {
                            return Err(format!("unrecognized UserBase sub: '{}'", what ).into())
                        }
                    }
                }
            },
            _ => {
                Platform::default_implementation(self, template )
            }
        }
    }
}


lazy_static! {
    pub static ref DEFAULT_PROPERTIES_CONFIG: PropertiesConfig = default_properties_config();
    pub static ref USER_PROPERTIES_CONFIG: PropertiesConfig = user_properties_config();
    pub static ref USER_BASE_PROPERTIES_CONFIG: PropertiesConfig = userbase_properties_config();
    pub static ref MECHTRON_PROERTIES_CONFIG: PropertiesConfig = mechtron_properties_config();
    pub static ref UNREQUIRED_BIND_AND_CONFIG_PROERTIES_CONFIG: PropertiesConfig = unrequired_bind_and_config_properties_config();
}

fn default_properties_config() -> PropertiesConfig {
    let mut builder = PropertiesConfig::builder();
    builder.build()
}

fn mechtron_properties_config() -> PropertiesConfig {
    let mut builder = PropertiesConfig::builder();
    builder.add("bind", Box::new(PointPattern {}), true, false, PropertySource::Shell, None, false, vec![] );
    builder.add("config", Box::new(PointPattern {}), true, false, PropertySource::Shell, None, false, vec![] );
    builder.build()
}


fn unrequired_bind_and_config_properties_config() -> PropertiesConfig {
    let mut builder = PropertiesConfig::builder();
    builder.add("bind", Box::new(PointPattern {}), false, false, PropertySource::Shell, None, false, vec![] );
    builder.add("config", Box::new(PointPattern {}), false, false, PropertySource::Shell, None, false, vec![] );
    builder.build()
}

fn user_properties_config() -> PropertiesConfig {
    let mut builder = PropertiesConfig::builder();
    builder.add("bind", Box::new(PointPattern {}), true, false, PropertySource::Shell, Some("hyperspace:repo:boot:1.0.0:/bind/user.bind".to_string()), true, vec![] );
    builder.add("username", Box::new(UsernamePattern{}), false, false, PropertySource::Core, None, false, vec![] );
    builder.add("email", Box::new(EmailPattern{}), false, true, PropertySource::Core, None, false, vec![PropertyPermit::Read] );
    builder.add("password", Box::new(AnythingPattern{}), false, true, PropertySource::CoreSecret, None, false, vec![] );
    builder.build()
}

fn userbase_properties_config() -> PropertiesConfig {
    let mut builder = PropertiesConfig::builder();
    builder.add("bind", Box::new(PointPattern {}), true, false, PropertySource::Shell, Some("hyperspace:repo:boot:1.0.0:/bind/userbase.bind".to_string()), true, vec![] );
    builder.add("config", Box::new(PointPattern {}), false, true, PropertySource::Shell, None, false, vec![] );
    builder.add("registration-email-as-username", Box::new(BoolPattern{}), false, false, PropertySource::Shell, Some("true".to_string()), false, vec![] );
    builder.add("verify-email", Box::new(BoolPattern{}), false, false, PropertySource::Shell, Some("false".to_string()), false, vec![] );
    builder.add("sso-session-max-lifespan", Box::new(U64Pattern{}), false, true, PropertySource::Core, Some("315360000".to_string()), false, vec![] );
    builder.build()
}


pub fn properties_config<K: ToBaseKind>(base:&K) -> &'static PropertiesConfig {
    match base.to_base(){
        BaseKind::Space => &UNREQUIRED_BIND_AND_CONFIG_PROERTIES_CONFIG,
        BaseKind::UserBase => &USER_BASE_PROPERTIES_CONFIG,
        BaseKind::User => &USER_PROPERTIES_CONFIG,
        BaseKind::App => &MECHTRON_PROERTIES_CONFIG,
        BaseKind::Mechtron => &MECHTRON_PROERTIES_CONFIG,
        _ => &DEFAULT_PROPERTIES_CONFIG
    }
}