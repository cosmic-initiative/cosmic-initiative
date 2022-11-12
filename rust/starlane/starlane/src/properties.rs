use cosmic_space::err::SpaceErr;
use cosmic_space::point::ToBaseKind;
use cosmic_space::particle::property::{
    AnythingPattern, BoolPattern, EmailPattern, PointPattern, PropertiesConfig, PropertyPermit,
    PropertySource, U64Pattern, UsernamePattern,
};

lazy_static! {
    pub static ref DEFAULT_PROPERTIES_CONFIG: PropertiesConfig =
        default_properties_config().unwrap();
    pub static ref USER_PROPERTIES_CONFIG: PropertiesConfig = user_properties_config().unwrap();
    pub static ref USER_BASE_PROPERTIES_CONFIG: PropertiesConfig =
        userbase_properties_config().unwrap();
    pub static ref MECHTRON_PROERTIES_CONFIG: PropertiesConfig =
        mechtron_properties_config().unwrap();
    pub static ref UNREQUIRED_BIND_AND_CONFIG_PROERTIES_CONFIG: PropertiesConfig =
        unrequired_bind_and_config_properties_config().unwrap();
}

fn default_properties_config() -> Result<PropertiesConfig, SpaceErr> {
    let mut builder = PropertiesConfig::builder();
    builder.build()
}

fn mechtron_properties_config() -> Result<PropertiesConfig, SpaceErr> {
    let mut builder = PropertiesConfig::builder();
    builder.add(
        "bind",
        Box::new(PointPattern {}),
        true,
        false,
        PropertySource::Shell,
        None,
        false,
        vec![],
    );
    builder.add(
        "config",
        Box::new(PointPattern {}),
        true,
        false,
        PropertySource::Shell,
        None,
        false,
        vec![],
    );
    builder.build()
}

fn unrequired_bind_and_config_properties_config() -> Result<PropertiesConfig, SpaceErr> {
    let mut builder = PropertiesConfig::builder();
    builder.add(
        "bind",
        Box::new(PointPattern {}),
        false,
        false,
        PropertySource::Shell,
        None,
        false,
        vec![],
    );
    builder.add(
        "config",
        Box::new(PointPattern {}),
        false,
        false,
        PropertySource::Shell,
        None,
        false,
        vec![],
    );
    builder.build()
}

fn user_properties_config() -> Result<PropertiesConfig, SpaceErr> {
    let mut builder = PropertiesConfig::builder();
    builder.add(
        "bind",
        Box::new(PointPattern {}),
        true,
        false,
        PropertySource::Shell,
        Some("hyperspace:repo:boot:1.0.0:/bind/user.bind".to_string()),
        true,
        vec![],
    );
    builder.add(
        "username",
        Box::new(UsernamePattern {}),
        false,
        false,
        PropertySource::Core,
        None,
        false,
        vec![],
    );
    builder.add(
        "email",
        Box::new(EmailPattern {}),
        false,
        true,
        PropertySource::Core,
        None,
        false,
        vec![PropertyPermit::Read],
    );
    builder.add(
        "password",
        Box::new(AnythingPattern {}),
        false,
        true,
        PropertySource::CoreSecret,
        None,
        false,
        vec![],
    );
    builder.build()
}

fn userbase_properties_config() -> Result<PropertiesConfig, SpaceErr> {
    let mut builder = PropertiesConfig::builder();
    builder.add(
        "bind",
        Box::new(PointPattern {}),
        true,
        false,
        PropertySource::Shell,
        Some("hyperspace:repo:boot:1.0.0:/bind/userbase.bind".to_string()),
        true,
        vec![],
    );
    builder.add(
        "config",
        Box::new(PointPattern {}),
        false,
        true,
        PropertySource::Shell,
        None,
        false,
        vec![],
    );
    builder.add(
        "registration-email-as-username",
        Box::new(BoolPattern {}),
        false,
        false,
        PropertySource::Shell,
        Some("true".to_string()),
        false,
        vec![],
    );
    builder.add(
        "verify-email",
        Box::new(BoolPattern {}),
        false,
        false,
        PropertySource::Shell,
        Some("false".to_string()),
        false,
        vec![],
    );
    builder.add(
        "sso-session-max-lifespan",
        Box::new(U64Pattern {}),
        false,
        true,
        PropertySource::Core,
        Some("315360000".to_string()),
        false,
        vec![],
    );
    builder.build()
}

pub fn properties_config<K: ToBaseKind>(base: &K) -> &'static PropertiesConfig {
    match base.to_base() {
        BaseKind::Space => &UNREQUIRED_BIND_AND_CONFIG_PROERTIES_CONFIG,
        BaseKind::UserBase => &USER_BASE_PROPERTIES_CONFIG,
        BaseKind::User => &USER_PROPERTIES_CONFIG,
        BaseKind::App => &MECHTRON_PROERTIES_CONFIG,
        BaseKind::Mechtron => &MECHTRON_PROERTIES_CONFIG,
        _ => &DEFAULT_PROPERTIES_CONFIG,
    }
}
