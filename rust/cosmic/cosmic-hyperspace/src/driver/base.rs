use crate::driver::{
    Driver, DriverAvail, DriverCtx, DriverSkel, HyperDriverFactory, ItemHandler, ItemSphere,
    DRIVER_BIND,
};
use crate::star::HyperStarSkel;
use crate::Cosmos;
use cosmic_space::artifact::ArtRef;
use cosmic_space::config::bind::BindConfig;
use cosmic_space::point::Point;
use cosmic_space::parse::bind_config;
use cosmic_space::selector::ProtoKindSelector;
use cosmic_space::util::log;
use std::str::FromStr;
use std::sync::Arc;

lazy_static! {
    static ref BASE_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(base_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/base.bind").unwrap()
    );
}

fn base_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
    }
    "#,
    ))
    .unwrap()
}

pub struct BaseDriverFactory {
    pub avail: DriverAvail,
}

impl BaseDriverFactory {
    pub fn new(avail: DriverAvail) -> Self {
        Self { avail }
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for BaseDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        ProtoKindSelector::from_base(BaseKind::Base)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(BaseDriver::new(self.avail.clone())))
    }
}

pub struct BaseDriver {
    pub avail: DriverAvail,
}

impl BaseDriver {
    pub fn new(avail: DriverAvail) -> Self {
        Self { avail }
    }
}

#[async_trait]
impl<P> Driver<P> for BaseDriver
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Base
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        println!("ITEM get BASE");
        Ok(ItemSphere::Handler(Box::new(Base)))
    }
}

pub struct Base;

#[handler]
impl Base {}

#[async_trait]
impl<P> ItemHandler<P> for Base
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(DRIVER_BIND.clone())
    }
}
