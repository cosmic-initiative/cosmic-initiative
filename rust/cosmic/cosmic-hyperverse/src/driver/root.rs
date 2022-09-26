use crate::driver::{
    Driver, DriverCtx, DriverSkel, HyperDriverFactory, Item, ItemHandler, ItemSphere,
};
use crate::star::HyperStarSkel;
use crate::Cosmos;
use cosmic_universe::artifact::ArtRef;
use cosmic_universe::config::bind::BindConfig;
use cosmic_universe::kind::{BaseKind, Kind};
use cosmic_universe::loc::Point;
use cosmic_universe::parse::bind_config;
use cosmic_universe::selector::KindSelector;
use cosmic_universe::util::log;
use cosmic_universe::wave::core::{CoreBounce, ReflectedCore};
use cosmic_universe::wave::exchange::asynch::DirectedHandler;
use std::marker::PhantomData;
use std::str::FromStr;
use std::sync::Arc;
use cosmic_universe::wave::exchange::asynch::RootInCtx;

lazy_static! {
    static ref ROOT_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(root_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/root.bind").unwrap()
    );
}

fn root_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    { }
    "#,
    ))
    .unwrap()
}

pub struct RootDriverFactory;

impl RootDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for RootDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> KindSelector {
        KindSelector::from_base(BaseKind::Root)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(RootDriver {}))
    }
}

pub struct RootDriver;

#[async_trait]
impl<P> Driver<P> for RootDriver
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Root
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        Ok(ItemSphere::Handler(Box::new(Root::restore((), (), ()))))
    }
}

pub struct Root<P>
where
    P: Cosmos,
{
    phantom: PhantomData<P>,
}

impl<P> Root<P>
where
    P: Cosmos,
{
    pub fn new() -> Self {
        Self {
            phantom: PhantomData::default(),
        }
    }
}

impl<P> Item<P> for Root<P>
where
    P: Cosmos,
{
    type Skel = ();
    type Ctx = ();
    type State = ();

    fn restore(skel: Self::Skel, ctx: Self::Ctx, state: Self::State) -> Self {
        Self::new()
    }
}

#[handler]
impl<P> Root<P> where P: Cosmos {}

#[async_trait]
impl<P> ItemHandler<P> for Root<P>
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(ROOT_BIND_CONFIG.clone())
    }
}
