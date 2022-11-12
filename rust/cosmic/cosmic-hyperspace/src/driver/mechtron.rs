use crate::driver::{
    Driver, DriverAvail, DriverCtx, DriverHandler, DriverSkel, DriverStatus, HyperDriverFactory,
    Item, ItemCtx, ItemHandler, ItemRouter, ItemSkel, ItemSphere,
};
use crate::err::HyperErr;
use crate::star::{HyperStarSkel, LayerInjectionRouter};
use crate::Cosmos;
use cosmic_space::artifact::ArtRef;
use cosmic_space::command::common::{PropertyMod, SetProperties, StateSrc};
use cosmic_space::command::direct::create::{
    Create, PointSegTemplate, PointTemplate, Strategy, Template, TemplateDef,
};
use cosmic_space::config::bind::BindConfig;
use cosmic_space::err::SpaceErr;
use cosmic_space::hyper::{Assign, HyperSubstance, ParticleLocation};
use cosmic_space::point::{Layer, Point, ToSurface};
use cosmic_space::log::RootLogger;
use cosmic_space::parse::bind_config;
use cosmic_space::particle::traversal::{Traversal, TraversalDirection};
use cosmic_space::selector::ProtoKindSelector;
use cosmic_space::substance::Substance;
use cosmic_space::util::log;
use cosmic_space::wave::core::hyp::HypMethod;
use cosmic_space::wave::core::DirectedCore;
use cosmic_space::wave::exchange::asynch::ProtoTransmitterBuilder;
use cosmic_space::wave::exchange::asynch::{InCtx, TraversalRouter};
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{DirectedProto, DirectedWave, Pong, UltraWave, Wave};
use dashmap::DashMap;
use mechtron_host::{HostsApi, HostsCall, HostsRunner, WasmHostApi};
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc;

lazy_static! {
    static ref HOST_DRIVER_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(host_driver_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/host-driver.bind").unwrap()
    );
    static ref HOST_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(host_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/host.bind").unwrap()
    );
    static ref MECHTRON_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(mechtron_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/mechtron.bind").unwrap()
    );
}

fn host_driver_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
       Route -> {
           Hyp<Host> -> (()) => &;
           Hyp<Assign> -> (()) => &;
       }
    }
    "#,
    ))
    .unwrap()
}

fn host_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
       Route -> {
          Hyp<Transport> -> (());
       }
    }
    "#,
    ))
    .unwrap()
}

fn mechtron_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
       Route -> {
          Ext<*> -> (()) => &;
          Http<*> -> (()) => &;
       }
    }
    "#,
    ))
    .unwrap()
}

pub struct HostDriverFactory {}

impl HostDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for HostDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        ProtoKindSelector::from_base(BaseKind::Host)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(HostDriver::new(driver_skel, ctx)))
    }
}

pub struct HostDriver<P>
where
    P: Cosmos,
{
    pub skel: HostDriverSkel<P>,
    pub ctx: DriverCtx,
}

impl<P> HostDriver<P>
where
    P: Cosmos,
{
    pub fn new(skel: DriverSkel<P>, ctx: DriverCtx) -> Self {
        let skel = HostDriverSkel::new(skel);
        Self { skel, ctx }
    }
}

#[async_trait]
impl<P> Driver<P> for HostDriver<P>
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Host
    }
    async fn init(&mut self, skel: DriverSkel<P>, _ctx: DriverCtx) -> Result<(), P::Err> {
        skel.create_in_driver(
            PointSegTemplate::Exact("hosts".to_string()),
            Kind::Base.to_template(),
        )
        .await?;
        skel.logger
            .result(skel.status_tx.send(DriverStatus::Ready).await)
            .unwrap_or_default();
        Ok(())
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        let host = self.skel.hosts.get_via_point(point).await?.clone();
        let skel = HostItemSkel {
            skel: ItemSkel::new(point.clone(), Kind::Host, self.skel.skel.clone()),
            host,
        };
        Ok(ItemSphere::Handler(Box::new(HostItem::restore(
            skel,
            (),
            (),
        ))))
    }

    fn bind(&self) -> ArtRef<BindConfig> {
        HOST_DRIVER_BIND_CONFIG.clone()
    }

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        Box::new(HostDriverHandler::restore(
            self.skel.clone(),
            self.ctx.clone(),
        ))
    }
}

#[derive(Clone)]
pub struct HostDriverPlatform<P>
where
    P: Cosmos,
{
    logger: RootLogger,
    phantom: PhantomData<P>,
}

impl<P> HostDriverPlatform<P>
where
    P: Cosmos,
{
    pub fn new(logger: RootLogger) -> Self {
        let phantom: PhantomData<P> = PhantomData::default();
        Self { logger, phantom }
    }
}

#[derive(Clone)]
pub struct HostDriverSkel<P>
where
    P: Cosmos,
{
    pub skel: DriverSkel<P>,
    pub hosts: HostsApi,
    pub hosts_base: Point,
}

impl<P> HostDriverSkel<P>
where
    P: Cosmos,
{
    pub fn new(skel: DriverSkel<P>) -> Self {
        let mut router = LayerInjectionRouter::new(skel.skel.clone(), skel.point.to_surface());
        router.direction = Some(TraversalDirection::Fabric);
        let router = Arc::new(router);
        let transmitter = ProtoTransmitterBuilder::new(router, skel.skel.exchanger.clone());

        let hosts = HostsRunner::new(
            skel.skel.machine.artifacts.clone(),
            transmitter,
            skel.logger.logger.clone(),
        );
        let hosts_base = skel.point.push("hosts").unwrap();
        Self {
            skel,
            hosts,
            hosts_base,
        }
    }
}

pub struct HostDriverHandler<P>
where
    P: Cosmos,
{
    pub skel: HostDriverSkel<P>,
    pub ctx: DriverCtx,
}

impl<P> HostDriverHandler<P>
where
    P: Cosmos,
{
    fn restore(skel: HostDriverSkel<P>, ctx: DriverCtx) -> Self {
        HostDriverHandler { skel, ctx }
    }
}

impl<P> DriverHandler<P> for HostDriverHandler<P> where P: Cosmos {}

#[handler]
impl<P> HostDriverHandler<P>
where
    P: Cosmos,
{
    #[route("Hyp<Host>")]
    pub async fn host(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Host(host_cmd) = ctx.input {
            let config = host_cmd
                .details
                .properties
                .get("config")
                .ok_or("expected config property")
                .map_err(|e| SpaceErr::server_error(e))?;
            let config = Point::from_str(config.value.as_str())?;
            let config = self
                .skel
                .skel
                .skel
                .machine
                .artifacts
                .mechtron(&config)
                .await?;

            let host = if let Ok(host) = &self.skel.hosts.get_via_wasm(&config.wasm).await {
                host.clone()
            } else {
                let mut properties = SetProperties::new();
                properties.push(PropertyMod::Set {
                    key: "wasm".to_string(),
                    value: config.wasm.to_string(),
                    lock: false,
                });
                let create = Create {
                    template: Template {
                        point: PointTemplate {
                            parent: self.skel.hosts_base.clone(),
                            child_segment_template: PointSegTemplate::Pattern("host-%".to_string()),
                        },
                        kind: Kind::Host.to_template(),
                    },
                    properties,
                    strategy: Strategy::Commit,
                    state: StateSrc::None,
                };

                let mut create: DirectedProto = create.into();
                let pong = self.ctx.transmitter.ping(create).await?;
                pong.ok_or()?;

                self.skel.hosts.get_via_wasm(&config.wasm).await?
            };

            host.create_mechtron(host_cmd.clone()).await;

            self.skel
                .skel
                .registry()
                .assign_host(&host_cmd.details.stub.point, &host.point().await?)
                .await?;

            Ok(())
        } else {
            Err("expecting Host".into())
        }
    }

    #[route("Hyp<Assign>")]
    pub async fn assign(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Assign(assign) = ctx.input {
            let mut router = LayerInjectionRouter::new(
                self.skel.skel.skel.clone(),
                assign
                    .details
                    .stub
                    .point
                    .to_surface()
                    .with_layer(Layer::Core),
            );
            router.direction = Some(TraversalDirection::Fabric);
            let mut transmitter = ProtoTransmitterBuilder::new(
                Arc::new(router),
                self.skel.skel.skel.exchanger.clone(),
            );
            transmitter.via = SetStrategy::Override(assign.details.stub.point.clone().to_surface());
            let transmitter = transmitter.build();

            let wasm = self.skel.skel.logger.result(
                assign
                    .details
                    .properties
                    .get(&"wasm".to_string())
                    .ok_or("wasm property must be set for a Mechtron Host"),
            )?;
            let wasm_point = Point::from_str(wasm.value.as_str())?;
            self.skel
                .hosts
                .create(assign.details.clone(), wasm_point.clone())
                .await?;

            Ok(())
        } else {
            Err(P::Err::new("expected HyperSubstance<Assign>"))
        }
    }
}

#[derive(Clone)]
pub struct HostItemSkel<P>
where
    P: Cosmos,
{
    pub skel: ItemSkel<P>,
    pub host: WasmHostApi,
}

pub struct HostItem<P>
where
    P: Cosmos,
{
    pub skel: HostItemSkel<P>,
}

impl<P> Item<P> for HostItem<P>
where
    P: Cosmos,
{
    type Skel = HostItemSkel<P>;
    type Ctx = ();
    type State = ();

    fn restore(skel: Self::Skel, ctx: Self::Ctx, state: Self::State) -> Self {
        Self { skel }
    }
}

#[handler]
impl<P> HostItem<P>
where
    P: Cosmos,
{
    #[route("Hyp<Transport>")]
    async fn transport(&self, ctx: InCtx<'_, UltraWave>) {
        let wave = ctx
            .wave()
            .clone()
            .to_ultra()
            .unwrap_from_transport()
            .unwrap();
        if let Ok(Some(wave)) = self.skel.host.transmit_to_guest(wave) {
            if wave.is_reflected() {
                ctx.transmitter.route(wave).await;
            } else {
            }
        }
    }
}

#[async_trait]
impl<P> ItemHandler<P> for HostItem<P>
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(HOST_BIND_CONFIG.clone())
    }
}

pub struct MechtronDriverFactory {}

impl MechtronDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for MechtronDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        ProtoKindSelector::from_base(BaseKind::Mechtron)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(MechtronDriver::new(driver_skel, ctx)))
    }
}

pub struct MechtronDriver<P>
where
    P: Cosmos,
{
    pub ctx: DriverCtx,
    pub skel: DriverSkel<P>,
}

#[async_trait]
impl<P> Driver<P> for MechtronDriver<P>
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Mechtron
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        let ctx = self.skel.item_ctx(point, Layer::Core)?;
        let skel = ItemSkel::new(point.clone(), Kind::Mechtron, self.skel.clone());
        let mechtron = Mechtron::restore(skel, ctx, ());
        Ok(ItemSphere::Router(Box::new(mechtron)))
    }
    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        Box::new(MechtronDriverHandler::restore(
            self.skel.clone(),
            self.ctx.clone(),
        ))
    }
}

impl<P> MechtronDriver<P>
where
    P: Cosmos,
{
    pub fn new(skel: DriverSkel<P>, ctx: DriverCtx) -> Self {
        Self { skel, ctx }
    }
}

pub struct MechtronDriverHandler<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
    ctx: DriverCtx,
}

impl<P> MechtronDriverHandler<P>
where
    P: Cosmos,
{
    fn restore(skel: DriverSkel<P>, ctx: DriverCtx) -> Self {
        MechtronDriverHandler { skel, ctx }
    }
}

impl<P> DriverHandler<P> for MechtronDriverHandler<P> where P: Cosmos {}

#[handler]
impl<P> MechtronDriverHandler<P>
where
    P: Cosmos,
{
    #[route("Hyp<Assign>")]
    async fn assign(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Assign(assign) = ctx.input {
            let logger = self.skel.logger.push_mark("assign")?;

            let config = assign
                .details
                .properties
                .get(&"config".to_string())
                .ok_or("config property must be set for a Mechtron")?;

            let config = Point::from_str(config.value.as_str())?;
            let config = self
                .skel
                .logger
                .result(self.skel.artifacts().mechtron(&config).await)?;

            let config = config.contents();

            let host = self.skel.drivers().local_driver_lookup(Kind::Host).await?.ok_or(P::Err::new("missing Host Driver which must be on the same Star as the Mechtron Driver in order for it to work"))?;
            let mut wave = DirectedProto::ping();
            wave.method(HypMethod::Host);
            wave.to(host.to_surface().with_layer(Layer::Core));
            wave.body(HyperSubstance::Host(assign.clone().to_host_cmd(config)).into());
            let pong = self.ctx.transmitter.ping(wave).await?;
            pong.ok_or()?;
            Ok(())
        } else {
            Err(P::Err::new("MechtronDriverHandler expecting Assign"))
        }
    }
}

pub struct Mechtron<P>
where
    P: Cosmos,
{
    skel: ItemSkel<P>,
    ctx: ItemCtx,
}

impl<P> Item<P> for Mechtron<P>
where
    P: Cosmos,
{
    type Skel = ItemSkel<P>;
    type Ctx = ItemCtx;
    type State = ();

    fn restore(skel: Self::Skel, ctx: Self::Ctx, _state: Self::State) -> Self {
        Self { skel, ctx }
    }
}

#[async_trait]
impl<P> TraversalRouter for Mechtron<P>
where
    P: Cosmos,
{
    async fn traverse(&self, traversal: Traversal<UltraWave>) -> Result<(), SpaceErr> {
        let wave = traversal.payload;
        let record = self
            .skel
            .skel
            .registry()
            .record(&self.skel.point)
            .await
            .map_err(|e| e.to_space_err())?;
        let location = record.location;

        let host = location
            .host
            .ok_or::<SpaceErr>("expected Mechtron to have an assigned Host".into())?
            .to_surface()
            .with_layer(Layer::Core);

        let transport =
            wave.wrap_in_transport(self.skel.point.to_surface().with_layer(Layer::Core), host);
        self.ctx.transmitter.signal(transport).await?;
        Ok(())
    }
}

#[async_trait]
impl<P> ItemRouter<P> for Mechtron<P>
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(MECHTRON_BIND_CONFIG.clone())
    }
}
