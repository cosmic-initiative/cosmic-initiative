pub mod artifact;
pub mod base;
pub mod control;
pub mod mechtron;
pub mod root;
pub mod space;
pub mod star;
pub mod web;

use crate::driver::star::StarDriverFactory;
use crate::err::HyperErr;
use crate::reg::{Registration, Registry};
use crate::star::HyperStarCall::LayerTraversalInjection;
use crate::star::{HyperStarSkel, LayerInjectionRouter};
use crate::Cosmos;
use cosmic_space::artifact::asynch::ArtifactApi;
use cosmic_space::artifact::ArtRef;
use cosmic_space::command::common::{SetProperties, StateSrc};
use cosmic_space::command::direct::create::{
    Create, KindTemplate, PointSegTemplate, PointTemplate, Strategy, Template,
};
use cosmic_space::config::bind::BindConfig;
use cosmic_space::err::SpaceErr;
use cosmic_space::hyper::{Assign, HyperSubstance, ParticleLocation, ParticleRecord};
use cosmic_space::point::{Layer, Point, Surface, ToPoint, ToSurface};
use cosmic_space::log::{PointLogger, Tracker};
use cosmic_space::parse::bind_config;
use cosmic_space::particle::traversal::{
    Traversal, TraversalDirection, TraversalInjection, TraversalLayer,
};
use cosmic_space::particle::{Details, Status, Stub};
use cosmic_space::selector::ProtoKindSelector;
use cosmic_space::substance::Substance;
use cosmic_space::util::log;
use cosmic_space::wave::core::cmd::CmdMethod;
use cosmic_space::wave::core::{CoreBounce, Method, ReflectedCore};
use cosmic_space::wave::exchange::asynch::{
    DirectedHandler, Exchanger, InCtx, ProtoTransmitter, ProtoTransmitterBuilder, RootInCtx,
    Router, TraversalRouter,
};
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{Agent, DirectedWave, ReflectedWave, UltraWave};
use cosmic_space::HYPERUSER;
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use futures::future::select_all;
use futures::task::Spawn;
use futures::{FutureExt, TryFutureExt};
use std::collections::HashMap;
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{mpsc, oneshot, watch, RwLock};
use cosmic_space::kind::KindSelector;

lazy_static! {
    static ref DEFAULT_BIND: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(default_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/default.bind").unwrap()
    );
    static ref DRIVER_BIND: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(driver_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/driver.bind").unwrap()
    );
}

fn driver_bind() -> BindConfig {
    log(bind_config(
        r#" Bind(version=1.0.0) {

       Route<Hyp<Assign>> -> (()) => &;

    } "#,
    ))
    .unwrap()
}

fn default_bind() -> BindConfig {
    log(bind_config(r#" Bind(version=1.0.0) { } "#)).unwrap()
}

pub struct DriversBuilder<P>
where
    P: Cosmos,
{
    factories: Vec<Arc<dyn HyperDriverFactory<P>>>,
    kinds: Vec<ProtoKindSelector>,
    external_kinds: Vec<ProtoKindSelector>,
}

impl<P> DriversBuilder<P>
where
    P: Cosmos,
{
    pub fn new(kind: StarSub) -> Self {
        let mut pre: Vec<Arc<dyn HyperDriverFactory<P>>> = vec![];
        let mut kinds = vec![];
        let mut external_kinds = vec![];
        let driver_driver_factory = Arc::new(DriverDriverFactory::new());
        let star_factory = Arc::new(StarDriverFactory::new(kind.clone()));
        kinds.push(<DriverDriverFactory as HyperDriverFactory<P>>::kind(
            &driver_driver_factory,
        ));
        if <DriverDriverFactory as HyperDriverFactory<P>>::avail(&driver_driver_factory)
            == DriverAvail::External
        {
            external_kinds.push(<DriverDriverFactory as HyperDriverFactory<P>>::kind(
                &driver_driver_factory,
            ));
        }
        pre.push(driver_driver_factory);
        kinds.push(kind.to_selector());
        pre.push(star_factory);
        Self {
            factories: pre,
            kinds,
            external_kinds,
        }
    }

    pub fn kinds(&self) -> Vec<ProtoKindSelector> {
        self.kinds.clone()
    }

    pub fn add_post(&mut self, factory: Arc<dyn HyperDriverFactory<P>>) {
        self.kinds.push(factory.kind());
        if factory.avail() == DriverAvail::External {
            self.external_kinds.push(factory.kind());
        }
        self.factories.push(factory);
    }

    pub fn add_pre(&mut self, factory: Arc<dyn HyperDriverFactory<P>>) {
        self.kinds.insert(0, factory.kind());
        if factory.avail() == DriverAvail::External {
            self.external_kinds.insert(0, factory.kind());
        }
        self.factories.insert(0, factory);
    }

    pub fn build(
        self,
        skel: HyperStarSkel<P>,
        call_tx: mpsc::Sender<DriversCall<P>>,
        call_rx: mpsc::Receiver<DriversCall<P>>,
        status_tx: watch::Sender<DriverStatus>,
        status_rx: watch::Receiver<DriverStatus>,
    ) -> DriversApi<P> {
        let port = skel.point.push("drivers").unwrap().to_surface();
        Drivers::new(
            port,
            skel.clone(),
            self.factories,
            self.kinds,
            self.external_kinds,
            call_tx,
            call_rx,
            status_tx,
            status_rx,
        )
    }
}

pub enum DriversCall<P>
where
    P: Cosmos,
{
    Init0,
    Init1,
    AddDriver {
        driver: DriverApi<P>,
        rtn: oneshot::Sender<()>,
    },
    Visit(Traversal<UltraWave>),
    InternalKinds(oneshot::Sender<Vec<ProtoKindSelector>>),
    ExternalKinds(oneshot::Sender<Vec<ProtoKindSelector>>),
    Find {
        kind: Kind,
        rtn: oneshot::Sender<Option<DriverApi<P>>>,
    },
    FindExternalKind {
        kind: Kind,
        rtn: oneshot::Sender<Option<DriverApi<P>>>,
    },
    FindInternalKind {
        kind: Kind,
        rtn: oneshot::Sender<Option<DriverApi<P>>>,
    },
    Drivers(oneshot::Sender<HashMap<ProtoKindSelector, DriverApi<P>>>),
    Get {
        kind: Kind,
        rtn: oneshot::Sender<Result<DriverApi<P>, SpaceErr>>,
    },
    LocalDriverLookup {
        kind: Kind,
        rtn: oneshot::Sender<Option<Point>>,
    },
    Status {
        kind: ProtoKindSelector,
        rtn: oneshot::Sender<Result<DriverStatus, SpaceErr>>,
    },
    StatusRx(oneshot::Sender<watch::Receiver<DriverStatus>>),
    ByPoint {
        point: Point,
        rtn: oneshot::Sender<Option<DriverApi<P>>>,
    },
}

#[derive(Clone)]
pub struct DriversApi<P>
where
    P: Cosmos,
{
    call_tx: mpsc::Sender<DriversCall<P>>,
    status_rx: watch::Receiver<DriverStatus>,
}

impl<P> DriversApi<P>
where
    P: Cosmos,
{
    pub fn new(tx: mpsc::Sender<DriversCall<P>>, status_rx: watch::Receiver<DriverStatus>) -> Self {
        Self {
            call_tx: tx,
            status_rx,
        }
    }

    pub fn status(&self) -> DriverStatus {
        self.status_rx.borrow().clone()
    }

    pub async fn status_changed(&mut self) -> Result<DriverStatus, SpaceErr> {
        self.status_rx.changed().await?;
        Ok(self.status())
    }

    pub async fn visit(&self, traversal: Traversal<UltraWave>) {
        self.call_tx.send(DriversCall::Visit(traversal)).await;
    }

    pub async fn internal_kinds(&self) -> Result<Vec<ProtoKindSelector>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx.send(DriversCall::InternalKinds(rtn)).await?;
        Ok(rtn_rx.await?)
    }

    pub async fn external_kinds(&self) -> Result<Vec<ProtoKindSelector>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx.send(DriversCall::ExternalKinds(rtn)).await?;
        Ok(rtn_rx.await?)
    }

    pub async fn find(&self, kind: Kind) -> Result<Option<DriverApi<P>>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx.send(DriversCall::Find { kind, rtn }).await?;
        Ok(rtn_rx.await?)
    }

    pub async fn find_external(&self, kind: Kind) -> Result<Option<DriverApi<P>>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriversCall::FindExternalKind { kind, rtn })
            .await?;
        Ok(rtn_rx.await?)
    }

    pub async fn find_internal(&self, kind: Kind) -> Result<Option<DriverApi<P>>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriversCall::FindInternalKind { kind, rtn })
            .await?;
        Ok(rtn_rx.await?)
    }

    pub async fn local_driver_lookup(&self, kind: Kind) -> Result<Option<Point>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriversCall::LocalDriverLookup { kind, rtn })
            .await?;
        Ok(rtn_rx.await?)
    }

    pub async fn drivers(&self) -> Result<HashMap<ProtoKindSelector, DriverApi<P>>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx.send(DriversCall::Drivers(rtn)).await;
        Ok(rtn_rx.await?)
    }

    pub async fn get(&self, kind: &Kind) -> Result<DriverApi<P>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriversCall::Get {
                kind: kind.clone(),
                rtn,
            })
            .await;
        rtn_rx.await?
    }

    pub async fn find_by_point(&self, point: &Point) -> Result<Option<DriverApi<P>>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriversCall::ByPoint {
                point: point.clone(),
                rtn,
            })
            .await;
        Ok(rtn_rx.await?)
    }

    pub async fn init(&self) {
        self.call_tx.send(DriversCall::Init0).await;
    }

    /*
    pub async fn route(&self, wave: UltraWave) {
        self.call_tx.send( DriversCall::Route(wave)).await;
    }

     */
}

#[derive(DirectedHandler)]
pub struct Drivers<P>
where
    P: Cosmos + 'static,
{
    port: Surface,
    skel: HyperStarSkel<P>,
    factories: Vec<Arc<dyn HyperDriverFactory<P>>>,
    kind_to_driver: HashMap<ProtoKindSelector, DriverApi<P>>,
    point_to_driver: HashMap<Point, DriverApi<P>>,
    call_rx: mpsc::Receiver<DriversCall<P>>,
    call_tx: mpsc::Sender<DriversCall<P>>,
    statuses_rx: Arc<DashMap<ProtoKindSelector, watch::Receiver<DriverStatus>>>,
    status_tx: mpsc::Sender<DriverStatus>,
    status_rx: watch::Receiver<DriverStatus>,
    kinds: Vec<ProtoKindSelector>,
    external_kinds: Vec<ProtoKindSelector>,
    init: bool,
}

impl<P> Drivers<P>
where
    P: Cosmos + 'static,
{
    pub fn new(
        port: Surface,
        skel: HyperStarSkel<P>,
        factories: Vec<Arc<dyn HyperDriverFactory<P>>>,
        kinds: Vec<ProtoKindSelector>,
        external_kinds: Vec<ProtoKindSelector>,
        call_tx: mpsc::Sender<DriversCall<P>>,
        call_rx: mpsc::Receiver<DriversCall<P>>,
        watch_status_tx: watch::Sender<DriverStatus>,
        watch_status_rx: watch::Receiver<DriverStatus>,
    ) -> DriversApi<P> {
        let statuses_rx = Arc::new(DashMap::new());
        let kind_to_driver = HashMap::new();
        let point_to_driver = HashMap::new();
        let (mpsc_status_tx, mut mpsc_status_rx): (
            tokio::sync::mpsc::Sender<DriverStatus>,
            tokio::sync::mpsc::Receiver<DriverStatus>,
        ) = mpsc::channel(128);

        tokio::spawn(async move {
            while let Some(status) = mpsc_status_rx.recv().await {
                watch_status_tx.send(status.clone());
                if let DriverStatus::Fatal(_) = status {
                    break;
                }
            }
        });

        let mut drivers = Self {
            port,
            skel,
            kind_to_driver,
            point_to_driver,
            call_rx,
            call_tx: call_tx.clone(),
            statuses_rx,
            factories,
            status_tx: mpsc_status_tx,
            status_rx: watch_status_rx.clone(),
            init: false,
            kinds,
            external_kinds,
        };

        drivers.start();

        DriversApi::new(call_tx, watch_status_rx)
    }

    fn start(mut self) {
        tokio::spawn(async move {
            while let Some(call) = self.call_rx.recv().await {
                match call {
                    DriversCall::Init0 => {
                        self.init0().await;
                    }
                    DriversCall::Init1 => {
                        self.init1().await;
                    }
                    DriversCall::AddDriver { driver, rtn } => {
                        self.point_to_driver
                            .insert(driver.get_point(), driver.clone());
                        self.kind_to_driver
                            .insert(driver.kind.clone(), driver.clone());
                        if !driver.kind.matches(&Kind::Driver) {
                            let driver_driver = self.find(&Kind::Driver).unwrap();
                            driver_driver.add_driver(driver.clone()).await;
                        }
                        rtn.send(());
                    }
                    DriversCall::Visit(traversal) => {
                        self.visit(traversal).await;
                    }
                    DriversCall::InternalKinds(rtn) => {
                        rtn.send(self.internal_kinds());
                    }
                    DriversCall::Drivers(rtn) => {
                        rtn.send(self.kind_to_driver.clone()).unwrap_or_default();
                    }
                    DriversCall::Status { kind, rtn } => match self.statuses_rx.get(&kind) {
                        None => {
                            rtn.send(Err(SpaceErr::not_found("status_rx")));
                        }
                        Some(status_rx) => {
                            rtn.send(Ok(status_rx.borrow().clone()));
                        }
                    },
                    DriversCall::StatusRx(rtn) => {
                        rtn.send(self.status_rx.clone());
                    }
                    DriversCall::Get { kind, rtn } => {
                        rtn.send(
                            self.find(&kind).cloned().ok_or(
                                format!("star does not have driver for kind: {}", kind.to_string())
                                    .into(),
                            ),
                        );
                    }
                    DriversCall::ExternalKinds(rtn) => {
                        rtn.send(self.external_kinds.clone());
                    }
                    DriversCall::FindExternalKind { kind, rtn } => {
                        rtn.send(self.find_external(&kind).cloned());
                    }
                    DriversCall::FindInternalKind { kind, rtn } => {
                        rtn.send(self.find_internal(&kind).cloned());
                    }
                    DriversCall::Find { kind, rtn } => {
                        rtn.send(self.find(&kind).cloned());
                    }
                    DriversCall::LocalDriverLookup { kind, rtn } => {
                        match self.find(&kind) {
                            None => {
                                rtn.send(None);
                            }
                            Some(driver_api) => {
                                rtn.send(Some(driver_api.get_point()));
                            }
                        };
                    } /*DriversCall::Route(wave) => {
                    println!("DriversApi::Route!");
                    match wave.to().to_single() {
                    Ok(to) => {
                    match self.point_to_driver.get(&to.point ) {
                    Some(api) => {
                    api.route(wave).await;
                    }
                    None => {
                    self.skel.logger.error(format!("Drivers does not have a Driver at surface: {}",to.to_string()));
                    }
                    }
                    }
                    Err(err) => {
                    self.skel.logger.error(format!("expecting single recipient: {}",err.to_string()));
                    }
                    }

                    }

                    */
                    DriversCall::ByPoint { point, rtn } => {
                        let driver = self.point_to_driver.get(&point).cloned();
                        rtn.send(driver);
                    }
                }
            }
        });
    }

    pub fn internal_kinds(&self) -> Vec<ProtoKindSelector> {
        self.kinds.clone()
    }

    pub fn external_kinds(&self) -> Vec<ProtoKindSelector> {
        self.external_kinds.clone()
    }
    pub async fn init0(&mut self) {
        if self.factories.is_empty() {
            self.status_listen(None).await;
        } else {
            let factory = self.factories.remove(0);
            let (status_tx, mut status_rx) = watch::channel(DriverStatus::Pending);
            self.statuses_rx
                .insert(ProtoKindSelector::from_base(BaseKind::Driver), status_rx.clone());

            self.create(factory.kind(), factory.clone(), status_tx)
                .await;

            let (rtn, mut rtn_rx) = oneshot::channel();
            self.status_listen(Some(rtn)).await;
            let call_tx = self.call_tx.clone();
            tokio::spawn(async move {
                match rtn_rx.await {
                    Ok(Ok(_)) => {
                        call_tx.send(DriversCall::Init0).await;
                    }
                    _ => {
                        // should be logged by status
                    }
                }
            });
        }
    }

    pub async fn init1(&mut self) {
        let mut statuses_tx = HashMap::new();
        for kind in &self.kinds {
            let (status_tx, status_rx) = watch::channel(DriverStatus::Pending);
            statuses_tx.insert(kind.clone(), status_tx);
            self.statuses_rx.insert(kind.clone(), status_rx);
        }

        for factory in self.factories.clone() {
            let status_tx = statuses_tx.remove(&factory.kind()).unwrap();
            self.create(factory.kind(), factory, status_tx).await;
        }

        self.status_listen(None).await;
    }

    async fn status_listen(&self, on_complete: Option<oneshot::Sender<Result<(), ()>>>) {
        let logger = self.skel.logger.clone();
        let status_tx = self.status_tx.clone();
        let statuses_rx = self.statuses_rx.clone();
        tokio::spawn(async move {
            loop {
                let mut inits = 0;
                let mut fatals = 0;
                let mut retries = 0;
                let mut readies = 0;

                if statuses_rx.is_empty() {
                    break;
                }

                for multi in statuses_rx.iter() {
                    let kind = multi.key();
                    let status_rx = multi.value();
                    match status_rx.borrow().clone() {
                        DriverStatus::Ready => {
                            readies = readies + 1;
                        }
                        DriverStatus::Retrying(msg) => {
                            logger.warn(format!("DRIVER RETRY: {} {}", kind.to_string(), msg));
                            retries = retries + 1;
                        }
                        DriverStatus::Fatal(msg) => {
                            logger.error(format!("DRIVER FATAL: {} {}", kind.to_string(), msg));
                            fatals = fatals + 1;
                        }
                        DriverStatus::Init => {
                            inits = inits + 1;
                        }
                        _ => {}
                    }
                }

                if readies == statuses_rx.len() {
                    if on_complete.is_some() {
                        on_complete.unwrap().send(Ok(()));
                        break;
                    } else {
                        status_tx.send(DriverStatus::Ready).await;
                    }
                } else if fatals > 0 {
                    status_tx
                        .send(DriverStatus::Fatal(
                            "One or more Drivers have a Fatal condition".to_string(),
                        ))
                        .await;
                    if on_complete.is_some() {
                        on_complete.unwrap().send(Err(()));
                    }
                    break;
                } else if retries > 0 {
                    status_tx
                        .send(DriverStatus::Fatal(
                            "One or more Drivers is Retrying initialization".to_string(),
                        ))
                        .await;
                } else if inits > 0 {
                    status_tx.send(DriverStatus::Init).await;
                } else {
                    status_tx.send(DriverStatus::Unknown).await;
                }

                for mut multi in statuses_rx.iter_mut() {
                    let status_rx = multi.value_mut();
                    let mut rx = vec![];
                    rx.push(status_rx.changed().boxed());
                    let (result, _, _) = select_all(rx).await;
                    if logger.result(result).is_err() {
                        break;
                    }
                }
            }
        });
    }

    async fn create(
        &self,
        kind: ProtoKindSelector,
        factory: Arc<dyn HyperDriverFactory<P>>,
        status_tx: watch::Sender<DriverStatus>,
    ) {
        {
            let skel = self.skel.clone();
            let call_tx = self.call_tx.clone();
            let drivers_point = self.skel.point.push("drivers").unwrap();

            async fn register<P>(
                skel: &HyperStarSkel<P>,
                point: &Point,
                logger: &PointLogger,
            ) -> Result<(), P::Err>
            where
                P: Cosmos,
            {
                let registration = Registration {
                    point: point.clone(),
                    kind: Kind::Driver,
                    registry: Default::default(),
                    properties: Default::default(),
                    owner: HYPERUSER.clone(),
                    strategy: Strategy::Ensure,
                    status: Status::Init,
                };

                skel.registry.register(&registration).await?;
                skel.api.create_states(point.clone()).await?;
                skel.registry.assign_star(&point, &skel.point).await?;
                Ok(())
            }
            let point = drivers_point
                .push(kind.as_point_segments().unwrap())
                .unwrap();
            let logger = self.skel.logger.point(point.clone());
            let status_rx = status_tx.subscribe();

            {
                let logger = logger.point(point.clone());
                let kind = kind.clone();
                let mut status_rx = status_rx.clone();
                tokio::spawn(async move {
                    loop {
                        let status = status_rx.borrow().clone();
                        match status {
                            DriverStatus::Unknown => {
                                //                                logger.info(format!("{} {}", kind.to_string(), status.to_string()));
                            }
                            DriverStatus::Pending => {
                                //                               logger.info(format!("{} {}", kind.to_string(), status.to_string()));
                            }
                            DriverStatus::Init => {
                                //                                logger.info(format!("{} {}", kind.to_string(), status.to_string()));
                            }
                            DriverStatus::Ready => {
                                //                                logger.info(format!("{} {}", kind.to_string(), status.to_string()));
                            }
                            DriverStatus::Retrying(ref reason) => {
                                logger.warn(format!(
                                    "{} {}({})",
                                    kind.to_string(),
                                    status.to_string(),
                                    reason
                                ));
                            }
                            DriverStatus::Fatal(ref reason) => {
                                logger.error(format!(
                                    "{} {}({})",
                                    kind.to_string(),
                                    status.to_string(),
                                    reason
                                ));
                            }
                        }
                        match status_rx.changed().await {
                            Ok(_) => {}
                            Err(_) => {
                                break;
                            }
                        }
                    }
                });
            }

            match logger.result(register(&skel, &point, &logger).await) {
                Ok(_) => {}
                Err(err) => {
                    status_tx.send(DriverStatus::Fatal(
                        "Driver registration failed".to_string(),
                    ));
                    return;
                }
            }

            let mut router = LayerInjectionRouter::new(
                skel.clone(),
                point.clone().to_surface().with_layer(Layer::Core),
            );

            router.direction = Some(TraversalDirection::Fabric);

            let mut transmitter =
                ProtoTransmitterBuilder::new(Arc::new(router), skel.exchanger.clone());
            transmitter.from =
                SetStrategy::Override(point.clone().to_surface().with_layer(Layer::Core));
            let transmitter = transmitter.build();

            let (runner_tx, runner_rx) = mpsc::channel(1024);
            let (request_tx, mut request_rx) = mpsc::channel(1024);
            let driver_skel = DriverSkel::new(
                skel,
                kind.clone(),
                point.clone(),
                transmitter.clone(),
                logger.clone(),
                status_tx,
                request_tx,
            );

            {
                let runner_tx = runner_tx.clone();
                let logger = logger.clone();
                tokio::spawn(async move {
                    while let Some(request) = request_rx.recv().await {
                        logger
                            .result(
                                runner_tx
                                    .send(DriverRunnerCall::DriverRunnerRequest(request))
                                    .await,
                            )
                            .unwrap_or_default();
                    }
                });
            }

            {
                let skel = self.skel.clone();
                let call_tx = call_tx.clone();
                let logger = logger.clone();
                let ctx = DriverCtx::new(transmitter.clone());

                tokio::spawn(async move {
                    let driver =
                        logger.result(factory.create(skel.clone(), driver_skel.clone(), ctx).await);
                    match driver {
                        Ok(driver) => {
                            let layer = driver.layer();
                            let runner = DriverRunner::new(
                                driver_skel.clone(),
                                skel.clone(),
                                driver,
                                runner_tx,
                                runner_rx,
                                status_rx.clone(),
                                layer,
                            );
                            let driver = DriverApi::new(
                                runner.clone(),
                                driver_skel.point.clone(),
                                factory.kind(),
                            );
                            let (rtn, rtn_rx) = oneshot::channel();
                            call_tx
                                .send(DriversCall::AddDriver {
                                    driver: driver.clone(),
                                    rtn,
                                })
                                .await
                                .unwrap_or_default();
                            if driver.kind.matches(&Kind::Driver) {
                                driver.on_added();
                            }
                            rtn_rx.await;
                        }
                        Err(err) => {
                            logger.error(err.to_string());
                            driver_skel
                                .status_tx
                                .send(DriverStatus::Fatal(
                                    "Driver Factory creation error".to_string(),
                                ))
                                .await;
                        }
                    }
                });
            }
        }
    }

    pub fn find(&self, kind: &Kind) -> Option<&DriverApi<P>> {
        for selector in &self.kinds {
            if selector.matches(kind) {
                return self.kind_to_driver.get(&selector);
            }
        }

        for selector in &self.external_kinds {
            if selector.matches(kind) {
                return self.kind_to_driver.get(selector);
            }
        }
        None
    }

    pub fn find_external(&self, kind: &Kind) -> Option<&DriverApi<P>> {
        for selector in &self.external_kinds {
            if selector.matches(kind) {
                return self.kind_to_driver.get(selector);
            }
        }
        None
    }

    pub fn find_internal(&self, kind: &Kind) -> Option<&DriverApi<P>> {
        for selector in &self.kinds {
            if selector.matches(kind) {
                return self.kind_to_driver.get(selector);
            }
        }
        None
    }
}

impl<P> Drivers<P>
where
    P: Cosmos,
{
    /*
    pub async fn assign(&self, assign: Assign) -> Result<(), P::Err> {
        let driver = self.find(&assign.details.stub.kind).ok_or::<UniErr>(
            format!(
                "Kind {} not supported by Drivers for Star: {}",
                assign.details.stub.kind.to_string(),
                self.skel.kind.to_string()
            )
            .into(),
        )?;
        driver.assign(assign).await
    }

     */

    /*
    pub async fn route(&self, wave: UltraWave) -> Result<(),P::Err>{
        let record = self
            .skel
            .registry
            .record(&wave.to().single_or()?.point)
            .await
            .map_err(|e| e.to_uni_err())?;
        let driver = self
            .find(&record.details.stub.kind)
            .ok_or::<UniErr>("do not handle this kind of driver".into())?;
        driver.route(wave).await;
        Ok(())
    }

     */

    /*
    pub async fn sys(&self, ctx: InCtx<'_, Sys>) -> Result<ReflectedCore, MsgErr> {
        if let Sys::Assign(assign) = &ctx.input {
            match self.drivers.get(&assign.details.stub.kind) {
                None => Err(format!(
                    "do not have driver for Kind: <{}>",
                    assign.details.stub.kind.to_string()
                )
                .into()),
                Some(driver) => {
                    let ctx = ctx.push_input_ref( assign );
                    let state = tokio::time::timeout(
                        Duration::from_secs(self.skel.machine.timeouts.high),
                        driver.assign(ctx).await,
                    )
                    .await??;
                   Ok(ctx.wave().core.ok())
                }
            }
        } else {
            Err(MsgErr::bad_request())
        }
    }

     */

    async fn start_outer_traversal(&self, traversal: Traversal<UltraWave>) {
        let traverse_to_next_tx = self.skel.traverse_to_next_tx.clone();
        tokio::spawn(async move {
            traverse_to_next_tx.send(traversal).await;
        });
    }

    async fn start_inner_traversal(&self, traversal: Traversal<UltraWave>) {}

    pub async fn visit(&self, traversal: Traversal<UltraWave>) {
        if traversal.dir.is_core() {
            match self.find(&traversal.record.details.stub.kind) {
                None => {
                    traversal.logger.warn(format!(
                        "star does not have a driver for Kind <{}>",
                        traversal.record.details.stub.kind.to_template().to_string()
                    ));
                }
                Some(driver) => {
                    let driver = driver.clone();
                    tokio::spawn(async move {
                        driver.traverse(traversal).await;
                    });
                }
            }
        } else {
            self.start_outer_traversal(traversal).await;
        }
    }
}

#[derive(Clone)]
pub struct DriverApi<P>
where
    P: Cosmos,
{
    pub call_tx: mpsc::Sender<DriverRunnerCall<P>>,
    pub kind: ProtoKindSelector,
    pub point: Point,
}

impl<P> DriverApi<P>
where
    P: Cosmos,
{
    pub fn new(tx: mpsc::Sender<DriverRunnerCall<P>>, point: Point, kind: ProtoKindSelector) -> Self {
        Self {
            call_tx: tx,
            point,
            kind,
        }
    }

    pub fn get_point(&self) -> Point {
        self.point.clone()
    }

    /// This method call will only work for DriverDriver
    pub async fn add_driver(&self, api: DriverApi<P>) {
        self.call_tx.send(DriverRunnerCall::AddDriver(api)).await;
    }

    pub async fn init_item(&self, point: Point) -> Result<Status, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx
            .try_send(DriverRunnerCall::InitItem { point, rtn });
        rtn_rx.await?
    }

    pub fn on_added(&self) {
        self.call_tx.try_send(DriverRunnerCall::OnAdded);
    }

    pub async fn bind(&self, point: &Point) -> Result<ArtRef<BindConfig>, P::Err> {
        let (rtn, rtn_rx) = oneshot::channel();
        self.call_tx
            .send(DriverRunnerCall::Bind {
                point: point.clone(),
                rtn,
            })
            .await;
        rtn_rx.await?
    }

    pub async fn driver_bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        let (rtn, rtn_rx) = oneshot::channel();
        self.call_tx.send(DriverRunnerCall::DriverBind(rtn)).await;
        Ok(rtn_rx.await?)
    }

    pub async fn traverse(&self, traversal: Traversal<UltraWave>) {
        self.call_tx
            .send(DriverRunnerCall::Traverse(traversal))
            .await;
    }

    pub async fn handle(&self, traversal: Traversal<UltraWave>) {
        self.call_tx.send(DriverRunnerCall::Handle(traversal)).await;
    }

    /*
    pub async fn route(&self, wave: UltraWave)  {
        self.call_tx
            .send(DriverRunnerCall::Route(wave) )
            .await;
    }

    pub fn route_sync(&self, wave: UltraWave)  {
        self.call_tx
            .try_send(DriverRunnerCall::Route(wave) );
    }

     */
}

#[derive(strum_macros::Display)]
pub enum DriverRunnerCall<P>
where
    P: Cosmos,
{
    AddDriver(DriverApi<P>),
    GetPoint(oneshot::Sender<Point>),
    Traverse(Traversal<UltraWave>),
    Handle(Traversal<UltraWave>),
    Item {
        point: Point,
        tx: oneshot::Sender<Result<ItemSphere<P>, P::Err>>,
    },

    OnAdded,
    InitItem {
        point: Point,
        rtn: oneshot::Sender<Result<Status, SpaceErr>>,
    },
    DriverRunnerRequest(DriverRunnerRequest<P>),
    DriverBind(oneshot::Sender<ArtRef<BindConfig>>),
    Bind {
        point: Point,
        rtn: oneshot::Sender<Result<ArtRef<BindConfig>, P::Err>>,
    },
}

pub enum DriverRunnerRequest<P>
where
    P: Cosmos,
{
    Create {
        agent: Agent,
        create: Create,
        rtn: oneshot::Sender<Result<Stub, P::Err>>,
    },
}

pub struct ItemOuter<P>
where
    P: Cosmos + 'static,
{
    pub surface: Surface,
    pub skel: HyperStarSkel<P>,
    pub item: ItemSphere<P>,
    pub router: Arc<dyn Router>,
}

impl<P> ItemOuter<P>
where
    P: Cosmos + 'static,
{
    pub async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        self.item.bind().await
    }
}

#[async_trait]
impl<P> TraversalLayer for ItemOuter<P>
where
    P: Cosmos,
{
    fn surface(&self) -> cosmic_space::point::Surface {
        self.surface.clone()
    }

    async fn deliver_directed(&self, direct: Traversal<DirectedWave>) -> Result<(), SpaceErr> {
        self.skel
            .logger
            .track(&direct, || Tracker::new("core:outer", "DeliverDirected"));
        let logger = self
            .skel
            .logger
            .point(self.surface().clone().to_point())
            .span();

        match &self.item {
            ItemSphere::Handler(item) => {
                if direct.core().method == Method::Cmd(CmdMethod::Init) {
                    let reflection = direct.reflection()?;
                    match item.init().await {
                        Ok(status) => {
                            self.skel
                                .registry
                                .set_status(&self.surface().point.clone(), &status)
                                .await;
                            let reflect = reflection.make(ReflectedCore::ok(), self.surface());
                            self.router.route(reflect.to_ultra()).await;
                        }
                        Err(err) => {
                            self.skel
                                .registry
                                .set_status(&self.surface().point.clone(), &Status::Panic)
                                .await;
                            let reflect = reflection.make(ReflectedCore::err(err), self.surface());
                            self.router.route(reflect.to_ultra()).await;
                        }
                    }
                } else {
                    let mut transmitter = ProtoTransmitterBuilder::new(
                        self.router.clone(),
                        self.skel.exchanger.clone(),
                    );
                    transmitter.from = SetStrategy::Override(self.surface.clone());
                    let transmitter = transmitter.build();
                    let to = direct.to.clone();
                    let reflection = direct.reflection();
                    let ctx = RootInCtx::new(direct.payload, to, logger, transmitter);

                    match item.handle(ctx).await {
                        CoreBounce::Absorbed => {}
                        CoreBounce::Reflected(reflected) => {
                            let reflection = reflection?;

                            let wave = reflection.make(reflected, self.surface.clone());
                            let wave = wave.to_ultra();
                            #[cfg(test)]
                            self.skel
                                .diagnostic_interceptors
                                .reflected_endpoint
                                .send(wave.clone());
                            self.inject(wave).await;
                        }
                    }
                }
            }
            ItemSphere::Router(router) => {
                self.skel
                    .logger
                    .result(router.traverse(direct.wrap()).await)?;
            }
        }

        Ok(())
    }

    async fn deliver_reflected(&self, reflect: Traversal<ReflectedWave>) -> Result<(), SpaceErr> {
        if reflect.to().layer == self.surface.layer {
            self.exchanger().reflected(reflect.payload).await
        } else {
            match &self.item {
                ItemSphere::Router(router) => {
                    router.traverse(reflect.wrap() ).await;
                    Ok(())
                }
                ItemSphere::Handler(_) => {
                    Err("cannot deliver a reflected to an ItemHandler::Handler, it must be an ItemHandler::Router".into())
                }
            }
        }
    }

    async fn traverse_next(&self, traversal: Traversal<UltraWave>) {
        self.skel.traverse_to_next_tx.send(traversal).await;
    }

    async fn inject(&self, wave: UltraWave) {
        let inject = TraversalInjection::new(self.surface().clone().with_layer(Layer::Guest), wave);
        self.skel.inject_tx.send(inject).await;
    }

    fn exchanger(&self) -> &Exchanger {
        &self.skel.exchanger
    }
}

#[derive(DirectedHandler)]
pub struct DriverRunner<P>
where
    P: Cosmos + 'static,
{
    skel: DriverSkel<P>,
    star_skel: HyperStarSkel<P>,
    call_tx: mpsc::Sender<DriverRunnerCall<P>>,
    call_rx: mpsc::Receiver<DriverRunnerCall<P>>,
    driver: Box<dyn Driver<P>>,
    router: LayerInjectionRouter,
    logger: PointLogger,
    status_rx: watch::Receiver<DriverStatus>,
    layer: Layer,
}

#[handler]
impl<P> DriverRunner<P>
where
    P: Cosmos + 'static,
{
    pub fn new(
        skel: DriverSkel<P>,
        star_skel: HyperStarSkel<P>,
        driver: Box<dyn Driver<P>>,
        call_tx: mpsc::Sender<DriverRunnerCall<P>>,
        call_rx: mpsc::Receiver<DriverRunnerCall<P>>,
        status_rx: watch::Receiver<DriverStatus>,
        layer: Layer,
    ) -> mpsc::Sender<DriverRunnerCall<P>> {
        let logger = star_skel.logger.point(skel.point.clone());
        let router = LayerInjectionRouter::new(
            star_skel.clone(),
            skel.point.clone().to_surface().with_layer(Layer::Guest),
        );

        let driver = Self {
            skel,
            star_skel: star_skel,
            call_tx: call_tx.clone(),
            call_rx: call_rx,
            driver,
            router,
            logger,
            status_rx,
            layer,
        };

        driver.start();

        call_tx
    }

    fn start(mut self) {
        tokio::spawn(async move {
            while let Some(call) = self.call_rx.recv().await {
                match call {
                    DriverRunnerCall::OnAdded => {
                        let mut router = LayerInjectionRouter::new(
                            self.star_skel.clone(),
                            self.skel.point.clone().to_surface().with_layer(Layer::Core),
                        );
                        router.direction = Some(TraversalDirection::Fabric);

                        let mut transmitter = ProtoTransmitter::new(
                            Arc::new(router),
                            self.star_skel.exchanger.clone(),
                        );
                        let ctx = DriverCtx::new(transmitter);
                        match self
                            .skel
                            .logger
                            .result(self.driver.init(self.skel.clone(), ctx).await)
                        {
                            Ok(_) => {}
                            Err(err) => {
                                self.skel
                                    .status_tx
                                    .send(DriverStatus::Fatal(err.to_string()))
                                    .await;
                            }
                        }
                    }
                    DriverRunnerCall::Traverse(traversal) => {
                        self.traverse(traversal).await;
                    }
                    DriverRunnerCall::Handle(traversal) => {
                        if traversal.is_directed() {
                            let wave = traversal.payload.to_directed().unwrap();
                            self.logger
                                .track(&wave, || Tracker::new("driver:shell", "Route"));
                            let reflection = wave.reflection();
                            let port = wave.to().clone().unwrap_single();
                            let logger =
                                self.star_skel.logger.point(port.clone().to_point()).span();
                            let router = Arc::new(self.router.clone());
                            let transmitter =
                                ProtoTransmitter::new(router, self.star_skel.exchanger.clone());
                            let ctx =
                                RootInCtx::new(wave, port.clone(), logger, transmitter.clone());
                            let handler = self.handler().await;
                            let skel = self.skel.clone();
                            tokio::spawn(async move {
                                match handler.handle(ctx).await {
                                    CoreBounce::Absorbed => {
                                        // do nothing
                                    }
                                    CoreBounce::Reflected(core) => {
                                        let reflection = reflection.unwrap();
                                        let reflect = reflection.make(
                                            core,
                                            skel.point.to_surface().with_layer(Layer::Core),
                                        );
                                        transmitter.route(reflect.to_ultra()).await;
                                    }
                                }
                            });
                        } else {
                            let wave = traversal.payload.to_reflected().unwrap();
                            self.star_skel.exchanger.reflected(wave).await;
                        }
                    }
                    DriverRunnerCall::Item { point, tx } => {
                        tx.send(self.driver.item(&point).await);
                    }
                    DriverRunnerCall::DriverRunnerRequest(request) => match request {
                        DriverRunnerRequest::Create { .. } => {}
                    },
                    DriverRunnerCall::Bind { point, rtn } => {
                        let item = self.driver.item(&point).await;
                        match item {
                            Ok(item) => {
                                tokio::spawn(async move {
                                    rtn.send(item.bind().await);
                                });
                            }
                            Err(err) => {
                                rtn.send(Err(err));
                            }
                        }
                    }
                    DriverRunnerCall::InitItem { point, rtn } => {
                        let item = self.driver.item(&point).await;
                        match item {
                            Ok(item) => {
                                rtn.send(item.init().await);
                            }
                            Err(err) => {
                                rtn.send(Err(err.to_space_err()));
                            }
                        }
                    }
                    DriverRunnerCall::GetPoint(rtn) => {
                        rtn.send(self.skel.point.clone());
                    }
                    DriverRunnerCall::DriverBind(rtn) => {
                        rtn.send(self.driver.bind());
                    }
                    DriverRunnerCall::AddDriver(api) => {
                        self.driver.add_driver(api.clone()).await;
                        api.on_added();
                    }
                }
            }
        });
    }

    async fn traverse(&self, traversal: Traversal<UltraWave>) -> Result<(), P::Err> {
        self.skel.logger.track(&traversal, || {
            Tracker::new(
                format!("drivers -> {}", traversal.dir.to_string()),
                "Traverse",
            )
        });

        let item = self
            .skel
            .logger
            .result(self.item(&traversal.to.point).await)?;
        let logger = item.skel.logger.clone();
        tokio::spawn(async move {
            if traversal.is_directed() {
                logger
                    .result(item.deliver_directed(traversal.unwrap_directed()).await)
                    .unwrap_or_default();
            } else {
                logger
                    .result(item.deliver_reflected(traversal.unwrap_reflected()).await)
                    .unwrap_or_default();
            }
        });
        Ok(())
    }

    async fn item(&self, point: &Point) -> Result<ItemOuter<P>, P::Err> {
        let port = point.clone().to_surface().with_layer(self.layer.clone());

        Ok(ItemOuter {
            surface: port.clone(),
            skel: self.star_skel.clone(),
            item: self.skel.logger.result(self.driver.item(point).await)?,
            router: Arc::new(self.router.clone().with(port)),
        })
    }

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        self.driver.handler().await
    }
}

#[derive(Clone)]
pub struct DriverCtx {
    pub transmitter: ProtoTransmitter,
}

impl DriverCtx {
    pub fn new(transmitter: ProtoTransmitter) -> Self {
        Self { transmitter }
    }
}

#[derive(Clone)]
pub struct DriverSkel<P>
where
    P: Cosmos,
{
    skel: HyperStarSkel<P>,
    pub kind: ProtoKindSelector,
    pub point: Point,
    pub logger: PointLogger,
    pub status_rx: watch::Receiver<DriverStatus>,
    pub status_tx: mpsc::Sender<DriverStatus>,
    pub request_tx: mpsc::Sender<DriverRunnerRequest<P>>,
    pub phantom: PhantomData<P>,
}

impl<P> DriverSkel<P>
where
    P: Cosmos,
{
    pub fn data_dir(&self) -> String {
        self.skel.data_dir()
    }

    pub fn status(&self) -> DriverStatus {
        self.status_rx.borrow().clone()
    }

    pub fn drivers(&self) -> &DriversApi<P> {
        &self.skel.drivers
    }

    pub fn artifacts(&self) -> &ArtifactApi {
        &self.skel.machine.artifacts
    }

    pub fn registry(&self) -> &Registry<P> {
        &self.skel.machine.registry
    }

    pub fn new(
        skel: HyperStarSkel<P>,
        kind: ProtoKindSelector,
        point: Point,
        transmitter: ProtoTransmitter,
        logger: PointLogger,
        status_tx: watch::Sender<DriverStatus>,
        request_tx: mpsc::Sender<DriverRunnerRequest<P>>,
    ) -> Self {
        let (mpsc_status_tx, mut mpsc_status_rx): (
            tokio::sync::mpsc::Sender<DriverStatus>,
            tokio::sync::mpsc::Receiver<DriverStatus>,
        ) = mpsc::channel(128);

        let watch_status_rx = status_tx.subscribe();
        tokio::spawn(async move {
            while let Some(status) = mpsc_status_rx.recv().await {
                status_tx.send(status.clone());
                if let DriverStatus::Fatal(_) = status {
                    break;
                }
            }
        });

        Self {
            skel,
            kind,
            point,
            logger,
            status_tx: mpsc_status_tx,
            status_rx: watch_status_rx,
            phantom: Default::default(),
            request_tx,
        }
    }

    pub async fn create_in_driver(
        &self,
        child_segment_template: PointSegTemplate,
        kind: KindTemplate,
    ) -> Result<Details, P::Err> {
        let create = Create {
            template: Template::new(
                PointTemplate {
                    parent: self.point.clone(),
                    child_segment_template,
                },
                kind,
            ),
            properties: Default::default(),
            strategy: Strategy::Ensure,
            state: StateSrc::None,
        };
        self.skel
            .logger
            .result(self.skel.create_in_star(create).await)
    }

    pub async fn locate(&self, point: &Point) -> Result<ParticleRecord, P::Err> {
        self.skel.registry.record(point).await
    }

    pub async fn local_driver_lookup(&self, kind: Kind) -> Result<Option<Point>, SpaceErr> {
        self.skel.drivers.local_driver_lookup(kind).await
    }

    pub fn item_ctx(&self, point: &Point, layer: Layer) -> Result<ItemCtx, P::Err> {
        let mut router = LayerInjectionRouter::new(
            self.skel.clone(),
            point.to_surface().with_layer(Layer::Core),
        );
        router.direction = Some(TraversalDirection::Fabric);
        let mut transmitter =
            ProtoTransmitterBuilder::new(Arc::new(router), self.skel.exchanger.clone());
        transmitter.from = SetStrategy::Fill(point.to_surface().with_layer(layer));
        transmitter.agent = SetStrategy::Fill(Agent::Point(point.clone()));
        let transmitter = transmitter.build();
        let ctx = ItemCtx { transmitter };
        Ok(ctx)
    }
}

pub struct DriverFactoryWrapper<P>
where
    P: Cosmos,
{
    pub factory: Box<dyn DriverFactory<P>>,
}

impl<P> DriverFactoryWrapper<P>
where
    P: Cosmos,
{
    pub fn wrap(factory: Box<dyn DriverFactory<P>>) -> Arc<dyn HyperDriverFactory<P>> {
        Arc::new(Self { factory })
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for DriverFactoryWrapper<P>
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        self.factory.kind()
    }

    async fn create(
        &self,
        star_skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        self.factory.create(driver_skel, ctx).await
    }
}

#[async_trait]
pub trait DriverFactory<P>: Send + Sync
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector;

    fn avail(&self) -> DriverAvail {
        DriverAvail::External
    }

    async fn create(
        &self,
        skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err>;

    fn properties(&self) -> SetProperties {
        SetProperties::default()
    }
}

#[async_trait]
pub trait HyperDriverFactory<P>: Send + Sync
where
    P: Cosmos,
{
    fn kind(&self) -> KindSelector;

    fn avail(&self) -> DriverAvail {
        DriverAvail::External
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err>;

    fn properties(&self) -> SetProperties {
        SetProperties::default()
    }
}

#[derive(Clone)]
pub struct HyperSkel<P>
where
    P: Cosmos,
{
    pub star: HyperStarSkel<P>,
    pub driver: DriverSkel<P>,
}

impl<P> HyperSkel<P>
where
    P: Cosmos,
{
    pub fn new(star: HyperStarSkel<P>, driver: DriverSkel<P>) -> Self {
        Self { star, driver }
    }
}

#[async_trait]
pub trait Driver<P>: Send + Sync
where
    P: Cosmos,
{
    fn kind(&self) -> Kind;

    fn layer(&self) -> Layer {
        Layer::Core
    }

    fn avail(&self) -> DriverAvail {
        DriverAvail::External
    }

    fn bind(&self) -> ArtRef<BindConfig> {
        DRIVER_BIND.clone()
    }

    async fn init(&mut self, skel: DriverSkel<P>, ctx: DriverCtx) -> Result<(), P::Err> {
        skel.logger
            .result(skel.status_tx.send(DriverStatus::Ready).await)
            .unwrap_or_default();
        Ok(())
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err>;

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        Box::new(DefaultDriverHandler::restore())
    }

    /// This is sorta a hack, it only works for DriverDriver
    async fn add_driver(&self, _driver: DriverApi<P>) {}
}

#[async_trait]
pub trait DriverHandler<P>: DirectedHandler
where
    P: Cosmos,
{
}

pub struct DefaultDriverHandler {}

impl DefaultDriverHandler {
    fn restore() -> Self {
        DefaultDriverHandler {}
    }
}

impl<P> DriverHandler<P> for DefaultDriverHandler where P: Cosmos {}
#[handler]
impl DefaultDriverHandler {
    #[route("Hyp<Assign>")]
    pub async fn assign(&self, _ctx: InCtx<'_, HyperSubstance>) -> Result<(), SpaceErr> {
        Ok(())
    }
}

pub trait States: Sync + Sync
where
    Self::ItemState: ItemState,
{
    type ItemState;
    fn new() -> Self;

    fn create(assign: Assign) -> Arc<RwLock<Self::ItemState>>;
    fn get(point: &Point) -> Option<&Arc<RwLock<Self::ItemState>>>;
    fn remove(point: &Point) -> Option<Arc<RwLock<Self::ItemState>>>;
}

#[derive(Clone, Eq, PartialEq, Hash, strum_macros::Display)]
pub enum DriverStatus {
    Unknown,
    Pending,
    Init,
    Ready,
    Retrying(String),
    Fatal(String),
}

impl<E> From<Result<DriverStatus, E>> for DriverStatus
where
    E: ToString,
{
    fn from(result: Result<DriverStatus, E>) -> Self {
        match result {
            Ok(status) => status,
            Err(e) => DriverStatus::Fatal(e.to_string()),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DriverStatusEvent {
    pub driver: Point,
    pub status: DriverStatus,
}

pub trait ItemState: Send + Sync {}

pub enum ItemSphere<P>
where
    P: Cosmos,
{
    Handler(Box<dyn ItemHandler<P>>),
    Router(Box<dyn ItemRouter<P>>),
}

impl<P> ItemSphere<P>
where
    P: Cosmos,
{
    pub async fn init(&self) -> Result<Status, SpaceErr> {
        match self {
            ItemSphere::Handler(handler) => handler.init().await,
            ItemSphere::Router(router) => {
                // needs to convert to a message and forward to router
                Ok(Status::Ready)
            }
        }
    }

    pub async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        match self {
            ItemSphere::Handler(handler) => handler.bind().await,
            ItemSphere::Router(router) => router.bind().await,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum DriverAvail {
    Internal,
    External,
}

#[async_trait]
pub trait Item<P>
where
    P: Cosmos,
{
    type Skel;
    type Ctx;
    type State;

    fn restore(skel: Self::Skel, ctx: Self::Ctx, state: Self::State) -> Self;

    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(DEFAULT_BIND.clone())
    }
}

#[async_trait]
pub trait ItemHandler<P>: DirectedHandler + Send + Sync
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err>;
    async fn init(&self) -> Result<Status, SpaceErr> {
        Ok(Status::Ready)
    }
}

#[async_trait]
pub trait ItemRouter<P>: TraversalRouter + Send + Sync
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err>;
}

#[derive(Clone)]
pub struct HyperItemSkel<P>
where
    P: Cosmos,
{
    pub skel: DriverSkel<P>,
    pub point: Point,
    pub kind: Kind,
}

#[derive(Clone)]
pub struct ItemSkel<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
    pub point: Point,
    pub kind: Kind,
}

impl<P> ItemSkel<P>
where
    P: Cosmos,
{
    pub fn new(point: Point, kind: Kind, skel: DriverSkel<P>) -> Self {
        Self { point, kind, skel }
    }

    pub fn data_dir(&self) -> String {
        self.skel.data_dir()
    }
}

pub struct ItemCtx {
    pub transmitter: ProtoTransmitter,
}

pub struct DriverDriverFactory {}

impl DriverDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for DriverDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        ProtoKindSelector::from_base(BaseKind::Driver)
    }

    fn avail(&self) -> DriverAvail {
        DriverAvail::Internal
    }

    async fn create(
        &self,
        star: HyperStarSkel<P>,
        driver: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(DriverDriver::new(driver).await?))
    }
}

#[derive(Clone)]
pub struct DriverDriverItemSkel<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
    pub api: DriverApi<P>,
}

impl<P> DriverDriverItemSkel<P>
where
    P: Cosmos,
{
    pub fn new(skel: DriverSkel<P>, api: DriverApi<P>) -> Self {
        Self { skel, api }
    }
}

impl<P> Deref for DriverDriverItemSkel<P>
where
    P: Cosmos,
{
    type Target = DriverSkel<P>;

    fn deref(&self) -> &Self::Target {
        &self.skel
    }
}

pub struct DriverDriver<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
    map: DashMap<Point, DriverApi<P>>,
}

impl<P> DriverDriver<P>
where
    P: Cosmos,
{
    async fn new(skel: DriverSkel<P>) -> Result<Self, P::Err> {
        let map = DashMap::new();
        Ok(Self { skel, map })
    }
}

impl<P> DriverDriver<P>
where
    P: Cosmos,
{
    pub fn get_driver(&self, point: &Point) -> Option<DriverApi<P>> {
        let rtn = self.map.get(point);
        match rtn {
            None => None,
            Some(driver) => Some(driver.value().clone()),
        }
    }
}

#[async_trait]
impl<P> Driver<P> for DriverDriver<P>
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Driver
    }

    async fn add_driver(&self, api: DriverApi<P>) {
        let point = api.get_point();
        self.map.insert(point, api);
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        let api = self.get_driver(point).ok_or(format!(
            "DriverApi is not associated with point: {} ",
            point.to_string()
        ))?;
        let skel = DriverDriverItemSkel::new(self.skel.clone(), api);
        let rtn = Ok(ItemSphere::Router(Box::new(DriverItem::restore(
            skel,
            (),
            (),
        ))));

        rtn
    }
}

#[derive(DirectedHandler)]
pub struct DriverItem<P>
where
    P: Cosmos,
{
    skel: DriverDriverItemSkel<P>,
}

impl<P> Item<P> for DriverItem<P>
where
    P: Cosmos,
{
    type Skel = DriverDriverItemSkel<P>;
    type Ctx = ();
    type State = ();

    fn restore(skel: Self::Skel, ctx: Self::Ctx, state: Self::State) -> Self {
        Self { skel }
    }
}

#[async_trait]
impl<P> TraversalRouter for DriverItem<P>
where
    P: Cosmos,
{
    async fn traverse(&self, traversal: Traversal<UltraWave>) -> Result<(), SpaceErr> {
        self.skel.api.handle(traversal).await;
        Ok(())
    }
}

#[async_trait]
impl<P> ItemRouter<P> for DriverItem<P>
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        self.skel.api.driver_bind().await
    }
}
