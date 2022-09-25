use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Add, Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use dashmap::mapref::one::{Ref, RefMut};
use dashmap::DashMap;
use futures::future::{BoxFuture, join_all};
use futures::FutureExt;
use serde::{Deserialize, Serialize};
use tokio::sync::mpsc::error::SendError;
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::{broadcast, mpsc, Mutex, oneshot, RwLock, watch};
use tokio::time::error::Elapsed;
use tracing::{error, info};

use cosmic_hyperlane::{
    Bridge, HyperClient, HyperRouter, Hyperway, HyperwayEndpoint, HyperwayEndpointFactory,
    HyperwayInterchange, HyperwayStub,
};
use cosmic_universe::artifact::ArtRef;
use cosmic_universe::command::common::StateSrc;
use cosmic_universe::command::direct::create::{Create, Strategy};
use cosmic_universe::command::direct::set::Set;
use cosmic_universe::command::RawCommand;
use cosmic_universe::config::bind::{BindConfig, RouteSelector};
use cosmic_universe::err::UniErr;
use cosmic_universe::hyper::{MountKind, ParticleLocation};
use cosmic_universe::hyper::{
    Assign, AssignmentKind, Discoveries, Discovery, HyperSubstance, Location, ParticleRecord,
    Provision, Search,
};
use cosmic_universe::kind::{BaseKind, Kind, StarStub, StarSub, Sub};
use cosmic_universe::loc::{
    GLOBAL_EXEC, Layer, LOCAL_STAR, Point, RouteSeg, StarKey, Surface, SurfaceSelector, ToBaseKind,
    Topic, ToPoint, ToSurface, Uuid,
};
use cosmic_universe::log::{PointLogger, RootLogger, Trackable, Tracker};
use cosmic_universe::parse::{bind_config, Env, route_attribute};
use cosmic_universe::particle::traversal::{
    Traversal, TraversalDirection, TraversalInjection, TraversalLayer,
};
use cosmic_universe::particle::{Details, Status, Stub};
use cosmic_universe::settings::Timeouts;
use cosmic_universe::substance::Bin;
use cosmic_universe::substance::{Substance, ToSubstance};
use cosmic_universe::util::{log, ValueMatcher, ValuePattern};
use cosmic_universe::wave::core::cmd::CmdMethod;
use cosmic_universe::wave::core::hyp::HypMethod;
use cosmic_universe::wave::core::{CoreBounce, DirectedCore, Method, ReflectedCore};
use cosmic_universe::wave::exchange::{
    DirectedHandler, DirectedHandlerSelector, DirectedHandlerShell, Exchanger, InCtx,
    ProtoTransmitter, ProtoTransmitterBuilder, RootInCtx, SetStrategy, TxRouter,
};
use cosmic_universe::wave::{
    Agent, Bounce, BounceBacks, DirectedKind, DirectedProto, DirectedWave, Echo, Echoes, Handling,
    HandlingKind, Ping, Pong, Priority, Recipients, RecipientSelector, Reflectable, ReflectedWave,
    Retries, Ripple, Scope, Signal, SingularRipple, ToRecipients, WaitTime, Wave, WaveKind,
};
use cosmic_universe::wave::{HyperWave, UltraWave};
use cosmic_universe::HYPERUSER;
use cosmic_universe::wave::exchange::asynch::Router;

use crate::driver::star::{StarDiscovery, StarPair, StarWrangles, Wrangler};
use crate::driver::{
    Driver, DriverAvail, DriverCtx, DriverDriver, DriverDriverFactory, DriverFactory, Drivers,
    DriversApi, DriversCall, DriverSkel, DriverStatus, HyperDriverFactory, Item, ItemHandler,
    ItemSkel, ItemSphere,
};
use crate::global::{GlobalCommandExecutionHandler, GlobalExecutionChamber};
use crate::layer::field::Field;
use crate::layer::shell::Shell;
use crate::layer::shell::ShellState;
use crate::machine::MachineSkel;
use crate::Registration;
use crate::{Cosmos, DriversBuilder, HyperErr, Registry, RegistryApi};

#[derive(Clone)]
pub struct ParticleStates<P>
where
    P: Cosmos + 'static,
{
    phantom: PhantomData<P>,
    topic: Arc<DashMap<Surface, Arc<dyn TopicHandler>>>,
    shell: Arc<DashMap<Point, ShellState>>,
}

impl<P> ParticleStates<P>
where
    P: Cosmos + 'static,
{
    pub fn create_shell(&self, point: Point) {
        self.shell.insert(point.clone(), ShellState::new(point));
    }

    pub fn new() -> Self {
        Self {
            topic: Arc::new(DashMap::new()),
            shell: Arc::new(DashMap::new()),
            phantom: PhantomData::default(),
        }
    }

    pub fn topic_handler(&self, port: Surface, handler: Arc<dyn TopicHandler>) {
        self.topic.insert(port, handler);
    }

    pub fn find_topic(
        &self,
        port: &Surface,
        source: &Surface,
    ) -> Option<Result<Arc<dyn TopicHandler>, UniErr>> {
        match self.topic.get(port) {
            None => None,
            Some(topic) => {
                let topic = topic.value().clone();
                if topic.source_selector().is_match(source).is_ok() {
                    Some(Ok(topic))
                } else {
                    Some(Err(UniErr::forbidden()))
                }
            }
        }
    }

    pub fn find_shell(&self, point: &Point) -> Result<ShellState, UniErr> {
        Ok(self
            .shell
            .get(point)
            .ok_or(format!(
                "expected shell state for point: {}",
                point.to_string()
            ))?
            .value()
            .clone())
    }
}

#[derive(Clone)]
pub struct HyperStarSkel<P>
where
    P: Cosmos + 'static,
{
    pub api: HyperStarApi<P>,
    pub key: StarKey,
    pub point: Point,
    pub kind: StarSub,
    pub logger: PointLogger,
    pub registry: Registry<P>,
    pub golden_path: Arc<DashMap<StarKey, StarKey>>,
    pub traverse_to_next_tx: mpsc::Sender<Traversal<UltraWave>>,
    pub inject_tx: mpsc::Sender<TraversalInjection>,
    pub machine: MachineSkel<P>,
    pub exchanger: Exchanger,
    pub state: ParticleStates<P>,
    pub adjacents: HashMap<Point, StarStub>,
    pub wrangles: StarWrangles,
    pub gravity_tx: mpsc::Sender<UltraWave>,
    pub gravity_router: TxRouter,
    pub gravity_transmitter: ProtoTransmitter,
    pub drivers: DriversApi<P>,
    pub drivers_traversal_tx: mpsc::Sender<Traversal<UltraWave>>,
    pub status_tx: mpsc::Sender<Status>,
    pub status_rx: watch::Receiver<Status>,
    pub template: StarTemplate,
    pub star_transmitter: ProtoTransmitter,

    #[cfg(test)]
    pub diagnostic_interceptors: DiagnosticInterceptors<P>,
}

impl<P> HyperStarSkel<P>
where
    P: Cosmos,
{
    pub async fn new(
        template: StarTemplate,
        machine: MachineSkel<P>,
        star_tx: &mut HyperStarTx<P>,
    ) -> Self {
        let point = template.key.clone().to_point();
        let logger = machine.logger.point(point.clone());
        let exchanger = Exchanger::new(point.clone().to_surface(), machine.timeouts.clone());
        let state = ParticleStates::new();
        let api = HyperStarApi::new(
            template.kind.clone(),
            star_tx.call_tx.clone(),
            star_tx.status_rx.clone(),
        );

        let mut adjacents = HashMap::new();
        let mut golden_path = Arc::new(DashMap::new());
        // prime the searcher by mapping the immediate lanes
        for hyperway in template.connections.clone() {
            adjacents.insert(hyperway.key().clone().to_point(), hyperway.stub().clone());
            golden_path.insert(hyperway.key().clone(), hyperway.key().clone());
        }

        let gravity_router = TxRouter::new(star_tx.gravity_tx.clone());
        let mut gravity_transmitter =
            ProtoTransmitterBuilder::new(Arc::new(gravity_router.clone()), exchanger.clone());
        gravity_transmitter.from = SetStrategy::Override(point.clone().to_surface());
        gravity_transmitter.handling = SetStrategy::Fill(Handling {
            kind: HandlingKind::Immediate,
            priority: Priority::High,
            retries: Retries::None,
            wait: WaitTime::Low,
        });
        gravity_transmitter.agent = SetStrategy::Fill(Agent::HyperUser);
        gravity_transmitter.scope = SetStrategy::Fill(Scope::Full);

        let gravity_transmitter = gravity_transmitter.build();

        let drivers = DriversApi::new(
            star_tx.drivers_call_tx.clone(),
            star_tx.drivers_status_rx.clone(),
        );

        let star_router = LayerInjectionRouter::injector(
            star_tx.inject_tx.clone(),
            point.to_surface().with_layer(Layer::Core),
        );
        let mut star_transmitter =
            ProtoTransmitterBuilder::new(Arc::new(star_router), exchanger.clone());
        star_transmitter.from = SetStrategy::Override(point.to_surface().with_layer(Layer::Core));
        star_transmitter.agent = SetStrategy::Override(Agent::HyperUser);
        let star_transmitter = star_transmitter.build();

        Self {
            api,
            key: template.key.clone(),
            point,
            kind: template.kind.clone(),
            logger,
            golden_path,
            gravity_tx: star_tx.gravity_tx.clone(),
            gravity_router,
            gravity_transmitter,
            traverse_to_next_tx: star_tx.traverse_to_next_tx.clone(),
            inject_tx: star_tx.inject_tx.clone(),
            exchanger,
            state,
            registry: machine.registry.clone(),
            machine,
            adjacents,
            wrangles: StarWrangles::new(),
            drivers,
            drivers_traversal_tx: star_tx.drivers_traversal_tx.clone(),
            status_tx: star_tx.status_tx.clone(),
            status_rx: star_tx.status_rx.clone(),
            star_transmitter,
            #[cfg(test)]
            diagnostic_interceptors: DiagnosticInterceptors::new(),
            template,
        }
    }

    /*
    pub async fn create_star_particle(&self, point: Point, kind: Kind ) -> Result<(),P::Err> {

        if !self.point.is_parent_of(&point) {
            return Err(P::Err::new(format!("create_star_particle must be a child of star. expected: {}+:**, encountered: {}", self.point.to_string(), point.to_string())));
        }

        let registration = Registration {
            point: point.clone(),
            kind,
            registry: Default::default(),
            properties: Default::default(),
            owner: self.point.clone(),
            strategy: Strategy::Override,
            status: Status::Ready
        };

        self.registry.register(&registration).await?;
        self.api.create_states(point.clone()).await?;
        self.registry.assign(&point).send(self.point.clone());
        Ok(())
    }

     */

    #[track_caller]
    pub async fn create_in_star(&self, create: Create) -> Result<Details, P::Err> {
        if self.point != create.template.point.parent
            && !self.point.is_parent_of(&create.template.point.parent)
        {
            return Err(P::Err::new(format!("cannot create_in_star in star {} for parent point {} since it is not a point within this star", self.point.to_string(), create.template.point.parent.to_string())));
        }

        let logger = self.logger.push_mark("create").unwrap();
        let global = GlobalExecutionChamber::new(self.clone());
        let details = self.logger.result_ctx(
            format!(
                "StarSkel::create_in_star(register({}))",
                create.template.kind.to_string()
            )
            .as_str(),
            global.create(&create, &Agent::HyperUser).await,
        )?;
        let assign_body = Assign::new(AssignmentKind::Create, details.clone(), StateSrc::None);
        let mut assign = DirectedProto::sys(
            self.point.clone().to_surface().with_layer(Layer::Core),
            HypMethod::Assign,
        );

        assign.body(assign_body.into());
        let router = Arc::new(LayerInjectionRouter::new(
            self.clone(),
            self.point.clone().to_surface().with_layer(Layer::Shell),
        ));
        let mut transmitter = ProtoTransmitterBuilder::new(router, self.exchanger.clone());
        transmitter.from = SetStrategy::Override(self.point.to_surface().with_layer(Layer::Core));
        transmitter.agent = SetStrategy::Override(Agent::HyperUser);
        let transmitter = transmitter.build();
assign.track = true;
        let assign_result: Wave<Pong> = logger.result_ctx(
            "StarSkel::create(assign_result)",
            transmitter.direct(assign).await,
        )?;
        let location = ParticleLocation::new( self.point.clone(), None);
        self.registry
            .assign(&details.stub.point, location).await?;
        let logger = logger.push_mark("result").unwrap();
        logger.result(assign_result.ok_or())?;
        Ok(details)
    }

    pub fn err<M: ToString>(&self, message: M) -> Result<(), P::Err> {
        self.logger.warn(message.to_string());
        return Err(P::Err::new(message.to_string()));
    }

    pub fn location(&self) -> &Point {
        &self.logger.point
    }

    pub fn stub(&self) -> StarStub {
        StarStub::new(self.key.clone(), self.kind.clone())
    }
}

pub enum HyperStarCall<P>
where
    P: Cosmos,
{
    Init,
    CreateStates {
        point: Point,
        rtn: oneshot::Sender<()>,
    },
    Stub(oneshot::Sender<StarStub>),
    FromHyperway {
        wave: UltraWave,
        rtn: Option<oneshot::Sender<Result<(), UniErr>>>,
    },
    TraverseToNextLayer(Traversal<UltraWave>),
    LayerTraversalInjection(TraversalInjection),
    ToDriver(Traversal<UltraWave>),
    Phantom(PhantomData<P>),
    ToGravity(UltraWave),
    ToHyperway(Wave<Signal>),
    Shard(UltraWave),
    Wrangle(oneshot::Sender<Result<StarWrangles, UniErr>>),
    Bounce {
        key: StarKey,
        rtn: oneshot::Sender<Result<(), UniErr>>,
    },
    #[cfg(test)]
    GetSkel(oneshot::Sender<HyperStarSkel<P>>),
}

pub struct HyperStarTx<P>
where
    P: Cosmos,
{
    pub gravity_tx: mpsc::Sender<UltraWave>,
    pub traverse_to_next_tx: mpsc::Sender<Traversal<UltraWave>>,
    pub inject_tx: mpsc::Sender<TraversalInjection>,
    pub drivers_traversal_tx: mpsc::Sender<Traversal<UltraWave>>,
    pub call_tx: mpsc::Sender<HyperStarCall<P>>,
    pub call_rx: Option<mpsc::Receiver<HyperStarCall<P>>>,
    pub drivers_call_tx: mpsc::Sender<DriversCall<P>>,
    pub drivers_call_rx: Option<mpsc::Receiver<DriversCall<P>>>,
    pub drivers_status_tx: Option<watch::Sender<DriverStatus>>,
    pub drivers_status_rx: watch::Receiver<DriverStatus>,
    pub status_tx: mpsc::Sender<Status>,
    pub status_rx: watch::Receiver<Status>,
}

impl<P> HyperStarTx<P>
where
    P: Cosmos,
{
    pub fn new(point: Point) -> Self {
        let (gravity_tx, mut gravity_rx) = mpsc::channel(1024);
        let (inject_tx, mut inject_rx): (
            mpsc::Sender<TraversalInjection>,
            mpsc::Receiver<TraversalInjection>,
        ) = mpsc::channel(1024);
        let (traverse_to_next_tx, mut traverse_to_next_rx): (
            mpsc::Sender<Traversal<UltraWave>>,
            mpsc::Receiver<Traversal<UltraWave>>,
        ) = mpsc::channel(1024);
        let (drivers_traversal_tx, mut drivers_rx) = mpsc::channel(1024);
        let (drivers_call_tx, mut drivers_call_rx) = mpsc::channel(1024);
        let (drivers_status_tx, drivers_status_rx) = watch::channel(DriverStatus::Pending);
        let (mpsc_status_tx, mut mpsc_status_rx) = mpsc::channel(128);
        let (watch_status_tx, watch_status_rx) = watch::channel(Status::Pending);

        tokio::spawn(async move {
            while let Some(status) = mpsc_status_rx.recv().await {
                watch_status_tx.send(status);
            }
        });

        let (call_tx, call_rx) = mpsc::channel(1024);

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(wave) = gravity_rx.recv().await {
                    call_tx.send(HyperStarCall::ToGravity(wave)).await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(traversal) = traverse_to_next_rx.recv().await {
                    match call_tx
                        .send(HyperStarCall::TraverseToNextLayer(traversal.clone()))
                        .await
                    {
                        Ok(_) => {}
                        Err(err) => {
                            println!("CALL TX ERR: {}", err.to_string());
                        }
                    }
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(inject) = inject_rx.recv().await {
                    call_tx
                        .send(HyperStarCall::LayerTraversalInjection(inject))
                        .await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(inject) = drivers_rx.recv().await {
                    match call_tx.send(HyperStarCall::ToDriver(inject)).await {
                        Ok(_) => {}
                        Err(_) => {
                            panic!("driveres not working");
                        }
                    }
                }
                panic!("======== DRIVERS RX STOPPED");
            });
        }

        Self {
            gravity_tx,
            traverse_to_next_tx,
            inject_tx,
            drivers_traversal_tx,
            call_tx,
            call_rx: Some(call_rx),
            drivers_call_tx,
            drivers_call_rx: Option::Some(drivers_call_rx),
            drivers_status_tx: Some(drivers_status_tx),
            drivers_status_rx,
            status_tx: mpsc_status_tx,
            status_rx: watch_status_rx,
        }
    }

    pub fn star_rx(&mut self) -> Option<mpsc::Receiver<HyperStarCall<P>>> {
        self.call_rx.take()
    }
}

#[derive(Clone)]
pub struct HyperStarApi<P>
where
    P: Cosmos,
{
    pub kind: StarSub,
    tx: mpsc::Sender<HyperStarCall<P>>,
    pub status_rx: watch::Receiver<Status>,
}

impl<P> HyperStarApi<P>
where
    P: Cosmos,
{
    pub fn new(
        kind: StarSub,
        tx: mpsc::Sender<HyperStarCall<P>>,
        status_rx: watch::Receiver<Status>,
    ) -> Self {
        Self {
            kind,
            tx,
            status_rx,
        }
    }

    pub fn status(&self) -> Status {
        self.status_rx.borrow().clone()
    }

    pub async fn wait_for_status(&mut self, status: Status) {
        loop {
            if self.status_rx.borrow().clone() == status {
                break;
            }
            self.status_rx.changed().await.unwrap();
        }
    }

    pub async fn init(&self) {
        self.tx.send(HyperStarCall::Init).await;
    }

    pub async fn wrangle(&self) -> Result<StarWrangles, UniErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.tx.send(HyperStarCall::Wrangle(rtn)).await?;
        tokio::time::timeout(Duration::from_secs(5), rtn_rx).await??
    }

    pub async fn bounce(&self, key: StarKey) -> Result<(), UniErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.tx.send(HyperStarCall::Bounce { key, rtn }).await?;
        tokio::time::timeout(Duration::from_secs(5), rtn_rx).await??
    }

    pub async fn from_hyperway(&self, wave: UltraWave, results: bool) -> Result<(), UniErr> {
        match results {
            true => {
                let (tx, mut rx) = oneshot::channel();
                self.tx
                    .send(HyperStarCall::FromHyperway {
                        wave,
                        rtn: Some(tx),
                    })
                    .await;
                rx.await?
            }
            false => {
                self.tx
                    .send(HyperStarCall::FromHyperway { wave, rtn: None })
                    .await;
                Ok(())
            }
        }
    }

    pub async fn traverse_to_next_layer(&self, traversal: Traversal<UltraWave>) {
        self.tx
            .send(HyperStarCall::TraverseToNextLayer(traversal))
            .await;
    }

    pub async fn inject_traversal(&self, inject: TraversalInjection) {
        self.tx
            .send(HyperStarCall::LayerTraversalInjection(inject))
            .await;
    }

    pub async fn stub(&self) -> Result<StarStub, UniErr> {
        let (tx, rx) = oneshot::channel();
        self.tx.send(HyperStarCall::Stub(tx)).await;
        Ok(rx.await?)
    }

    pub async fn create_states(&self, point: Point) -> Result<(), UniErr> {
        let (rtn, rtn_rx) = oneshot::channel();
        self.tx
            .send(HyperStarCall::CreateStates { point, rtn })
            .await;
        rtn_rx.await?;
        Ok(())
    }

    #[cfg(test)]
    pub async fn get_skel(&self) -> Result<HyperStarSkel<P>, UniErr> {
        let (tx, rx) = oneshot::channel();
        self.tx.send(HyperStarCall::GetSkel(tx)).await;
        Ok(rx.await?)
    }

    #[cfg(test)]
    pub async fn to_gravity(&self, wave: UltraWave) {
        self.tx.send(HyperStarCall::ToGravity(wave)).await;
    }

    pub async fn to_hyperway(&self, wave: Wave<Signal>) {
        self.tx.send(HyperStarCall::ToHyperway(wave)).await;
    }
}

pub struct HyperStar<P>
where
    P: Cosmos + 'static,
{
    skel: HyperStarSkel<P>,
    star_tx: mpsc::Sender<HyperStarCall<P>>,
    star_rx: mpsc::Receiver<HyperStarCall<P>>,
    drivers: DriversApi<P>,
    injector: Surface,
    forwarders: Vec<Point>,
    hyperway_transmitter: ProtoTransmitter,
    gravity: Surface,
    hyper_router: Arc<dyn Router>,
    layer_traversal_engine: LayerTraversalEngine<P>,
    global_handler: DirectedHandlerShell<GlobalCommandExecutionHandler<P>>,
}

impl<P> HyperStar<P>
where
    P: Cosmos,
{
    pub async fn new(
        skel: HyperStarSkel<P>,
        mut drivers: DriversBuilder<P>,
        mut hyperway_endpoint: HyperwayEndpoint,
        interchange: Arc<HyperwayInterchange>,
        mut star_tx: HyperStarTx<P>,
    ) -> Result<HyperStarApi<P>, P::Err> {
        let drivers = drivers.build(
            skel.clone(),
            star_tx.drivers_call_tx.clone(),
            star_tx.drivers_call_rx.take().unwrap(),
            star_tx.drivers_status_tx.take().unwrap(),
            star_tx.drivers_status_rx.clone(),
        );

        let star_rx = star_tx.call_rx.take().unwrap();
        let star_tx = star_tx.call_tx;

        let global_port = Point::global_executor()
            .to_surface()
            .with_layer(Layer::Core);
        let mut transmitter = ProtoTransmitterBuilder::new(
            Arc::new(skel.gravity_router.clone()),
            skel.exchanger.clone(),
        );
        transmitter.from = SetStrategy::Override(global_port.clone());
        transmitter.agent = SetStrategy::Fill(Agent::HyperUser);

        let global_handler = DirectedHandlerShell::new(
            GlobalCommandExecutionHandler::new(skel.clone()),
            transmitter,
            global_port,
            skel.logger.logger.clone(),
        );

        let mut forwarders = vec![];
        for (point, stub) in skel.adjacents.iter() {
            if stub.kind.is_forwarder() {
                forwarders.push(point.clone());
            }
        }

        let hyper_router = Arc::new(TxRouter::new(hyperway_endpoint.tx.clone()));
        let mut hyperway_transmitter =
            ProtoTransmitterBuilder::new(hyper_router.clone(), skel.exchanger.clone());
        hyperway_transmitter.agent = SetStrategy::Override(Agent::HyperUser);
        hyperway_transmitter.scope = SetStrategy::Override(Scope::Full);
        let hyperway_transmitter = hyperway_transmitter.build();

        let mut injector = skel
            .location()
            .clone()
            .push("injector")
            .unwrap()
            .to_surface()
            .with_layer(Layer::Gravity);

        let (to_gravity_traversal_tx, mut to_gravity_traversal_rx): (
            mpsc::Sender<Traversal<UltraWave>>,
            mpsc::Receiver<Traversal<UltraWave>>,
        ) = mpsc::channel(1024);
        {
            let skel = skel.clone();
            tokio::spawn(async move {
                while let Some(traversal) = to_gravity_traversal_rx.recv().await {
                    skel.gravity_tx.send(traversal.payload).await;
                }
            });
        }

        let layer_traversal_engine = LayerTraversalEngine::new(
            skel.clone(),
            injector.clone(),
            skel.drivers_traversal_tx.clone(),
            to_gravity_traversal_tx,
        );

        let gravity = skel.point.clone().to_surface().with_layer(Layer::Gravity);

        // relay from hyper_rx
        {
            let star_tx = star_tx.clone();
            let skel = skel.clone();
            tokio::spawn(async move {
                while let Some(wave) = hyperway_endpoint.rx.recv().await {
                    star_tx
                        .send(HyperStarCall::FromHyperway { wave, rtn: None })
                        .await;
                }
                skel.status_tx.send(Status::Panic).await.unwrap_or_default();
            });
        }

        {
            let skel = skel.clone();
            let mut drivers = drivers.clone();
            let status_tx = skel.status_tx.clone();
            tokio::spawn(async move {
                let mut previous = DriverStatus::Unknown;
                loop {
                    if previous != drivers.status() {
                        previous = drivers.status();
                        match drivers.status() {
                            DriverStatus::Unknown => {
                                status_tx.send(Status::Unknown).await;
                            }
                            DriverStatus::Pending => {
                                status_tx.send(Status::Pending).await;
                            }
                            DriverStatus::Init => {
                                status_tx.send(Status::Init).await;
                            }
                            DriverStatus::Ready => {
                                let star_driver =
                                    drivers.get(&Kind::Star(skel.kind.clone())).await.unwrap();
                                star_driver.init_item(skel.point.to_point()).await;
                                status_tx.send(Status::Ready).await;
                            }
                            DriverStatus::Retrying(_) => {
                                status_tx.send(Status::Panic).await;
                            }
                            DriverStatus::Fatal(_) => {
                                status_tx.send(Status::Fatal).await;
                            }
                        }
                    }
                    match drivers.status_changed().await {
                        Ok(_) => {}
                        Err(_) => {
                            break;
                        }
                    }
                }
            });
        }

        let status_rx = skel.status_rx.clone();

        {
            let skel = skel.clone();
            tokio::spawn(async move {
                let logger = skel.logger.push_mark("client-connect").unwrap();
                for con in &skel.template.connections {
                    if let StarCon::Connector(stub) = con {
                        match interchange
                            .mount(
                                HyperwayStub::new(
                                    stub.key.to_point().to_surface().with_layer(Layer::Gravity),
                                    Agent::HyperUser,
                                ),
                                None,
                            )
                            .await
                        {
                            Ok(local_endpoint) => {
                                match skel
                                    .machine
                                    .api
                                    .endpoint_factory(skel.key.clone(), stub.key.clone())
                                    .await
                                {
                                    Ok(remote_factory) => {
                                        match Bridge::new(
                                            local_endpoint,
                                            remote_factory,
                                            logger.push_point("endpoint").unwrap(),
                                        ) {
                                            Ok(_) => {}
                                            Err(err) => {
                                                skel.logger.error(format!("could not create Bridge for remote connection: {} because {}", stub.key.to_string(), err.to_string()) );
                                                skel.status_tx.send(Status::Fatal).await;
                                            }
                                        }
                                    }
                                    Err(err) => {
                                        skel.logger.error(format!("could not create endpoint factory for remote connection: {} because {}", stub.key.to_string(), err.to_string()) );
                                        skel.status_tx.send(Status::Fatal).await;
                                    }
                                }
                            }
                            Err(err) => {
                                skel.logger.error(format!(
                                    "could not mount local connection: {} because {}",
                                    stub.key.to_string(),
                                    err.to_string()
                                ));
                                skel.status_tx.send(Status::Fatal).await;
                            }
                        }
                    }
                }
            });
        }

        {
            let skel = skel.clone();
            tokio::spawn(async move {
                let mut retries = 0;
                tokio::time::sleep(Duration::from_secs(5)).await;
                loop {
                    match tokio::time::timeout(Duration::from_secs(60), skel.api.wrangle()).await {
                        Ok(Ok(_)) => {
                            break;
                        }
                        Ok(Err(err)) => {
                            skel.logger.error(format!(
                                "HyperStar Auto Wrangle failed: {}",
                                err.to_string()
                            ));
                        }
                        Err(err) => {
                            skel.logger.error(format!(
                                "HyperStar Auto Wrangle failed: {}",
                                err.to_string()
                            ));
                        }
                    }
                    skel.logger.info("trying wrangle again in 5 seconds...");
                    if retries > 10 {
                        tokio::time::sleep(Duration::from_secs(15)).await;
                    } else if retries > 2 {
                        tokio::time::sleep(Duration::from_secs(5)).await;
                    } else {
                        tokio::time::sleep(Duration::from_secs(1)).await;
                    }
                    retries = retries + 1;
                }
            });
        }

        let kind = skel.kind.clone();
        {
            let star = Self {
                skel,
                star_tx: star_tx.clone(),
                star_rx,
                drivers,
                injector,
                hyperway_transmitter,
                forwarders,
                gravity,
                hyper_router,
                layer_traversal_engine,
                global_handler,
            };
            star.start();
        }

        Ok(HyperStarApi::new(kind, star_tx, status_rx))
    }

    fn start(mut self) {
        tokio::spawn(async move {
            while let Some(call) = self.star_rx.recv().await {
                match call {
                    HyperStarCall::Init => {
                        self.drivers.init().await;
                    }
                    HyperStarCall::FromHyperway { wave, rtn } => {
                        let result = self
                            .from_hyperway(wave)
                            .await
                            .map_err(|e| e.to_cosmic_err());
                        if let Some(tx) = rtn {
                            tx.send(result);
                        } else {
                            match result {
                                Ok(_) => {}
                                Err(e) => {
                                    self.skel.err(e.to_string());
                                }
                            }
                        }
                    }
                    HyperStarCall::TraverseToNextLayer(traversal) => {
                        let layer_traversal_engine = self.layer_traversal_engine.clone();
                        tokio::spawn(async move {
                            layer_traversal_engine
                                .traverse_to_next_layer(traversal)
                                .await;
                        });
                    }
                    HyperStarCall::LayerTraversalInjection(inject) => {
                        let layer_traversal_engine = self.layer_traversal_engine.clone();
                        tokio::spawn(async move {
                            layer_traversal_engine
                                .start_layer_traversal(inject.wave, &inject.injector, false)
                                .await;
                        });
                    }
                    HyperStarCall::Stub(rtn) => {
                        rtn.send(self.skel.stub());
                    }
                    HyperStarCall::Phantom(_) => {
                        // phantom literally does nothing but hold the P in not test mode
                    }
                    HyperStarCall::ToDriver(traversal) => {
                        self.drivers.visit(traversal).await;
                    }
                    HyperStarCall::ToGravity(wave) => match self.to_gravity(wave).await {
                        Ok(_) => {}
                        Err(err) => {
                            self.skel.err(err.to_string());
                        }
                    },
                    HyperStarCall::ToHyperway(wave) => match self.to_hyperway(wave).await {
                        Ok(_) => {}
                        Err(err) => {
                            self.skel.err(err.to_string());
                        }
                    },
                    #[cfg(test)]
                    HyperStarCall::GetSkel(rtn) => {
                        rtn.send(self.skel.clone());
                    }
                    HyperStarCall::CreateStates { point, rtn } => {
                        self.create_states(point).await;
                        rtn.send(());
                    }
                    HyperStarCall::Shard(wave) => {
                        self.shard(wave).await;
                    }
                    HyperStarCall::Wrangle(rtn) => {
                        self.wrangle(rtn).await;
                    }
                    HyperStarCall::Bounce { key, rtn } => {
                        self.bounce(key, rtn).await;
                    }
                }
            }
        });
    }

    async fn create_states(&self, point: Point) {
        self.skel.state.create_shell(point.clone());
    }

    async fn init_drivers(&self) {
        self.drivers.init().await;
    }

    // receive a wave from the hyperlane... this wave should always be
    // a Wave<Signal> of the SysMethod<Hop> which should in turn contain a SysMethod<Transport> Signal
    #[track_caller]
    async fn from_hyperway(&self, wave: UltraWave) -> Result<(), P::Err> {
        self.skel
            .logger
            .track(&wave, || Tracker::new("from_hyperway", "Receive"));
        #[cfg(test)]
        {
            let wave = wave.clone();
            self.skel.diagnostic_interceptors.from_hyperway.send(wave);
        }

        let mut transport = wave.unwrap_from_hop()?;
        transport.inc_hops();
        if transport.hops > 255 {
            self.skel.logger.track_msg(
                &transport,
                || Tracker::new("from_hyperway", "HopsExceeded"),
                || "transport hops exceeded",
            );
            return self.skel.err("transport signal exceeded max hops");
        }

        if transport.to.point == self.skel.point {
            // we are now going to send this transport down the layers to the StarCore
            // where it's contents will be unwrapped from transport and routed to the appropriate particle
            let layer_engine = self.layer_traversal_engine.clone();
            let injector = self.injector.clone();

            self.skel.logger.track(&transport, || {
                Tracker::new("from_hyperway", "SendToStartLayerTraversal")
            });

            tokio::spawn(async move {
                layer_engine
                    .start_layer_traversal(transport.to_ultra(), &injector, true)
                    .await;
            });
            Ok(())
        } else {
            self.forward(transport).await
        }
    }

    // send this transport signal towards it's destination
    async fn forward(&self, transport: Wave<Signal>) -> Result<(), P::Err> {
        if self.skel.kind.is_forwarder() {
            self.to_hyperway(transport).await
        } else {
            self.skel.err(format!(
                "attempt to forward a transport on a non forwarding Star Kind: {}",
                self.skel.kind.to_string()
            ))
        }
    }
    // sending a wave that is from and to a particle into the fabric...
    // here it will be wrapped into a transport for star to star delivery or
    // sent to GLOBAL::registry if addressed in such a way
    #[track_caller]
    async fn to_gravity(&self, mut wave: UltraWave) -> Result<(), P::Err> {
        wave.add_to_history(self.skel.point.clone());

        #[cfg(test)]
        self.skel
            .diagnostic_interceptors
            .to_gravity
            .send(wave.clone())
            .unwrap_or_default();
        let logger = self.skel.logger.push_mark("hyperstar:to-gravity").unwrap();
        logger.track(&wave, || Tracker::new("to_gravity", "Receive"));
        if wave.is_directed()
            && wave.to().is_single()
            && wave.to().to_single().unwrap().point == *GLOBAL_EXEC
        {
            let wave = wave.to_directed().unwrap();
            let handler = self.global_handler.clone();
            tokio::spawn(async move {
                handler.handle(wave).await;
            });
            return Ok(());
        } else {
            logger
                .result(self.star_tx.send(HyperStarCall::Shard(wave)).await)
                .unwrap_or_default();
        }
        Ok(())
    }

    #[track_caller]
    async fn shard(&self, mut wave: UltraWave) {
        wave.add_to_history(self.skel.point.clone());

        let skel = self.skel.clone();
        let locator = SmartLocator::new(self.skel.clone());
        let gravity = self.gravity.clone();
        tokio::spawn(async move {
            async fn shard<P>(
                mut wave: UltraWave,
                skel: HyperStarSkel<P>,
                locator: SmartLocator<P>,
                gravity: Surface,
            ) -> Result<(), P::Err>
            where
                P: Cosmos,
            {
                match &mut wave {
                    UltraWave::Ripple(ripple) => {
                        let mut map =
                            shard_ripple_by_location(ripple, &skel.adjacents, &skel.registry)
                                .await?;

                        for (star, mut wave) in map {
                            // add this star to history
                            wave.history.insert(skel.point.clone());
                            let mut transport = wave.to_ultra().wrap_in_transport(
                                gravity.clone(),
                                star.to_surface().with_layer(Layer::Core),
                            );
                            transport.from(skel.point.clone().to_surface());
                            let transport = transport.build()?;
                            let transport = transport.to_signal()?;
                            skel.api.to_hyperway(transport).await;
                        }
                    }
                    _ => {
                        let to = wave.to().unwrap_single();
                        let location = locator.locate(&to.point).await?;
                        let mut transport = wave.wrap_in_transport(
                            gravity,
                            location.star.to_surface().with_layer(Layer::Core),
                        );
                        transport.from(skel.point.clone().to_surface());
                        let transport = transport.build()?;
                        let transport = transport.to_signal()?;
                        skel.api.to_hyperway(transport).await;
                    }
                }
                Ok(())
            }
            let logger = skel.logger.push_mark("shard").unwrap();
            logger
                .result(shard(wave, skel, locator, gravity).await)
                .unwrap_or_default();
        });
    }

    // send this transport signal into the hyperway
    // wrap the transport into a hop to go to one and only one star

    #[track_caller]
    async fn to_hyperway(&self, transport: Wave<Signal>) -> Result<(), P::Err> {
        let logger = self.skel.logger.push_mark("hyperstar:to-hyperway")?;
        if self.skel.point == transport.to.point {
            // it's a bit of a strange case, but even if this star is sending a transport message
            // to itself, it still makes use of the Hyperway Interchange, which will bounce it back
            // The reason for this is that it is the Hyperway that handles things like Priority, Urgency
            // and hopefully in the future durability, whereas within the star itself all waves are
            // treated equally.
            logger.result(
                self.hyperway_transmitter
                    .direct(
                        transport.wrap_in_hop(
                            self.gravity.clone(),
                            self.skel.point.clone().to_surface(),
                        ),
                    )
                    .await,
            )?;
            Ok(())
        } else if self.skel.adjacents.contains_key(&transport.to.point) {
            let to = transport.to.clone();
            logger.result(
                self.hyperway_transmitter
                    .direct(transport.wrap_in_hop(self.gravity.clone(), to))
                    .await,
            )?;
            Ok(())
        } else if self.forwarders.len() == 1 {
            let to = self.forwarders.first().unwrap().clone().to_surface();
            logger.result(
                self.hyperway_transmitter
                    .direct(transport.wrap_in_hop(self.gravity.clone(), to))
                    .await,
            )?;
            Ok(())
        } else if self.forwarders.is_empty() {
            self.skel.err("this star needs to send a transport to a non-adjacent star yet does not have any adjacent forwarders")
        } else {
            unimplemented!("need to now send out a ripple search for the star being transported to")
        }
    }

    async fn wrangle(&self, rtn: oneshot::Sender<Result<StarWrangles, UniErr>>) {
        let skel = self.skel.clone();
        tokio::spawn(async move {
            let mut wrangler = Wrangler::new(skel.clone(), Search::Kinds);
            let mut history = HashSet::new();
            history.insert(skel.point.clone());
            wrangler.history(history);

            let discoveries = match wrangler.wrangle().await {
                Ok(discoveries) => discoveries,
                Err(err) => {
                    rtn.send(Err(err)).unwrap_or_default();
                    return;
                }
            };
            let mut coalated = vec![];
            for discovery in discoveries.iter() {
                coalated.push(StarDiscovery::new(
                    StarPair::new(
                        skel.key.clone(),
                        StarKey::try_from(discovery.star_key.to_point())
                            .expect("expected star key"),
                    ),
                    discovery.clone(),
                ));
            }
            coalated.sort();
            skel.wrangles.add(coalated).await;
            rtn.send(Ok(skel.wrangles.clone())).unwrap_or_default();
        });
    }

    async fn bounce(&self, key: StarKey, rtn: oneshot::Sender<Result<(), UniErr>>) {
        let transmitter = self.skel.star_transmitter.clone();
        let logger = self.skel.logger.clone();
        tokio::spawn(async move {
            let mut proto = DirectedProto::ping();
            proto.method(CmdMethod::Bounce);
            proto.to(key.to_point().to_surface().with_layer(Layer::Core));
            let pong: Wave<Pong> = match transmitter.direct(proto).await {
                Ok(pong) => pong,
                Err(err) => {
                    rtn.send(Err(err));
                    return;
                }
            };

            if pong.core.status.is_success() {
                rtn.send(Ok(()));
            } else {
                rtn.send(Err(pong.core.to_err()));
            }
        });
    }
}

#[derive(Clone)]
pub struct LayerTraversalEngine<P>
where
    P: Cosmos + 'static,
{
    pub skel: HyperStarSkel<P>,
    pub injector: Surface,
    pub exit_up: mpsc::Sender<Traversal<UltraWave>>,
    pub exit_down: mpsc::Sender<Traversal<UltraWave>>,
    pub layers: HashSet<Layer>,
}

impl<P> LayerTraversalEngine<P>
where
    P: Cosmos + 'static,
{
    pub fn new(
        skel: HyperStarSkel<P>,
        injector: Surface,
        exit_down: mpsc::Sender<Traversal<UltraWave>>,
        exit_up: mpsc::Sender<Traversal<UltraWave>>,
    ) -> Self {
        let mut layers = HashSet::new();
        layers.insert(Layer::Field);
        layers.insert(Layer::Shell);
        Self {
            skel,
            injector,
            exit_down,
            exit_up,
            layers,
        }
    }

    async fn start_layer_traversal(
        &self,
        mut wave: UltraWave,
        injector: &Surface,
        from_hyperway: bool,
    ) -> Result<(), P::Err> {
        #[cfg(test)]
        self.skel
            .diagnostic_interceptors
            .start_layer_traversal_wave
            .send(wave.clone())
            .unwrap_or_default();

        let logger = self
            .skel
            .logger
            .push_mark("hyperstar:start-layer-traversal")?;

        let tos = match wave.kind() {
            WaveKind::Ripple => {
                let mut tos = vec![];
                match wave.to() {
                    Recipients::Single(single) => {
                        tos.push(single);
                    }
                    Recipients::Multi(ports) => {
                        for port in &ports {
                            let record = self.skel.registry.record(&port.point).await?;
                            let loc = logger.result(record.location.ok_or(P::Err::new("multi port ripple has recipient that is not located, this should have been provisioned when the ripple was sent")))?;
                            if loc.star == self.skel.point {
                                tos.push(port.clone());
                            }
                        }
                    }
                    Recipients::Watchers(_) => {}
                    Recipients::Stars => {
                        if self.skel.point == wave.from().point {
                            tos.push(self.skel.point.to_surface().with_layer(Layer::Gravity));
                        } else {
                            tos.push(self.skel.point.to_surface().with_layer(Layer::Core));
                        }
                    }
                }
                tos
            }
            _ => {
                vec![wave.to().unwrap_single()]
            }
        };

        for to in tos {
            let record = match self.skel.registry.record(&to.point).await {
                Ok(record) => record,
                Err(err) => {
                    // this needs to send a 404  or 30x (moved) status to the caller
                    return self.skel.err(format!(
                        "could not locate record for surface {} from {}",
                        to.to_string(),
                        wave.from().to_string()
                    ));
                }
            };
            let plan = record.details.stub.kind.wave_traversal_plan().clone();

            let mut dest = None;
            let mut dir = TraversalDirection::Core;
            // now we check if we are doing an inter point delivery (from one layer to another in the same Particle)
            // if this delivery was from_hyperway, then it was certainly a message being routed back to the star
            // and is not considered an inter point delivery
            if !from_hyperway && to.point == wave.from().point {
                // it's the SAME point, so the to layer becomes our dest
                dest.replace(to.layer.clone());

                // make sure we have this layer in the plan
                if to.layer != Layer::Gravity && !plan.has_layer(&to.layer) {
                    return self.skel.err(format!("attempt to send wave {} to layer {} that the recipient Kind {} does not have in its traversal plan", wave.id().to_string(), to.layer.to_string(),record.details.stub.kind.to_string() ) );
                }

                // dir is from inject_layer to dest
                dir = match TraversalDirection::new(&injector.layer, &to.layer) {
                    Ok(dir) => dir,
                    Err(_) => {
                        // looks like we are already on the dest layer...
                        // that means it doesn't matter what the TraversalDirection is
                        TraversalDirection::Fabric
                    }
                }
            } else {

                    // if this was injected by something else (like the Star)
                    // then it needs to traverse towards the Core
                    dir = TraversalDirection::Core;
                    // and dest will be the to layer
                    if !from_hyperway {
                        dest.replace(to.layer.clone());
                    }
            }
if wave.track() && wave.transported().is_none() {
    println!("\n\rDIR : {} ",dir.to_string());
}

            let traversal_logger = self.skel.logger.point(to.to_point());
            let traversal_logger = traversal_logger.span();

            let point = if dir == TraversalDirection::Core {
                to.clone().to_point()
            } else {
                // if injected by any other point then the injector is the point that this traversal belongs to
                wave.from().to_point()
            };

            let mut traversal = Traversal::new(
                wave.clone(),
                record,
                injector.layer.clone(),
                traversal_logger,
                dir,
                dest,
                to,
                point,
            );

            // in the case that we injected into a layer that is not part
            // of this plan, we need to send the traversal to the next layer
            if !self.has_layer(&traversal.layer) {
                match traversal.next() {
                    None => {
                        self.exit(traversal).await;
                        continue;
                    }
                    Some(_) => {}
                }
            }

            #[cfg(test)]
            self.skel
                .diagnostic_interceptors
                .start_layer_traversal
                .send(traversal.clone())
                .unwrap_or_default();

            // alright, let's visit the injection layer first...
            self.visit_layer(traversal).await?;
        }
        Ok(())
    }

    fn has_layer(&self, layer: &Layer) -> bool {
        *layer == Layer::Shell || *layer == Layer::Field
    }

    async fn exit(&self, traversal: Traversal<UltraWave>) -> Result<(), UniErr> {
        match traversal.dir {
            TraversalDirection::Fabric => {
println!("Sending EXIT UP {}",traversal.method().as_ref().unwrap().to_string());
                self.exit_up.send(traversal).await;
                return Ok(());
            }
            TraversalDirection::Core => {
                self.exit_down.send(traversal).await;
                return Ok(());
            }
        }
    }

    async fn visit_layer(&self, traversal: Traversal<UltraWave>) -> Result<(), UniErr> {
        let logger = self.skel.logger.push_mark("stack-traversal:visit")?;
        logger.track(&traversal, || {
            Tracker::new(
                format!("visit:layer@{}", traversal.layer.to_string()),
                "Visit",
            )
        });

        match traversal.layer {
            Layer::Field => {
                let field = Field::new(traversal.point.clone(), self.skel.clone());
                tokio::spawn(async move {
                    let logger = logger.push_action("Field").unwrap();
                    logger
                        .result(field.visit(traversal).await)
                        .unwrap_or_default();
                });
            }
            Layer::Shell => {
                let shell = Shell::new(
                    self.skel.clone(),
                    self.skel
                        .state
                        .find_shell(&traversal.point.to_surface().with_layer(Layer::Shell))?,
                );

                let logger = logger.clone();
                tokio::spawn(async move {
                    let logger = logger.push_action("Shell").unwrap();
                    logger
                        .result(shell.visit(traversal).await)
                        .unwrap_or_default();
                });
            }
            _ => {
                logger
                    .result(self.exit(traversal).await)
                    .unwrap_or_default();
            }
        }
        Ok(())
    }

    async fn traverse_to_next_layer(&self, mut traversal: Traversal<UltraWave>) {
        let logger = self
            .skel
            .logger
            .push_mark("stack-traversal:traverse-to-next-layer")
            .unwrap();

        self.skel
            .logger
            .track(&traversal, || Tracker::new("traverse", "NextLayer"));

        if traversal.dest.is_some() && traversal.layer == *traversal.dest.as_ref().unwrap() {
            self.visit_layer(traversal).await;
            return;
        }

        let next = traversal.next();

        match next {
            None => match traversal.dir {
                TraversalDirection::Fabric => {
                    self.exit_up.send(traversal).await;
                }
                TraversalDirection::Core => {
                    logger.warn("should not have traversed a wave all the way to the core in Star");
                }
            },
            Some(_) => {
                logger
                    .result(self.visit_layer(traversal).await)
                    .unwrap_or_default();
            }
        }
    }
}

pub struct StarMount {
    pub point: Point,
    pub kind: MountKind,
    pub tx: mpsc::Sender<UltraWave>,
}

#[derive(Clone)]
pub struct LayerInjectionRouter {
    pub inject_tx: mpsc::Sender<TraversalInjection>,
    pub injector: Surface,
}

impl LayerInjectionRouter {
    pub fn new<P>(skel: HyperStarSkel<P>, injector: Surface) -> Self
    where
        P: Cosmos,
    {
        Self {
            inject_tx: skel.inject_tx.clone(),
            injector,
        }
    }

    pub fn with(&self, injector: Surface) -> Self {
        Self {
            inject_tx: self.inject_tx.clone(),
            injector,
        }
    }

    pub fn injector(inject_tx: mpsc::Sender<TraversalInjection>, injector: Surface) -> Self {
        Self {
            inject_tx,
            injector,
        }
    }
}

#[async_trait]
impl Router for LayerInjectionRouter {
    async fn route(&self, wave: UltraWave) {
        let inject = TraversalInjection::new(self.injector.clone(), wave);
        self.inject_tx.send(inject).await;
    }

}

pub trait TopicHandler: Send + Sync + DirectedHandler {
    fn source_selector(&self) -> &SurfaceSelector;
}

pub trait TopicHandlerSerde<T: TopicHandler> {
    fn serialize(&self, handler: T) -> Substance;
    fn deserialize(&self, ser: Substance) -> T;
}

#[derive(Clone)]
pub struct StarTemplate {
    pub key: StarKey,
    pub kind: StarSub,
    pub connections: Vec<StarCon>,
}

impl StarTemplate {
    pub fn new(key: StarKey, kind: StarSub) -> Self {
        Self {
            key,
            kind,
            connections: vec![],
        }
    }

    pub fn to_stub(&self) -> StarStub {
        StarStub::new(self.key.clone(), self.kind.clone())
    }

    pub fn receive(&mut self, stub: StarStub) {
        self.connections.push(StarCon::Receiver(stub));
    }

    pub fn connect(&mut self, stub: StarStub) {
        self.connections.push(StarCon::Connector(stub));
    }
}

#[derive(Clone)]
pub enum StarCon {
    Receiver(StarStub),
    Connector(StarStub),
}

impl StarCon {
    pub fn is_connector(&self) -> bool {
        match self {
            StarCon::Receiver(_) => false,
            StarCon::Connector(_) => true,
        }
    }

    pub fn is_receiver(&self) -> bool {
        match self {
            StarCon::Receiver(_) => true,
            StarCon::Connector(_) => false,
        }
    }

    pub fn stub(&self) -> &StarStub {
        match self {
            StarCon::Receiver(stub) => stub,
            StarCon::Connector(stub) => stub,
        }
    }

    pub fn key(&self) -> &StarKey {
        match self {
            StarCon::Receiver(stub) => &stub.key,
            StarCon::Connector(stub) => &stub.key,
        }
    }

    pub fn kind(&self) -> &StarSub {
        match self {
            StarCon::Receiver(stub) => &stub.kind,
            StarCon::Connector(stub) => &stub.kind,
        }
    }
}

async fn shard_ripple_by_location<E>(
    ripple: &Wave<Ripple>,
    adjacent: &HashMap<Point, StarStub>,
    registry: &Registry<E>,
) -> Result<HashMap<Point, Wave<Ripple>>, E::Err>
where
    E: Cosmos,
{
    let mut map = HashMap::new();
    for (star, recipients) in shard_by_location(ripple.to.clone(), adjacent, registry).await? {
        if !ripple.history.contains(&star) {
            let mut ripple = ripple.clone();
            ripple.variant.to = recipients;
            map.insert(star, ripple);
        } else {
        }
    }
    Ok(map)
}

pub async fn ripple_to_singulars<E>(
    ripple: Wave<Ripple>,
    adjacent: &HashSet<Point>,
    registry: &Registry<E>,
) -> Result<Vec<Wave<SingularRipple>>, E::Err>
where
    E: Cosmos,
{
    let mut rtn = vec![];
    for port in to_ports(ripple.to.clone(), adjacent, registry).await? {
        let wave = ripple.as_single(port);
        rtn.push(wave)
    }
    Ok(rtn)
}

pub async fn shard_by_location<E>(
    recipients: Recipients,
    adjacent: &HashMap<Point, StarStub>,
    registry: &Registry<E>,
) -> Result<HashMap<Point, Recipients>, E::Err>
where
    E: Cosmos,
{
    match recipients {
        Recipients::Single(single) => {
            Err(E::Err::new("unimplemented"))
            /*
            let mut map = HashMap::new();
            let record = registry.locate(&single.point).await?;
            map.insert(record.location, Recipients::Single(single));
            Ok(map)

             */
        }
        Recipients::Multi(multi) => {
            Err(E::Err::new("unimplemented"))
            /*
            let mut map: HashMap<Point, Vec<Port>> = HashMap::new();
            for p in multi {
                let record = registry.locate(&p).await?;
                if let Some(found) = map.get_mut(&record.location) {
                    found.push(p);
                } else {
                    map.insert(record.location, vec![p]);
                }
            }


            let mut map2 = HashMap::new();
            for (location, points) in map {
                map2.insert(location, Recipients::Multi(points));
            }
            Ok(map2)
             */
        }
        Recipients::Watchers(_) => {
            let mut map = HashMap::new();
            // todo
            Ok(map)
        }
        Recipients::Stars => {
            let mut map = HashMap::new();
            for (star, _) in adjacent {
                map.insert(star.clone(), Recipients::Stars);
            }
            Ok(map)
        }
    }
}

pub async fn to_ports<E>(
    recipients: Recipients,
    adjacent: &HashSet<Point>,
    registry: &Registry<E>,
) -> Result<Vec<Surface>, E::Err>
where
    E: Cosmos,
{
    match recipients {
        Recipients::Single(single) => Ok(vec![single]),
        Recipients::Multi(multi) => Ok(multi.into_iter().map(|p| p).collect()),
        Recipients::Watchers(watch) => {
            unimplemented!();
        }
        Recipients::Stars => {
            let stars: Vec<Surface> = adjacent
                .clone()
                .into_iter()
                .map(|p| p.to_surface())
                .collect();
            Ok(stars)
        }
    }
}

#[derive(Clone)]
pub struct DiagnosticInterceptors<P>
where
    P: Cosmos,
{
    pub from_hyperway: broadcast::Sender<UltraWave>,
    pub to_gravity: broadcast::Sender<UltraWave>,
    pub to_hyperway: broadcast::Sender<Wave<Signal>>,
    pub start_layer_traversal_wave: broadcast::Sender<UltraWave>,
    pub start_layer_traversal: broadcast::Sender<Traversal<UltraWave>>,
    pub transport_endpoint: broadcast::Sender<UltraWave>,
    pub reflected_endpoint: broadcast::Sender<UltraWave>,
    pub assignment: broadcast::Sender<Assign>,
    pub err: broadcast::Sender<P::Err>,
}

impl<P> DiagnosticInterceptors<P>
where
    P: Cosmos,
{
    pub fn new() -> Self {
        let (from_hyperway, _) = broadcast::channel(1024);
        let (to_hyperway, _) = broadcast::channel(1024);
        let (to_gravity, _) = broadcast::channel(1024);
        let (start_layer_traversal, _) = broadcast::channel(1024);
        let (start_layer_traversal_wave, _) = broadcast::channel(1024);
        let (err, _) = broadcast::channel(1024);
        let (transport_endpoint, _) = broadcast::channel(1024);
        let (reflected_endpoint, _) = broadcast::channel(1024);
        let (assignment, _) = broadcast::channel(1024);
        Self {
            from_hyperway,
            to_hyperway,
            to_gravity,
            start_layer_traversal,
            start_layer_traversal_wave,
            err,
            transport_endpoint,
            reflected_endpoint,
            assignment,
        }
    }
}

#[derive(Clone)]
pub struct SmartLocator<P>
where
    P: Cosmos,
{
    pub skel: HyperStarSkel<P>,
}

impl<P> SmartLocator<P>
where
    P: Cosmos,
{
    pub fn new(skel: HyperStarSkel<P>) -> Self {
        Self { skel }
    }

    pub async fn locate(&self, point: &Point) -> Result<ParticleLocation, P::Err> {
        let record = self.skel.registry.record(&point).await?;
        match record.location {
            Some(location) => Ok(location),
            None => {
                // now we must provision
                self.provision(point, StateSrc::None).await
            }
        }
    }

    #[async_recursion]
    pub async fn provision(&self, point: &Point, state: StateSrc) -> Result<ParticleLocation, P::Err> {
        // check if parent is provisioned
        let parent = point
            .parent()
            .ok_or(P::Err::new("expected Root to be provisioned"))?;
        let mut parent_record = self.skel.registry.record(&parent).await?;
        if parent_record.location.is_none() {
            self.provision(&parent, StateSrc::None).await?;
            parent_record = self.skel.registry.record(&parent).await?;
        }

        let parent_star = parent_record.location.unwrap();
        let provision = Provision::new(point.clone(), state);
        let mut wave = DirectedProto::ping();
        wave.method(HypMethod::Provision);
        wave.body(HyperSubstance::Provision(provision).into());
        wave.from(self.skel.point.clone().to_surface().with_layer(Layer::Core));
        wave.to(parent_star.star.to_surface().with_layer(Layer::Core));
        let pong: Wave<Pong> = self.skel.star_transmitter.direct(wave).await?;
        if pong.core.status.as_u16() == 200 {
            if let Substance::Location(location) = &pong.core.body {
                Ok(location.clone())
            } else {
                Err(P::Err::new("Provision result expected Substance Point"))
            }
        } else {
            self.skel
                .registry
                .set_status(&point, &Status::Panic)
                .await?;

            match self.skel.registry.record(&point).await {
                Ok(record) => Err(P::Err::new(format!(
                    "failed to provision {}<{}>",
                    point.to_string(),
                    record.details.stub.kind.to_template().to_string()
                ))),
                Err(_) => Err(P::Err::new(format!(
                    "failed to provision {}",
                    point.to_string()
                ))),
            }
        }
    }
}
