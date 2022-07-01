use mesh_portal_versions::RegistryApi;
use crate::machine::MachineSkel;
use crate::star::StarSkel;
use mesh_portal_versions::DriverState;
use dashmap::DashMap;
use mesh_portal_versions::error::MsgErr;
use mesh_portal_versions::version::v0_0_1::id::id::{Kind, Layer, ToPoint, TraversalLayer, Uuid};
use mesh_portal_versions::version::v0_0_1::id::{StarKey, Traversal, TraversalInjection};
use mesh_portal_versions::version::v0_0_1::log::PointLogger;
use mesh_portal_versions::version::v0_0_1::particle::particle::Status;
use mesh_portal_versions::version::v0_0_1::substance::substance::Substance;
use mesh_portal_versions::version::v0_0_1::sys::{Assign, Sys};
use mesh_portal_versions::version::v0_0_1::wave::{
    DirectedHandler, InCtx, Ping, ReflectedCore, Pong, Wave,
};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::Sender;
use tokio::sync::{broadcast, mpsc, oneshot};
use mesh_portal::version::latest::id::{Point, Port};

#[derive(AsyncRequestHandler)]
pub struct Drivers {
    pub skel: StarSkel,
    pub drivers: HashMap<Kind, mpsc::Sender<DriverCall>>,
}

impl Drivers {
    pub fn new(skel: StarSkel, drivers: HashMap<Kind, mpsc::Sender<DriverCall>>) -> Self {
        Self { skel, drivers }
    }

    pub async fn init(&self) -> Result<(), MsgErr> {
        let mut errs = vec![];
        for driver in self.drivers.values() {
            let status = driver.status().await;
            if status != DriverStatus::Ready
                && status != DriverStatus::Initializing
            {
                driver.lifecycle(DriverLifecycleCall::Init);
            }

            if driver.status().await != DriverStatus::Ready {
                errs.push(MsgErr::server_error());
            }
        }

        if !errs.is_empty() {
            // need to fold these errors into one
            Err(MsgErr::server_error())
        } else {
            Ok(())
        }
    }
}

impl Drivers {
    pub async fn assign(&self, ctx: InCtx<'_, Sys>) -> Result<ReflectedCore, MsgErr> {
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

    async fn start_outer_traversal(&self, traversal: Traversal<Wave>) {
        self.skel.traverse_to_next.send(traversal).await;
    }

    async fn start_inner_traversal(&self, traversal: Traversal<Wave>) {
    }


    pub async fn visit(&self, traversal: Traversal<Wave>) {
        if traversal.dir.is_core() {
            match self.drivers.get(&traversal.record.details.stub.kind) {
                None => {
                    traversal.logger.warn(format!(
                        "star does not have a driver for Kind <{}>",
                        traversal.record.details.stub.kind.to_string()
                    ));
                }
                Some(driver) => {
                    let driver_ex = driver.ex(&traversal.to().point, DriverState::None );
                    driver_ex.visit(traversal).await;
                }
            }
        } else {
            self.start_outer_traversal(traversal).await;
        }
    }
}

pub struct DriversBuilder {
    pub factories: HashMap<Kind, Box<dyn DriverFactory>>,
    pub logger: Option<PointLogger>,
}

impl DriversBuilder {
    pub fn add(&mut self, factory: Box<dyn DriverFactory>) {
        self.factories.insert(factory.kind().clone(), factory);
    }

    pub fn logger(&mut self, logger: PointLogger) {
        self.logger.replace(logger);
    }

    pub fn build(self, skel: StarSkel) -> Result<Drivers, MsgErr> {
        if self.logger.is_none() {
            return Err("expected point logger to be set".into());
        }
        let mut drivers = HashMap::new();
        for factory in self.factories.values() {
            let point = skel.location().clone().push(factory.kind().as_point_segments() ).unwrap();
            let driver_skel = DriverSkel::new( skel.clone(), point );
            let core = factory.create(driver_skel);
            let shell = DriverShell::new(skel.clone(), core);
            drivers.insert(factory.kind().clone(), shell);
        }
        Ok(Drivers::new(skel, drivers))
    }
}

pub trait DriverFactory {
    fn kind(&self) -> &Kind;
    fn create(&self, skel: DriverSkel) -> Box<dyn DriverCore>;
}

enum DriverCall {
    LifecycleCall(DriverLifecycleCall),
    Traversal(Traversal<Wave>),
    Handle(Ping)
}

pub struct DriverEx {
   pub port: Port,
   pub skel: DriverSkel,
   pub state: DriverState,
}

#[async_trait]
impl TraversalLayer for DriverEx {
    fn port(&self) -> &mesh_portal_versions::version::v0_0_1::id::id::Port {
        &self.port
    }

    async fn traverse_next(&self, traversal: Traversal<Wave>) {
        self.skel.traversal_router.send(traversal).await;
    }

    async fn inject(&self, wave: Wave) {
        let inject = TraversalInjection::new(self.port().clone(),wave);
        self.skel.inject_tx.send(inject).await;
    }

    fn exchange(&self) -> &Arc<DashMap<Uuid, oneshot::Sender<Pong>>> {
        &self.skel.exchange
    }
}

#[derive(AsyncRequestHandler)]
pub struct DriverShell {
    skel: StarSkel,
    status: DriverStatus,
    states: Arc<DashMap<Point,DriverState>>,
    tx: mpsc::Sender<DriverCall>,
    rx: mpsc::Receiver<DriverCall>,
    core: Box<dyn DriverCore>
}

#[routes_async]
impl DriverShell {

    pub fn new(skel: StarSkel, core: Box<dyn DriverCore>) -> mpsc::Sender<DriverCall>{
        let kind = core.kind().clone();
        let states = Arc::new(DashMap::new());
        skel.state.driver.insert(kind.clone(),states.clone());
        let (tx,rx) = mpsc::channel(1024);
        let driver = Self {
            skel,
            status: DriverStatus::Started,
            states,
            tx: tx.clone(),
            rx,
            core
        };

        driver.start();

        tx
    }

    fn start( mut self ) {
        tokio::spawn(async move {
            while let Some(call) = self.rx.recv().await {
                match call {
                    DriverCall::LifecycleCall(lifecycle) => {
                        self.lifecycle(lifecycle);
                    }
                    DriverCall::Traversal(traversal) => {
                        self.traverse(traversal);
                    }
                    DriverCall::Handle(req) => {
                        self.handle(req).await;
                    }
                }
            }
        });
    }

    fn lifecycle(&self, event: DriverLifecycleCall) {
        self.core.lifecycle(event);
    }

    fn get_state( &self, point: &Point ) -> DriverState {
        match self.states.get(point) {
            None => {
                let state = self.core.new_state();
                self.states.insert( point.clone(), state.clone() );
                state
            }
            Some(state) => {
                state.clone()
            }
        }
    }

    fn ex( &self, point: &Point ) -> DriverEx {
        self.core.ex(point, self.get_state(point))
    }

    fn traverse( &self, traversal: Traversal<Wave> ) {
        let driver_ex = self.ex(&traversal.to().point);
        driver_ex.visit(traversal).await;
    }

    #[route("Sys<Assign>")]
    async fn assign(&self, ctx: InCtx<'_,Sys>) -> Result<ReflectedCore, MsgErr> {
        match ctx.input {
            Sys::Assign(assign) => {
                let ctx = ctx.push_input_ref(assign);
                self.core.assign(ctx).await
            }
            _ => {
                Err(MsgErr::bad_request())
            }
        }
    }

    fn status(&self) -> &DriverStatus {
        & self.status
    }

}




#[async_trait]
pub trait DriverCore: DirectedHandler {
    fn kind(&self) -> &Kind;
    async fn status(&self) -> DriverStatus;
    fn lifecycle(&self, event: DriverLifecycleCall);
    fn new_state(&self) -> DriverState;
    fn ex(&self, point: &Point, state: DriverState ) -> DriverEx;
    async fn assign(&self, ctx: InCtx<'_,Assign>) -> Result<ReflectedCore, MsgErr>;
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum DriverLifecycleCall {
    Init,
    Shutdown,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub enum DriverStatus {
    Started,
    Initializing,
    Ready,
    Unavailable,
}

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DriverStatusEvent {
    pub driver: Point,
    pub status: DriverStatus
}

#[derive(Clone)]
pub struct DriverSkel {
    pub location: Point,
    pub star: StarKey,
    pub logger: PointLogger,
    pub registry: Arc<dyn RegistryApi>,
    pub surface: mpsc::Sender<Wave>,
    pub traversal_router: mpsc::Sender<Traversal<Wave>>,
    pub inject_tx: mpsc::Sender<TraversalInjection>,
    pub fabric: mpsc::Sender<Wave>,
    pub machine: MachineSkel,
    pub exchange: Arc<DashMap<Uuid, oneshot::Sender<Pong>>>,
    pub status_tx: broadcast::Sender<DriverStatusEvent>,
    pub states: Arc<DashMap<Point,DriverState>>,
    pub point: Point
}

impl  DriverSkel {
    fn new(skel: StarSkel, point: Point) -> Self {
        let location = skel.location().clone();
        let (status_tx,_) = broadcast::channel(16);
        let states = Arc::new(DashMap::new());
        let logger = skel.logger.point(point.clone());
        Self {
            location,
            star: skel.key,
            logger,
            registry: skel.registry,
            surface: skel.surface,
            traversal_router: skel.traverse_to_next,
            fabric: skel.fabric,
            machine: skel.machine,
            exchange: skel.exchange,
            inject_tx: skel.inject_tx,
            status_tx,
            states,
            point
        }
    }
}
