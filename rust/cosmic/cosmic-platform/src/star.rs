use crate::driver::Drivers;
use crate::field::{FieldEx, FieldState};
use crate::machine::MachineSkel;
use crate::shell::ShellEx;
use crate::state::ShellState;
use cosmic_api::bin::Bin;
use cosmic_api::cli::RawCommand;
use cosmic_api::command::command::common::StateSrc;
use cosmic_api::command::request::set::Set;
use cosmic_api::config::config::bind::RouteSelector;
use cosmic_api::error::MsgErr;
use cosmic_api::id::id::{
    Kind, Layer, Point, Port, PortSelector, RouteSeg, Sub, ToPoint, ToPort, Topic, TraversalLayer,
    Uuid,
};
use cosmic_api::id::{StarKey, StarStub, StarSub, TraversalInjection};
use cosmic_api::id::{Traversal, TraversalDirection};
use cosmic_api::log::{PointLogger, RootLogger};
use cosmic_api::parse::{route_attribute, Env};
use cosmic_api::particle::particle::{Details, Status, Stub};
use cosmic_api::quota::Timeouts;
use cosmic_api::substance::substance::{Substance, ToSubstance};
use cosmic_api::sys::{Assign, AssignmentKind, Discoveries, Discovery, Location, Search, Sys};
use cosmic_api::util::{ValueMatcher, ValuePattern};
use cosmic_api::wave::{Agent, Bounce, BounceBacks, CoreBounce, DirectedHandler, DirectedHandlerSelector, DirectedKind, DirectedProto, Echo, Echoes, Handling, HandlingKind, InCtx, Method, Ping, Pong, Priority, ProtoTransmitter, RecipientSelector, Recipients, Reflectable, ReflectedCore, ReflectedWave, Retries, Ripple, RootInCtx, Router, Scope, SetStrategy, Signal, ToRecipients, TxRouter, WaitTime, Wave, SingularRipple, ProtoTransmitterBuilder, DirectedWave};
use cosmic_api::wave::{DirectedCore, Exchanger, HyperWave, SysMethod, UltraWave};
use cosmic_api::{
    MountKind, Registration, State, StateFactory,
};
use cosmic_driver::{Core, Driver, DriverFactory, DriverLifecycleCall, DriverSkel, DriverStatus};
use cosmic_hyperlane::HyperRouter;
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use futures::future::{join_all, BoxFuture};
use futures::FutureExt;
use http::StatusCode;
use serde::{Deserialize, Serialize};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::marker::PhantomData;
use std::ops::{Add, Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::{mpsc, oneshot, Mutex, RwLock, broadcast};
use tokio::time::error::Elapsed;
use tracing::info;
use crate::{PlatErr, Platform, Registry, RegistryApi};

#[derive(Clone)]
pub struct StarState<P>
where
    P: Platform + 'static,
{
    states: Arc<DashMap<Port, Arc<RwLock<dyn State>>>>,
    topic: Arc<DashMap<Port, Arc<dyn TopicHandler>>>,
    tx: mpsc::Sender<StateCall>,
    field: Arc<DashMap<Port, FieldState<P>>>,
    shell: Arc<DashMap<Port, ShellState>>,
}

impl<P> StarState<P>
where
    P: Platform + 'static,
{
    pub fn new() -> Self {
        let states: Arc<DashMap<Port, Arc<RwLock<dyn State>>>> = Arc::new(DashMap::new());

        let (tx, mut rx) = mpsc::channel(32 * 1024);

        {
            let states = states.clone();
            tokio::spawn(async move {
                while let Some(call) = rx.recv().await {
                    match call {
                        StateCall::Get { port, tx } => match states.get(&port) {
                            None => {
                                tx.send(Ok(None));
                            }
                            Some(state) => {
                                tx.send(Ok(Some(state.value().clone())));
                            }
                        },
                        StateCall::Put { port, state, tx } => {
                            if states.contains_key(&port) {
                                tx.send(Err(MsgErr::bad_request()));
                            } else {
                                states.insert(port, state);
                                tx.send(Ok(()));
                            }
                        }
                    }
                }
            });
        }

        Self {
            states,
            topic: Arc::new(DashMap::new()),
            field: Arc::new(DashMap::new()),
            shell: Arc::new(DashMap::new()),
            tx,
        }
    }

    pub fn api(&self) -> StateApi {
        StateApi::new(self.tx.clone())
    }

    pub fn states_tx(&self) -> mpsc::Sender<StateCall> {
        self.tx.clone()
    }

    pub fn topic_handler(&self, port: Port, handler: Arc<dyn TopicHandler>) {
        self.topic.insert(port, handler);
    }

    pub async fn find_state<S>(&self, port: &Port) -> Result<Arc<RwLock<dyn State>>, MsgErr> {
        Ok(self
            .states
            .get(port)
            .ok_or(format!("could not find state for: {}", port.to_string()))?
            .value()
            .clone())
    }

    pub fn find_topic(
        &self,
        port: &Port,
        source: &Port,
    ) -> Option<Result<Arc<dyn TopicHandler>, MsgErr>> {
        match self.topic.get(port) {
            None => None,
            Some(topic) => {
                let topic = topic.value().clone();
                if topic.source_selector().is_match(source).is_ok() {
                    Some(Ok(topic))
                } else {
                    Some(Err(MsgErr::forbidden()))
                }
            }
        }
    }

    pub fn find_field(&self, port: &Port) -> Result<FieldState<P>, MsgErr> {
        let rtn = self
            .field
            .get(port)
            .ok_or("expected field state")?
            .value()
            .clone();
        Ok(rtn)
    }

    pub fn find_shell(&self, port: &Port) -> Result<ShellState, MsgErr> {
        Ok(self
            .shell
            .get(port)
            .ok_or("expected shell state")?
            .value()
            .clone())
    }
}

#[derive(Clone)]
pub struct StarSkel<P>
where
    P: Platform+'static
{
    pub key: StarKey,
    pub point: Point,
    pub kind: StarSub,
    pub kinds: HashSet<Kind>,
    pub logger: PointLogger,
    pub registry: Registry<P>,
    pub traverse_to_next_tx: mpsc::Sender<Traversal<UltraWave>>,
    pub inject_tx: mpsc::Sender<TraversalInjection>,
    pub machine: MachineSkel<P>,
    pub exchanger: Exchanger,
    pub state: StarState<P>,
    pub connections: Vec<StarCon>,
    pub adjacents: HashMap<Point,StarStub>,
    pub wrangles: StarWrangles,
    pub gravity_well_tx: mpsc::Sender<UltraWave>,
    pub gravity_well_router: TxRouter,
    pub gravity_well_transmitter: ProtoTransmitter,

    #[cfg(test)]
    pub diagnostic_interceptors: DiagnosticInterceptors<P>
}

impl<P> StarSkel<P>
where
    P: Platform,
{
    pub fn new(template: StarTemplate, machine: MachineSkel<P>, kinds: HashSet<Kind>) -> Self {
        let point = template.key.clone().to_point();
        let logger = machine.logger.point(point.clone());
        let star_tx: StarTx<P> = StarTx::new(point.clone());
        let exchanger = Exchanger::new(point.clone().to_port(), machine.timeouts.clone());
        let state = StarState::new();

        let mut adjacents = HashMap::new();
        // prime the searcher by mapping the immediate lanes
        for hyperway in template.hyperway.clone() {
            adjacents.insert(hyperway.key().clone().to_point(), hyperway.stub().clone() );
        }

        let gravity_well_router = TxRouter::new(star_tx.gravity_well_tx.clone());
        let mut gravity_well_transmitter =
            ProtoTransmitterBuilder::new(Arc::new(gravity_well_router.clone()), exchanger.clone());
        gravity_well_transmitter.from = SetStrategy::Override(point.clone().to_port());
        gravity_well_transmitter.handling = SetStrategy::Fill(Handling {
            kind: HandlingKind::Immediate,
            priority: Priority::High,
            retries: Retries::None,
            wait: WaitTime::Low,
        });
        gravity_well_transmitter.agent = SetStrategy::Fill(Agent::HyperUser);
        gravity_well_transmitter.scope = SetStrategy::Fill(Scope::Full);

        let gravity_well_transmitter = gravity_well_transmitter.build();



        Self {
            key: template.key,
            point,
            kind: template.kind,
            kinds,
            logger,
            gravity_well_tx: star_tx.gravity_well_tx.clone(),
            gravity_well_router,
            gravity_well_transmitter,
            traverse_to_next_tx: star_tx.traverse_to_next.clone(),
            inject_tx: star_tx.inject_tx.clone(),
            exchanger,
            state,
            connections: template.hyperway,
            registry: machine.registry.clone(),
            machine,
            adjacents,
            wrangles: StarWrangles::new(),

            #[cfg(test)]
            diagnostic_interceptors: DiagnosticInterceptors::new()
        }
    }

    pub fn err<M:ToString>(&self, message: M) -> Result<(),P::Err> {
        self.logger.warn(message.to_string());
        return Err(P::Err::new(message.to_string()));
    }

    pub fn location(&self) -> &Point {
        &self.logger.point
    }

    pub fn create_star_drivers(&self, driver_skel: DriverSkel) -> HashMap<Kind, Box<dyn Driver>> {
        let mut rtn: HashMap<Kind, Box<dyn Driver>> = HashMap::new();
        let star_driver = StarDriver::new(self.clone(), driver_skel);
        rtn.insert(star_driver.kind().clone(), Box::new(star_driver));
        rtn
    }

    pub fn stub(&self) -> StarStub {
        StarStub::new( self.key.clone(), self.kind.clone() )
    }


}

pub enum StarCall<P> where P: Platform {
    PreInit(oneshot::Sender<Result<(),MsgErr>>),
    Stub(oneshot::Sender<StarStub>),
    FromHyperway{wave: HyperWave, tx: Option<oneshot::Sender<Result<(),MsgErr>>>},
    TraverseToNextLayer(Traversal<UltraWave>),
    LayerTraversalInjection(TraversalInjection),
    CreateMount {
        agent: Agent,
        kind: MountKind,
        tx: oneshot::Sender<(mpsc::Sender<UltraWave>, mpsc::Receiver<UltraWave>)>,
    },
    Phantom(PhantomData<P>),
    #[cfg(test)]
    ToFabric(UltraWave),
    #[cfg(test)]
    GetSkel(oneshot::Sender<StarSkel<P>>)
}

pub struct StarTx<P> where P: Platform{
    gravity_well_tx: mpsc::Sender<UltraWave>,
    traverse_to_next: mpsc::Sender<Traversal<UltraWave>>,
    inject_tx: mpsc::Sender<TraversalInjection>,
    call_rx: mpsc::Receiver<StarCall<P>>,
}

impl <P> StarTx<P> where P: Platform {
    pub fn new(point: Point) -> Self {
        let (gravity_well_tx, mut gravity_well_rx) = mpsc::channel(1024);
        let (inject_tx, mut inject_rx) = mpsc::channel(1024);
        let (traverse_to_next_tx, mut traverse_to_next_rx) = mpsc::channel(1024);

        let (call_tx, call_rx) = mpsc::channel(1024);

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(wave) = gravity_well_rx.recv().await {
                    let wave = HyperWave {
                        wave,
                        from: point.clone(),
                    };
                    call_tx.send(StarCall::FromHyperway{wave, tx: None}).await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(traversal) = traverse_to_next_rx.recv().await {
                    call_tx.send(StarCall::TraverseToNextLayer(traversal)).await;
                }
            });
        }

        {
            let call_tx = call_tx.clone();
            tokio::spawn(async move {
                while let Some(inject) = inject_rx.recv().await {
                    call_tx
                        .send(StarCall::LayerTraversalInjection(inject))
                        .await;
                }
            });
        }

        Self {
            gravity_well_tx,
            traverse_to_next: traverse_to_next_tx,
            inject_tx,
            call_rx,
        }
    }
}

#[derive(Clone)]
pub struct StarApi<P> where P: Platform {
    pub kind: StarSub,
    tx: mpsc::Sender<StarCall<P>>,
}

impl <P> StarApi<P> where P: Platform {
    pub fn new(kind: StarSub, tx: mpsc::Sender<StarCall<P>>) -> Self {
        Self { kind, tx }
    }

    pub async fn pre_init(&self) -> Result<(),MsgErr>{
        let (tx,mut rx) = oneshot::channel();
        self.tx.send(StarCall::PreInit(tx) ).await;
        rx.await?
    }

    pub async fn from_hyperway(&self, wave: HyperWave, results: bool ) -> Result<(),MsgErr> {
        match results {
            true => {
                let (tx,mut rx) = oneshot::channel();
                self.tx.send(StarCall::FromHyperway{wave, tx: Some(tx)}).await;
                rx.await?
            }
            false => {
                self.tx.send(StarCall::FromHyperway{wave, tx: None}).await;
                Ok(())
            }
        }
    }

    pub async fn traverse_to_next_layer(&self, traversal: Traversal<UltraWave>) {
        self.tx.send(StarCall::TraverseToNextLayer(traversal)).await;
    }

    pub async fn inject_traversal(&self, inject: TraversalInjection) {
        self.tx
            .send(StarCall::LayerTraversalInjection(inject))
            .await;
    }

    pub async fn stub(&self) -> Result<StarStub,MsgErr> {
       let(tx,rx) = oneshot::channel();
       self.tx.send(StarCall::Stub(tx)).await;
       Ok(rx.await?)
    }

    #[cfg(test)]
    pub async fn get_skel(&self) -> Result<StarSkel<P>,MsgErr> {
        let(tx,rx) = oneshot::channel();
        self.tx.send(StarCall::GetSkel(tx)).await;
        Ok(rx.await?)
    }


    #[cfg(test)]
    pub async fn to_fabric(&self, wave: UltraWave) {
        self.tx.send(StarCall::ToFabric(wave)).await;
    }
}



pub struct Star<P>
where
    P: Platform + 'static,
{
    skel: StarSkel<P>,
    star_rx: mpsc::Receiver<StarCall<P>>,
    drivers: Drivers<P>,
    injector: Port,
    mounts: HashMap<Point, StarMount>,
    forwarders: Vec<Point>,
    golden_path: DashMap<StarKey, StarKey>,
    hyperway_transmitter: ProtoTransmitter,
    surface: Port,
}

impl<P> Star<P>
where
    P: Platform,
{
    pub fn new(
        skel: StarSkel<P>,
        mut drivers: Drivers<P>,
        hyperway_tx: mpsc::Sender<UltraWave>,
    ) -> Result<StarApi<P>, MsgErr> {
        let star_driver_factory = Box::new(StarDriverFactory::new(skel.clone()));
        drivers.add(star_driver_factory)?;

        let mut forwarders = vec![];
        for (point,stub) in skel.adjacents.iter() {
            if stub.kind.is_forwarder() {
                forwarders.push(point.clone());
            }
        }

        let hyperway_router = Arc::new(TxRouter::new(hyperway_tx));
        let mut hyperway_transmitter = ProtoTransmitterBuilder::new( hyperway_router, skel.exchanger.clone() );
        hyperway_transmitter.agent = SetStrategy::Override(Agent::HyperUser);
        hyperway_transmitter.scope = SetStrategy::Override(Scope::Full);
        let hyperway_transmitter = hyperway_transmitter.build();

        let (star_tx, star_rx) = mpsc::channel(32 * 1024);
        let mut injector = skel.location().clone().push("injector").unwrap().to_port();
        injector.layer = Layer::Surface;

        let mut golden_path = DashMap::new();
        for con in skel.connections.iter() {
            golden_path.insert(con.key().clone(), con.key().clone());
        }

        let surface = skel.point.clone().to_port().with_layer(Layer::Surface);

        // test interceptors

        let kind = skel.kind.clone();
        {
            let star = Self {
                skel,
                star_rx,
                drivers,
                injector,
                mounts: HashMap::new(),
                golden_path,
                hyperway_transmitter,
                forwarders,
                surface,
            };
            star.start();
        }
        Ok(StarApi::new(kind, star_tx))
    }


    fn start(mut self) {
        tokio::spawn(async move {
            while let Some(call) = self.star_rx.recv().await {
                match call {
                    StarCall::PreInit(tx) => {
                        tx.send(self.pre_init().await);
                    }
                    StarCall::FromHyperway{wave, tx} => {
                        let result = self.from_hyperway(wave.wave).await.map_err(|e|e.to_cosmic_err());
                        if let Some(tx) = tx {
                            tx.send(result);
                        }
                    }
                    StarCall::TraverseToNextLayer(traversal) => {
                        self.traverse_to_next_layer(traversal).await;
                    }
                    StarCall::LayerTraversalInjection(inject) => {
                        self.start_layer_traversal(inject.wave, &inject.injector)
                            .await;
                    }
                    StarCall::CreateMount { agent, kind, tx } => {
                        self.create_mount(agent, kind, tx).await;
                    }
                    StarCall::Stub(tx) => {
                        tx.send(self.skel.stub());
                    }

                    StarCall::Phantom(_) => {
                        // phantom literally does nothing but hold the P in not test mode
                    }
                    #[cfg(test)]
                    StarCall::ToFabric(wave) => {
                        self.to_fabric(wave).await;
                    }
                    #[cfg(test)]
                    StarCall::GetSkel(tx) => {
                        tx.send(self.skel.clone());
                    }
                }
            }
        });
    }

    async fn pre_init(&self) -> Result<(),MsgErr> {
        self.drivers.init().await
    }

    // receive a wave from the hyperlane... this wave should always be
    // a Wave<Signal> of the SysMethod<Hop> which should in turn contain a SysMethod<Transport> Signal
    async fn from_hyperway(&self, wave: UltraWave) -> Result<(), P::Err> {

        #[cfg(test)]
        {
            let wave = wave.clone();
            self.skel.diagnostic_interceptors.from_hyperway.send(wave);
        }

        let mut transport = wave.unwrap_from_hop()?;
        transport.inc_hops();
        if transport.hops > 255 {
            return self.skel.err("transport signal exceeded max hops");
        }

        if transport.to.point == self.skel.point {
            let  transport= transport.to_ultra();
            // we are now going to send this transport down the layers to the StarCore
            // where it's contents will be unwrapped from transport and routed to the appropriate particle
            self.start_layer_traversal(transport, &self.injector).await
        } else {
            self.forward(transport).await
        }
    }

    /*

        let mut wave = wave.wave;

        wave.inc_hops();
        if wave.hops() > 255 {
            self.skel.logger.warn("wave exceeded the hop limit of 255!");
            return Ok(());
        }

        wave.add_to_history(self.skel.point.clone());

        if wave.to().is_match(&self.skel.point) {
            self.start_layer_traversal(wave, &self.injector).await;
        } else if wave.can_shard() {
            for (location, wave) in
                shard_ultrawave_by_location(wave,&self.skel.adjacents, &self.skel.registry)
                .await?
            {
                if location == self.skel.point {
                    self.start_layer_traversal(wave, &self.injector).await;
                } else {
                    fling_to_fabric(self, location, wave).await?;
                }
            }
        } else {
            let location = self
                .skel
                .registry
                .locate(&wave.to().clone().unwrap_single().point)
                .await
                .map_err(|e| e.to_cosmic_err())?
                .location;
            fling_to_fabric(self, location, wave).await?;
        }

        async fn fling_to_fabric<E>(
            star: &Star<E>,
            location: Point,
            wave: UltraWave,
        ) -> Result<(), MsgErr>
        where
            E: Platform,
        {
            let hop = star
                .find_next_hop(&StarKey::try_from(location)?)
                .await?
                .ok_or::<MsgErr>("expected a next hop".into())?
                .to_port();
            if !wave.has_visited(&hop.point) {
                let mut proto = DirectedProto::new();
                proto.kind(DirectedKind::Signal);
                proto.method(SysMethod::Hop);
                proto.fill(&wave);
                proto.agent(Agent::HyperUser);
                proto.scope(Scope::Full);
                proto.to(hop.to_recipients());
                proto.body(Substance::UltraWave(Box::new(wave)));
                let wave = proto.build()?.to_ultra();
                star.fabric_tx.send(wave).await;
            }
            Ok(())
        }

        Ok(())
    }

     */

    // send this transport signal towards it's destination
    async fn forward(&self, transport: Wave<Signal>) -> Result<(), P::Err> {
        if self.skel.kind.is_forwarder() {
            self.to_hyperway(transport).await
        } else {
            self.skel.err(format!("attempt to forward a transport on a non forwarding Star Kind: {}", self.skel.kind.to_string()))
        }
    }

    // sending a wave that is from and to a particle into the fabric...
    // here it will be wrapped into a transport for star to star delivery
    async fn to_fabric(&self, wave: UltraWave) -> Result<(), P::Err> {
println!("to_fabric!!!");

        #[cfg(test)]
        self.skel.diagnostic_interceptors.to_fabric.send(wave.clone());

        let waves = shard_ultrawave_by_location(wave, &self.skel.adjacents, &self.skel.registry).await?;
        for (to,wave) in waves {
            let mut transport = wave.wrap_in_transport(self.surface.clone(),to.to_port());
            transport.from(self.skel.point.clone().to_port());
            let transport = transport.build()?;
            let transport = transport.to_signal()?;
println!("to_hyperway!");
            self.to_hyperway(transport).await?;
        }
        Ok(())
    }

    // send this transport signal into the hyperway
    // wrap the transport into a hop to go to one and only one star
    async fn to_hyperway(&self, transport: Wave<Signal>) -> Result<(), P::Err> {
println!("to_hyperway: {}", transport.to.to_string());
        if self.skel.point == transport.to.point {
            // it's a bit of a strange case, but even if this star is sending a transport message
            // to itself, it still makes use of the Hyperway Interchange, which will bounce it back
            // The reason for this is that it is the Hyperway that handles things like Priority, Urgency
            // and hopefully in the future durability, whereas within the star itself all waves are
            // treated equally.
            self.hyperway_transmitter.direct(transport.wrap_in_hop(self.surface.clone(), self.skel.point.clone().to_port()) ).await?;
            Ok(())
        }
        else if self.skel.adjacents.contains_key(&transport.to.point) {
            let to = transport.to.clone();
            self.hyperway_transmitter.direct(transport.wrap_in_hop( self.surface.clone(), to) ).await?;
            Ok(())
        } else if self.forwarders.len() == 1 {
            let to = self.forwarders.first().unwrap().clone().to_port();
            self.hyperway_transmitter.direct(transport.wrap_in_hop(self.surface.clone(), to) ).await?;
            Ok(())
        } else if self.forwarders.is_empty() {
            self.skel.err("this star needs to send a transport to a non-adjacent star yet does not have any adjacent forwarders")
        } else {
            unimplemented!("need to now send out a ripple search for the star being transported to")
        }
    }

    async fn start_layer_traversal(&self, wave: UltraWave, injector: &Port) -> Result<(), P::Err> {
        let record = match self
            .skel
            .registry
            .locate(&wave.to().clone().unwrap_single().to_point())
            .await
        {
            Ok(record) => record,
            Err(err) => {
                self.skel.logger.error(err.to_string());
                return Err(err);
            }
        };

        let location = record.location.clone();
        let plan = record.details.stub.kind.wave_traversal_plan().clone();

        let mut dest = None;
        let mut dir = TraversalDirection::Core;
        // determine layer destination. A dest of None will send all the way to the Fabric or Core
        if location == *self.skel.location() {
            // now we check if we are doing an inter point delivery (from one layer to another in the same Particle)
            if wave.to().clone().unwrap_single().point == wave.from().point {
                // it's the SAME point, so the to layer becomes our dest
                dest.replace(wave.to().clone().unwrap_single().layer);

                // make sure we have this layer in the plan
                if !plan.has_layer(&wave.to().clone().unwrap_single().layer) {
                    return self.skel.err("attempt to send wave to layer that the recipient Kind does not have in its traversal plan");
                }

                // dir is from inject_layer to dest
                dir = match TraversalDirection::new(
                    &injector.layer,
                    &wave.to().clone().unwrap_single().layer,
                ) {
                    Ok(dir) => dir,
                    Err(_) => {
                        // looks like we are already on the dest layer...
                        // that means it doesn't matter what the TraversalDirection is
                        TraversalDirection::Fabric
                    }
                }
            } else {
                // if this wave was injected by the from Particle, then we need to first
                // traverse towards the fabric
                if injector.point == wave.from().point {
                    dir = TraversalDirection::Fabric;
                } else {
                    // if this was injected by something else (like the Star)
                    // then it needs to traverse towards the Core
                    dir = TraversalDirection::Core;
                    // and dest will be the to layer
                    dest.replace(wave.to().clone().unwrap_single().layer);
                }
            }
        } else {
            // location is outside of this Star, so dest is None and direction if Fabric
            dir = TraversalDirection::Fabric;
            dest = None;
        }

        let logger = self
            .skel
            .logger
            .point(wave.to().clone().unwrap_single().to_point());
        let logger = logger.span();
        let to = wave.to().clone().unwrap_single();

        let point = if *injector == self.injector {
            // if injected by the star then the destination is the point that this traversal belongs to
            to.clone().to_point()
        } else {
            // if injected by any other point then the injector is the point that this traversal belongs to
            injector.clone().to_point()
        };

        let mut traversal = Traversal::new(
            wave,
            record,
            location,
            injector.layer.clone(),
            logger,
            dir,
            dest,
            to,
            point,
        );

        // in the case that we injected into a layer that is not part
        // of this plan, we need to send the traversal to the next layer
        if !plan.has_layer(&injector.layer) {
            traversal.next();
        }

        // alright, let's visit the injection layer first...
        self.visit_layer(traversal).await;
        Ok(())
    }

    async fn visit_layer(&self, traversal: Traversal<UltraWave>) -> Result<(), MsgErr> {
        if traversal.is_directed() && self.skel.state.topic.contains_key(&traversal.to) {
            let topic = self.skel.state.find_topic(&traversal.to, traversal.from());
            match topic {
                None => {
                    // send some sort of Not_found
                    /*
                    let mut traversal = traversal.unwrap_directed();
                    let mut traversal = traversal.with(traversal.not_found());
                    traversal.reverse();
                    let traversal = traversal.wrap();
                    self.traverse_to_next(traversal).await;

                     */
                    return Err(MsgErr::not_found());
                }
                Some(result) => {
                    match result {
                        Ok(topic_handler) => {
                            let transmitter =
                                LayerInjectionRouter::new(self.skel.clone(), traversal.to.clone());
                            let transmitter = ProtoTransmitter::new(
                                Arc::new(transmitter),
                                self.skel.exchanger.clone(),
                            );
                            let to = traversal.to.clone();
                            let directed = traversal.unwrap_directed().payload;
                            let ctx =
                                RootInCtx::new(directed, to, self.skel.logger.span(), transmitter);

                            topic_handler.handle(ctx).await;
                        }
                        Err(err) => {
                            // some some 'forbidden' error message sending towards_core...
                        }
                    }
                }
            }
        } else {
            match traversal.layer {
                Layer::Field => {
                    let field = FieldEx::new(
                        traversal.point.clone(),
                        self.skel.clone(),
                        self.skel.state.find_field(&traversal.to)?,
                        traversal.logger.clone(),
                    );
                    field.visit(traversal).await;
                }
                Layer::Shell => {
                    let shell = ShellEx::new(
                        self.skel.clone(),
                        self.skel.state.find_shell(&traversal.to)?,
                    );
                    shell.visit(traversal).await;
                }
                Layer::Driver => {
                    self.drivers.visit(traversal).await;
                }
                _ => {
                    self.skel.logger.warn("attempt to traverse wave in the inner layers which the Star does not manage");
                }
            }
        }
        Ok(())
    }

    async fn traverse_to_next_layer(&self, mut traversal: Traversal<UltraWave>) {
        if traversal.dest.is_some() && traversal.layer == *traversal.dest.as_ref().unwrap() {
            self.visit_layer(traversal).await;
            return;
        }

        let next = traversal.next();
        match next {
            None => match traversal.dir {
                TraversalDirection::Fabric => {
                    self.skel.gravity_well_tx.send(traversal.payload);
                }
                TraversalDirection::Core => {
                    self.skel
                        .logger
                        .warn("should not have traversed a wave all the way to the core in Star");
                }
            },
            Some(_) => {
                self.visit_layer(traversal).await;
            }
        }
    }

    async fn create_mount(
        &mut self,
        agent: Agent,
        kind: MountKind,
        tx: oneshot::Sender<(mpsc::Sender<UltraWave>, mpsc::Receiver<UltraWave>)>,
    ) -> Result<(), P::Err> {
        let point = self.skel.point.clone().push("controls").unwrap();
        let index = self.skel.registry.sequence(&point).await?;
        let point = point.push(format!("control-{}", index)).unwrap();
        let registration = Registration {
            point: point.clone(),
            kind: kind.kind(),
            registry: Default::default(),
            properties: Default::default(),
            owner: agent.clone().to_point(),
        };
        self.skel
            .registry
            .register(&registration)
            .await
            .map_err(|e| e.to_cosmic_err())?;
        self.skel
            .registry
            .assign(&point, self.skel.location())
            .await
            .map_err(|e| e.to_cosmic_err())?;

        let (in_mount_tx, mut in_mount_rx) = mpsc::channel(32 * 1024);
        let (out_mount_tx, out_mount_rx) = mpsc::channel(32 * 1024);

        tx.send((in_mount_tx, out_mount_rx));

        let inject_tx = self.skel.inject_tx.clone();
        {
            let point = point.clone();
            tokio::spawn(async move {
                let port = point.to_port().with_layer(Layer::Core);
                while let Some(wave) = in_mount_rx.recv().await {
                    let inject = TraversalInjection::new(port.clone(), wave);
                    inject_tx.send(inject).await;
                }
            });
        }

        let mount = StarMount {
            point: point.clone(),
            kind,
            tx: out_mount_tx,
        };

        self.mounts.insert(point, mount);
        Ok(())
    }

    async fn find_next_hop(&self, star_key: &StarKey) -> Result<Option<StarKey>, MsgErr> {
        if let Some(adjacent) = self.golden_path.get(star_key) {
            Ok(Some(adjacent.value().clone()))
        } else {
            let mut ripple = DirectedProto::new();
            ripple.kind(DirectedKind::Ripple);
            ripple.method(SysMethod::Search);
            ripple.body(Substance::Sys(Sys::Search(Search::Star(star_key.clone()))));
            ripple.bounce_backs = Some(BounceBacks::Count(self.skel.adjacents.len()));
            ripple.to(Recipients::Stars);
            let echoes: Echoes = self.skel.gravity_well_transmitter.direct(ripple).await?;

            let mut coalated = vec![];
            for echo in echoes {
                if let Substance::Sys(Sys::Discoveries(discoveries)) = &echo.core.body {
                    for discovery in discoveries.iter() {
                        coalated.push(StarDiscovery::new(
                            StarPair::new(
                                self.skel.key.clone(),
                                StarKey::try_from(echo.from.point.clone())?,
                            ),
                            discovery.clone(),
                        ));
                    }
                } else {
                    self.skel
                        .logger
                        .warn("unexpected reflected core substance from search echo");
                }
            }

            coalated.sort();

            match coalated.first() {
                None => Ok(None),
                Some(discovery) => {
                    let key = discovery.pair.not(&self.skel.key).clone();
                    self.golden_path.insert(star_key.clone(), key.clone());
                    Ok(Some(key))
                }
            }
        }
    }

    /*
    async fn re_ripple( &self, ripple: Wave<Ripple> ) -> Result<Vec<Echoes>,MsgErr> {
        let mut reflections: Vec<BoxFuture<Echoes>> = vec![];

        for (location, ripple) in ripple.shard_by_location(&self.skel.adjacents, &self.skel.registry ).await?
        {
           if !ripple.history.contains(&location) {
               let key = StarKey::try_from(location)?;
               let adjacent = self.find_next_hop(&key).await?.ok_or("could not find golden way")?.to_port();
               let mut wave = DirectedProto::new();
               wave.kind(DirectedKind::Signal);
               wave.to(adjacent);
               wave.method(SysMethod::Transport.into());
               wave.handling(ripple.handling.clone());
               wave.agent(Agent::HyperUser);
               wave.body(Substance::UltraWave(Box::new(ripple.to_ultra())));
               reflections.push(self.skel.gravity_well_transmitter.direct(wave).boxed());
           }
        }

        if reflections.is_empty() {
            return Ok(vec![]);
        }

        let echoes = join_all(reflections).await?;

        Ok(echoes)
    }
     */
}

pub struct StarMount {
    pub point: Point,
    pub kind: MountKind,
    pub tx: mpsc::Sender<UltraWave>,
}

#[derive(Clone)]
pub struct LayerInjectionRouter<P>
where
    P: Platform + 'static
{
    pub skel: StarSkel<P>,
    pub injector: Port,
}

impl<P> LayerInjectionRouter<P>
where
    P: Platform + 'static,
{
    pub fn new(skel: StarSkel<P>, injector: Port) -> Self {
        Self { skel, injector }
    }

    pub fn with(&self, injector: Port) -> Self {
        Self {
            skel: self.skel.clone(),
            injector,
        }
    }
}

#[async_trait]
impl<P> Router for LayerInjectionRouter<P>
where
    P: Platform
{
    async fn route(&self, wave: UltraWave) {
        let inject = TraversalInjection::new(self.injector.clone(), wave);
        self.skel.inject_tx.send(inject).await;
    }

    fn route_sync(&self, wave: UltraWave) {
        let inject = TraversalInjection::new(self.injector.clone(), wave);
        self.skel.inject_tx.try_send(inject);
    }
}

pub trait TopicHandler: Send + Sync + DirectedHandler {
    fn source_selector(&self) -> &PortSelector;
}

pub trait TopicHandlerSerde<T: TopicHandler> {
    fn serialize(&self, handler: T) -> Substance;
    fn deserialize(&self, ser: Substance) -> T;
}

impl StateApi {
    pub fn new(tx: mpsc::Sender<StateCall>) -> Self {
        Self {
            tx,
            layer_filter: None,
        }
    }

    pub fn with_layer(self, layer: Layer) -> Self {
        Self {
            tx: self.tx,
            layer_filter: Some(layer),
        }
    }
}

#[derive(Clone)]
pub struct StateApi {
    pub tx: mpsc::Sender<StateCall>,
    layer_filter: Option<Layer>,
}

impl StateApi {
    pub async fn get_state(&self, port: Port) -> Result<Option<Arc<RwLock<dyn State>>>, MsgErr> {
        if let Some(layer) = &self.layer_filter {
            if port.layer != *layer {
                return Err(MsgErr::forbidden_msg(format!(
                    "not allowed to get state from Port Layer {} try layer {}",
                    port.layer.to_string(),
                    layer.to_string()
                )));
            }
        }
        let (tx, rx) = oneshot::channel();
        self.tx.send(StateCall::Get { port, tx }).await;
        rx.await?
    }

    pub async fn put_state(&self, port: Port, state: Arc<RwLock<dyn State>>) -> Result<(), MsgErr> {
        if let Some(layer) = &self.layer_filter {
            if port.layer != *layer {
                return Err(MsgErr::forbidden_msg(format!(
                    "not allowed to put state on Port Layer {} try layer {}",
                    port.layer.to_string(),
                    layer.to_string()
                )));
            }
        }
        let (tx, rx) = oneshot::channel();
        self.tx.send(StateCall::Put { port, state, tx }).await;
        rx.await?
    }
}

pub enum StateCall {
    Get {
        port: Port,
        tx: oneshot::Sender<Result<Option<Arc<RwLock<dyn State>>>, MsgErr>>,
    },
    Put {
        port: Port,
        state: Arc<RwLock<dyn State>>,
        tx: oneshot::Sender<Result<(), MsgErr>>,
    },
}

#[derive(Clone)]
pub struct StarTemplate {
    pub key: StarKey,
    pub kind: StarSub,
    pub hyperway: Vec<StarCon>,
}



impl StarTemplate {
    pub fn new(key: StarKey, kind: StarSub) -> Self {
        Self {
            key,
            kind,
            hyperway: vec![],
        }
    }

    pub fn to_stub(&self) -> StarStub {
        StarStub::new( self.key.clone(), self.kind.clone() )
    }

    pub fn receive(&mut self, stub: StarStub) {
        self.hyperway.push(StarCon::Receive(stub));
    }

    pub fn connect(&mut self, stub: StarStub) {
        self.hyperway.push(StarCon::Connect(stub));
    }
}

#[derive(Clone)]
pub enum StarCon {
    Receive(StarStub),
    Connect(StarStub),
}

impl StarCon {
    pub fn stub(&self) -> &StarStub {
        match self {
            StarCon::Receive(stub) => stub,
            StarCon::Connect(stub) => stub
        }
    }

    pub fn key(&self) -> &StarKey {
        match self {
            StarCon::Receive(stub) => &stub.key,
            StarCon::Connect(stub) => &stub.key,
        }
    }

    pub fn kind(&self) -> &StarSub {
        match self {
            StarCon::Receive(stub) => &stub.kind,
            StarCon::Connect(stub) => &stub.kind,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct StarPair {
    pub a: StarKey,
    pub b: StarKey,
}

impl StarPair {
    pub fn new(a: StarKey, b: StarKey) -> Self {
        if a < b {
            Self { a, b }
        } else {
            Self { a: b, b: a }
        }
    }

    pub fn not(&self, this: &StarKey) -> &StarKey {
        if self.a == *this {
            &self.b
        } else {
            &self.a
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct StarDiscovery {
    pub pair: StarPair,
    pub discovery: Discovery,
}

impl Deref for StarDiscovery {
    type Target = Discovery;

    fn deref(&self) -> &Self::Target {
        &self.discovery
    }
}

impl StarDiscovery {
    pub fn new(pair: StarPair, discovery: Discovery) -> Self {
        Self { pair, discovery }
    }
}

impl Ord for StarDiscovery {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.discovery.hops != other.discovery.hops {
            self.discovery.hops.cmp(&other.discovery.hops)
        } else {
            self.pair.cmp(&other.pair)
        }
    }
}

impl PartialOrd for StarDiscovery {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.discovery.hops != other.discovery.hops {
            self.discovery.hops.partial_cmp(&other.discovery.hops)
        } else {
            self.pair.partial_cmp(&other.pair)
        }
    }
}

pub struct StarDriverFactory<P>
where
    P: Platform + 'static
{
    pub skel: StarSkel<P>,
}

impl<P> StarDriverFactory<P>
where
    P: Platform + 'static
{
    pub fn new(skel: StarSkel<P>) -> Self {
        Self { skel }
    }
}

impl<P> DriverFactory for StarDriverFactory<P>
where
    P: Platform + 'static
{
    fn kind(&self) -> Kind {
        Kind::Star(self.skel.kind.clone())
    }

    fn create(&self, driver_skel: DriverSkel) -> Box<dyn Driver> {
        Box::new(StarDriver::new(self.skel.clone(), driver_skel))
    }
}

#[derive(DirectedHandler)]
pub struct StarDriver<P>
where
    P: Platform + 'static,
{
    pub status: DriverStatus,
    pub star_skel: StarSkel<P>,
    pub driver_skel: DriverSkel,
}

impl<P> StarDriver<P>
where
    P: Platform
{
    pub fn new(star_skel: StarSkel<P>, driver_skel: DriverSkel) -> Self {
        Self {
            status: DriverStatus::Started,
            star_skel,
            driver_skel,
        }
    }

    async fn search_for_stars(&self, search: Search) -> Result<Vec<Discovery>, MsgErr> {
println!("Search for stars!");
        let mut ripple = DirectedProto::new();
        ripple.kind(DirectedKind::Ripple);
        ripple.method(SysMethod::Search);
        ripple.bounce_backs = Some(BounceBacks::Count(self.star_skel.adjacents.len()));
        ripple.body(Substance::Sys(Sys::Search(search)));
        ripple.to(Recipients::Stars);
        let echoes: Echoes = self
            .star_skel
            .gravity_well_transmitter
            .direct(ripple)
            .await?;

        let mut rtn = vec![];
        for echo in echoes {

println!("echo {}", echo.core.status.to_string());
            if let Substance::Sys(Sys::Discoveries(discoveries)) = echo.variant.core.body {
                for discovery in discoveries.vec.into_iter() {
                    rtn.push(discovery);
                }
            } else {
                self.star_skel
                    .logger
                    .warn("unexpected reflected core substance from search echo");
            }
        }

        Ok(rtn)
    }
}

#[async_trait]
impl<P> Driver for StarDriver<P>
where
    P: Platform,
{
    fn kind(&self) -> Kind {
        Kind::Star(self.star_skel.kind.clone())
    }

    async fn status(&self) -> DriverStatus {
        self.status.clone()
    }

    async fn lifecycle(&mut self, event: DriverLifecycleCall) -> Result<DriverStatus, MsgErr> {
        match event {
            DriverLifecycleCall::Init => {
                if self.status == DriverStatus::Ready {
                    return Ok(self.status.clone());
                }

                self.status = DriverStatus::Initializing;

                let mut discoveries = vec![];
                for discovery in self.search_for_stars(Search::Kinds).await? {
                    let discovery = StarDiscovery::new(
                        StarPair::new(self.star_skel.key.clone(), discovery.star_key.clone()),
                        discovery,
                    );
                    discoveries.push(discovery);
                }
                discoveries.sort();
            }
            DriverLifecycleCall::Shutdown => {}
        }

        Ok(self.status.clone())
    }

    fn ex(&self, point: &Point, state: Option<Arc<RwLock<dyn State>>>) -> Box<dyn Core> {
        Box::new(StarCore::new(
            self.star_skel.clone(),
            self.driver_skel.clone(),
        ))
    }

    async fn assign(
        &self,
        ctx: InCtx<'_, Assign>,
    ) -> Result<Option<Arc<RwLock<dyn State>>>, MsgErr> {
        Err("only allowed one Star per StarDriver".into())
    }
}

#[routes]
impl<P> StarDriver<P> where P: Platform {}

#[derive(DirectedHandler)]
pub struct StarCore<P>
where
    P: Platform + 'static
{
    pub star_skel: StarSkel<P>,
    pub driver_skel: DriverSkel,
}

impl<P> Core for StarCore<P> where P: Platform+'static {}

#[routes]
impl<P> StarCore<P>
where
    P: Platform + 'static,
{
    pub fn new(star_skel: StarSkel<P>, driver_skel: DriverSkel) -> Self {
        Self {
            star_skel,
            driver_skel,
        }
    }

    #[route("Sys<Assign>")]
    pub async fn handle_assign(&self, ctx: InCtx<'_, Sys>) -> Result<ReflectedCore, MsgErr> {
        if let Sys::Assign(assign) = ctx.input {
            self.driver_skel.assign(assign.clone()).await?;
            Ok(ReflectedCore::ok())
        } else {
            Err("expected Sys<Assign>".into())
        }
    }

    #[route("Sys<Transport>")]
    pub async fn transport(&self, ctx: InCtx<'_, UltraWave>) {
        self.star_skel
            .gravity_well_tx
            .send(ctx.wave().clone().to_ultra())
            .await;
    }

    #[route("Sys<Search>")]
    pub async fn handle_search_request(&self, ctx: InCtx<'_, Sys>) -> CoreBounce {
        fn reflect<'a, E>(core: &StarCore<E>, ctx: &'a InCtx<'a, Sys>) -> ReflectedCore
        where
            E: Platform,
        {
            let discovery = Discovery {
                star_kind: core.star_skel.kind.clone(),
                hops: ctx.wave().hops(),
                star_key: core.star_skel.key.clone(),
                kinds: core.star_skel.kinds.clone(),
            };
            let mut core = ReflectedCore::new();
            let mut discoveries = Discoveries::new();
            discoveries.push(discovery);
            core.body = Substance::Sys(Sys::Discoveries(discoveries));
            core.status = StatusCode::from_u16(200).unwrap();
            core
        }

        if let Sys::Search(search) = ctx.input {
            match search {
                Search::Star(star) => {
                    if self.star_skel.key == *star {
                        return CoreBounce::Reflected(reflect(self, &ctx));
                    }
                }
                Search::StarKind(kind) => {
                    if *kind == self.star_skel.kind {
                        return CoreBounce::Reflected(reflect(self, &ctx));
                    }
                }
                Search::Kinds => {
                    return CoreBounce::Reflected(reflect(self, &ctx));
                }
            }
            return CoreBounce::Absorbed;
        } else {
            return CoreBounce::Reflected(ctx.bad_request());
        }
    }
}

#[derive(Clone)]
pub struct StarWrangles {
    pub wrangles: Arc<DashMap<StarSub, RoundRobinWrangleSelector>>,
}

impl StarWrangles {
    pub fn new() -> Self {
        Self {
            wrangles: Arc::new(DashMap::new()),
        }
    }

    pub fn add(&self, discoveries: Vec<StarDiscovery>) {
        let mut map = HashMap::new();
        for discovery in discoveries {
            match map.get_mut(&discovery.star_kind) {
                None => {
                    map.insert(discovery.star_kind.clone(), vec![discovery]);
                }
                Some(discoveries) => discoveries.push(discovery),
            }
        }

        for (kind, discoveries) in map {
            let wrangler = RoundRobinWrangleSelector::new(kind, discoveries);
            self.wrangles.insert(wrangler.kind.clone(), wrangler);
        }
    }

    pub fn verify(&self, kinds: &[&StarSub]) -> Result<(), MsgErr> {
        for kind in kinds {
            if !self.wrangles.contains_key(*kind) {
                return Err(format!(
                    "star must be able to wrangle at least one {}",
                    kind.to_string()
                )
                .into());
            }
        }
        Ok(())
    }

    pub async fn wrangle(&self, kind: &StarSub) -> Result<StarKey, MsgErr> {
        self.wrangles
            .get(kind)
            .ok_or(format!(
                "could not find wrangles for kind {}",
                kind.to_string()
            ))?
            .value()
            .wrangle()
            .await
    }
}

pub struct RoundRobinWrangleSelector {
    pub kind: StarSub,
    pub stars: Vec<StarDiscovery>,
    pub index: Mutex<usize>,
    pub step_index: usize,
}

impl RoundRobinWrangleSelector {
    pub fn new(kind: StarSub, mut stars: Vec<StarDiscovery>) -> Self {
        stars.sort();
        let mut step_index = 0;
        let mut hops: i32 = -1;
        for discovery in &stars {
            if hops < 0 {
                hops = discovery.hops as i32;
            } else if discovery.hops as i32 > hops {
                break;
            }
            step_index += 1;
        }

        Self {
            kind,
            stars,
            index: Mutex::new(0),
            step_index,
        }
    }

    pub async fn wrangle(&self) -> Result<StarKey, MsgErr> {
        if self.stars.is_empty() {
            return Err(format!("cannot find wrangle for kind: {}", self.kind.to_string()).into());
        }

        let index = {
            let mut lock = self.index.lock().await;
            let index = *lock;
            lock.add(1);
            index
        };

        let index = index % self.step_index;

        if let Some(discovery) = self.stars.get(index) {
            Ok(discovery.discovery.star_key.clone())
        } else {
            Err(format!("cannot find wrangle for kind: {}", self.kind.to_string()).into())
        }
    }
}

pub async fn shard_ultrawave_by_location<E>(wave: UltraWave, adjacent: &HashMap<Point,StarStub>, registry: &Registry<E>) -> Result<HashMap<Point,UltraWave>,E::Err> where E: Platform {
    match wave{
        _ => {
println!("WAVE IS NOT A RIPPLE!");
            let mut map = HashMap::new();
            let point = &wave.to().unwrap_single().point;
println!("looking for: {}",point.to_string());
            let record = registry.locate(&wave.to().unwrap_single().point ).await;
println!("found?: {}", record.is_ok());
            let record = record?;
            map.insert(record.location, wave );
println!("located: {:?}",map);
            Ok(map)
        }
        UltraWave::Ripple(ripple) => {
println!("WAVE IS A RIPPLE!");
            let mut map = shard_ripple_by_location(ripple, adjacent,registry).await?;
            Ok(map.into_iter().map( |(point,ripple)| (point,ripple.to_ultra()) ).collect())
        }
    }
}


pub async fn shard_ripple_by_location<E>(ripple: Wave<Ripple>, adjacent: &HashMap<Point,StarStub>, registry: &Registry<E>) -> Result<HashMap<Point,Wave<Ripple>>,E::Err> where E: Platform{
    let mut map = HashMap::new();
    for (point,recipients) in shard_by_location(ripple.to.clone(),adjacent, registry).await? {
        let mut ripple = ripple.clone();
        ripple.variant.to = recipients;
        map.insert( point, ripple );
    }
    Ok(map)
}

pub async fn ripple_to_singulars<E>(ripple: Wave<Ripple>, adjacent: &HashSet<Point>, registry: &Registry<E>) -> Result<Vec<Wave<SingularRipple>>,E::Err> where E: Platform{
    let mut rtn = vec![];
    for port in to_ports(ripple.to.clone(),adjacent,registry).await? {
        let wave = ripple.as_single(port);
        rtn.push(wave)
    }
    Ok(rtn)
}


pub async fn shard_by_location<E>(
    recipients: Recipients,
    adjacent: &HashMap<Point,StarStub>,
    registry: &Registry<E>,
) -> Result<HashMap<Point, Recipients>, E::Err>  where E: Platform{
    match recipients{
        Recipients::Single(single) => {
            let mut map = HashMap::new();
            let record = registry.locate(&single.point).await?;
            map.insert(record.location, Recipients::Single(single));
            Ok(map)
        }
        Recipients::Multi(multi) => {
            let mut map:HashMap<Point,Vec<Port>> = HashMap::new();
            for p in multi {
                let record = registry.locate(&p).await?;
                if let Some(found) = map.get_mut(&record.location) {
                    found.push(p);
                } else {
                    map.insert(record.location, vec![p]);
                }
            }

            let mut map2 = HashMap::new();
            for (location,points) in map {
                map2.insert( location, Recipients::Multi(points));
            }
            Ok(map2)
        }
        Recipients::Watchers(_) => {
            let mut map = HashMap::new();
            // todo
            Ok(map)
        }
        Recipients::Stars => {
            let mut map = HashMap::new();
            for (star,_) in adjacent {
                map.insert( star.clone(), Recipients::Stars );
            }
            Ok(map)
        }
    }
}



pub async fn to_ports<E>(recipients: Recipients, adjacent: &HashSet<Point>, registry: &Registry<E>,) -> Result<Vec<Port>,E::Err> where E: Platform{
    match recipients{
        Recipients::Single(single) => Ok(vec![single]),
        Recipients::Multi(multi) => {
            Ok(multi.into_iter().map(|p| p).collect())
        }
        Recipients::Watchers(watch) => {
            unimplemented!();
        }
        Recipients::Stars => {
            let stars :Vec<Port> = adjacent.clone().into_iter().map(|p|p.to_port()).collect();
            Ok(stars)
        }
    }
}


#[derive(Clone)]
pub struct DiagnosticInterceptors<P> where P: Platform {
    pub from_hyperway: broadcast::Sender<UltraWave>,
    pub to_fabric: broadcast::Sender<UltraWave>,
    pub to_hyperway: broadcast::Sender<Wave<Signal>>,
    pub start_layer_traversal: broadcast::Sender<Traversal<UltraWave>>,
    pub err: broadcast::Sender<P::Err>
}

impl <P> DiagnosticInterceptors<P> where P: Platform {
    pub fn new() -> Self {
        let (from_hyperway,_) = broadcast::channel(1024);
        let (to_hyperway,_) = broadcast::channel(1024);
        let (to_fabric,_) = broadcast::channel(1024);
        let (start_layer_traversal,_) = broadcast::channel(1024);
        let (err,_) = broadcast::channel(1024);
        Self {
            from_hyperway,
            to_hyperway,
            to_fabric,
            start_layer_traversal,
            err,
        }
    }
}