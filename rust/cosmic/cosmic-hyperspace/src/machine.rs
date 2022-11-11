use std::collections::{HashMap, HashSet};
use std::future::Future;
use std::marker::PhantomData;
use std::process::Output;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use dashmap::DashMap;
use futures::future::{join_all, select_all, BoxFuture};
use futures::FutureExt;
use tokio::sync::broadcast::Receiver;
use tokio::sync::oneshot::error::RecvError;
use tokio::sync::watch::Ref;
use tokio::sync::{broadcast, mpsc, oneshot, watch};
use tracing::info;

use cosmic_hyperlane::{
    HyperClient, HyperConnectionDetails, HyperConnectionErr, HyperGate, HyperGateSelector,
    HyperRouter, Hyperway, HyperwayEndpoint, HyperwayEndpointFactory, HyperwayInterchange,
    HyperwayStub, InterchangeGate, LayerTransform, LocalHyperwayGateJumper,
    LocalHyperwayGateUnlocker, MountInterchangeGate, SimpleGreeter,
    TokenAuthenticatorWithRemoteWhitelist,
};
use cosmic_space::artifact::asynch::{ArtifactApi, ArtifactFetcher, ReadArtifactFetcher};
use cosmic_space::err::SpaceErr;
use cosmic_space::hyper::{InterchangeKind, Knock};
use cosmic_space::loc::{
    ConstellationName, Layer, MachineName, Point, StarHandle, StarKey, Surface, ToPoint, ToSurface,
};
use cosmic_space::log::{PointLogger, RootLogger};
use cosmic_space::particle::{Status, Stub};
use cosmic_space::settings::Timeouts;
use cosmic_space::substance::{Bin, Substance};
use cosmic_space::wave::core::cmd::CmdMethod;
use cosmic_space::wave::exchange::asynch::Exchanger;
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{Agent, DirectedProto, HyperWave, Pong, UltraWave, Wave};

use crate::err::HyperErr;
use crate::reg::{Registry, RegistryApi};
use crate::star::{HyperStar, HyperStarApi, HyperStarSkel, HyperStarTx, StarCon, StarTemplate};
use crate::{Cosmos, DriversBuilder};

#[derive(Clone)]
pub struct MachineApi<P>
where
    P: Cosmos,
{
    tx: mpsc::Sender<MachineCall<P>>,
}

impl<P> MachineApi<P>
where
    P: Cosmos,
{
    pub fn new(tx: mpsc::Sender<MachineCall<P>>) -> Self {
        Self { tx }
    }

    pub async fn endpoint_factory(
        &self,
        from: StarKey,
        to: StarKey,
    ) -> Result<Box<dyn HyperwayEndpointFactory>, P::Err> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.tx
            .send(MachineCall::EndpointFactory { from, to, rtn })
            .await;
        Ok(rtn_rx.await?)
    }

    pub async fn add_interchange(
        &self,
        kind: InterchangeKind,
        gate: Arc<dyn HyperGate>,
    ) -> Result<(), SpaceErr> {
        let (rtn, rtn_rx) = oneshot::channel();
        self.tx
            .send(MachineCall::AddGate { kind, gate, rtn })
            .await?;
        rtn_rx.await?
    }

    pub async fn knock(&self, knock: Knock) -> Result<HyperwayEndpoint, SpaceErr> {
        let (rtn, rtn_rx) = oneshot::channel();
        self.tx.send(MachineCall::Knock { knock, rtn }).await;
        rtn_rx.await?
    }

    pub fn terminate(&self) {
        self.tx.try_send(MachineCall::Terminate);
    }

    pub async fn wait_ready(&self) {
        let (tx, mut rx) = oneshot::channel();
        self.tx.send(MachineCall::WaitForReady(tx)).await;
        rx.await;
    }

    pub async fn await_termination(&self) -> Result<(), P::Err> {
        let (tx, mut rx) = oneshot::channel();
        self.tx.send(MachineCall::AwaitTermination(tx)).await;
        let mut rx = match rx.await {
            Ok(rx) => rx,
            Err(err) => {
                return Err(P::Err::new(err.to_string()));
            }
        };
        match rx.recv().await {
            Ok(result) => result,
            Err(err) => {
                return Err(P::Err::new(err.to_string()));
            }
        }
    }

    #[cfg(test)]
    pub async fn get_machine_star(&self) -> Result<HyperStarApi<P>, SpaceErr> {
        let (tx, mut rx) = oneshot::channel();
        self.tx.send(MachineCall::GetMachineStar(tx)).await;
        Ok(rx.await?)
    }

    #[cfg(test)]
    pub async fn get_star(&self, key: StarKey) -> Result<HyperStarApi<P>, SpaceErr> {
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.tx.send(MachineCall::GetStar { key, rtn }).await;
        rtn_rx.await?
    }
}

#[derive(Clone)]
pub struct MachineSkel<P>
where
    P: Cosmos,
{
    pub name: MachineName,
    pub cosmos: P,
    pub registry: Registry<P>,
    pub artifacts: ArtifactApi,
    pub logger: RootLogger,
    pub timeouts: Timeouts,
    pub api: MachineApi<P>,
    pub status_rx: watch::Receiver<MachineStatus>,
    pub status_tx: mpsc::Sender<MachineStatus>,
    pub machine_star: Surface,
    pub global: Surface,
}

pub struct Machine<P>
where
    P: Cosmos + 'static,
{
    pub skel: MachineSkel<P>,
    pub stars: Arc<HashMap<Point, HyperStarApi<P>>>,
    pub machine_star: HyperStarApi<P>,
    pub gate_selector: Arc<HyperGateSelector>,
    pub call_tx: mpsc::Sender<MachineCall<P>>,
    pub call_rx: mpsc::Receiver<MachineCall<P>>,
    pub termination_broadcast_tx: broadcast::Sender<Result<(), P::Err>>,
    pub logger: PointLogger,
}

impl<P> Machine<P>
where
    P: Cosmos + 'static,
{
    pub fn new(platform: P) -> MachineApi<P> {
        let (call_tx, call_rx) = mpsc::channel(1024);
        let machine_api = MachineApi::new(call_tx.clone());
        tokio::spawn(async move { Machine::init(platform, call_tx, call_rx).await });

        machine_api
    }

    async fn init(
        platform: P,
        call_tx: mpsc::Sender<MachineCall<P>>,
        call_rx: mpsc::Receiver<MachineCall<P>>,
    ) -> Result<MachineApi<P>, P::Err> {
        let template = platform.machine_template();
        let machine_name = platform.machine_name();
        let machine_api = MachineApi::new(call_tx.clone());
        let (mpsc_status_tx, mut mpsc_status_rx) = mpsc::channel(128);
        let (watch_status_tx, watch_status_rx) = watch::channel(MachineStatus::Init);
        tokio::spawn(async move {
            while let Some(status) = mpsc_status_rx.recv().await {
                watch_status_tx.send(status);
            }
        });

        let machine_star = StarKey::machine(machine_name.clone())
            .to_point()
            .to_surface()
            .with_layer(Layer::Gravity);
        let logger = platform.logger().point(machine_star.point.clone());
        let global = machine_star
            .point
            .push("global")
            .unwrap()
            .to_surface()
            .with_layer(Layer::Core);
        let skel = MachineSkel {
            name: machine_name.clone(),
            machine_star,
            registry: platform.global_registry().await?,
            artifacts: platform.artifact_hub(),
            logger: platform.logger(),
            timeouts: Timeouts::default(),
            cosmos: platform.clone(),
            api: machine_api.clone(),
            status_tx: mpsc_status_tx,
            status_rx: watch_status_rx,
            global,
        };

        let mut stars = HashMap::new();
        let mut gates = Arc::new(DashMap::new());
        let star_templates = template.with_machine_star(machine_name);

        for star_template in star_templates {
            let star_point = star_template.key.clone().to_point();
            let star_port = star_point.clone().to_surface().with_layer(Layer::Core);

            let drivers_point = star_point.push("drivers".to_string()).unwrap();
            let logger = skel.logger.point(drivers_point.clone());

            let mut star_tx: HyperStarTx<P> = HyperStarTx::new(star_point.clone());
            let star_skel =
                HyperStarSkel::new(star_template.clone(), skel.clone(), &mut star_tx).await;

            let mut drivers = platform.drivers_builder(&star_template.kind);

            let mut interchange =
                HyperwayInterchange::new(logger.push_point("interchange").unwrap());

            let star_hop = star_point.clone().to_surface().with_layer(Layer::Gravity);

            let mut hyperway = Hyperway::new(star_hop.clone(), Agent::HyperUser, logger.clone());
            hyperway.transform_inbound(Box::new(LayerTransform::new(Layer::Gravity)));

            let hyperway_endpoint = hyperway.hyperway_endpoint_far(None).await;
            interchange.add(hyperway).await;
            interchange.singular_to(star_hop.clone());

            let interchange = Arc::new(interchange);
            let auth = skel.cosmos.star_auth(&star_template.key)?;
            let greeter = SimpleGreeter::new(star_hop.clone(), star_port.clone());
            let gate: Arc<dyn HyperGate> = Arc::new(MountInterchangeGate::new(
                auth,
                greeter,
                interchange.clone(),
                logger.clone(),
            ));

            for con in star_template.connections.iter() {
                match con {
                    StarCon::Receiver(remote) => {
                        let star = remote
                            .key
                            .clone()
                            .to_point()
                            .to_surface()
                            .with_layer(Layer::Gravity);
                        let hyperway = Hyperway::new(star, Agent::HyperUser, logger.clone());
                        interchange.add(hyperway).await;
                    }
                    StarCon::Connector(remote) => {
                        let star = remote
                            .key
                            .clone()
                            .to_point()
                            .to_surface()
                            .with_layer(Layer::Gravity);
                        let hyperway = Hyperway::new(star, Agent::HyperUser, logger.clone());
                        interchange.add(hyperway).await;
                    }
                }
            }

            gates.insert(InterchangeKind::Star(star_template.key.clone()), gate);
            let star_api = HyperStar::new(
                star_skel.clone(),
                drivers,
                hyperway_endpoint,
                interchange.clone(),
                star_tx,
            )
            .await?;
            stars.insert(star_point.clone(), star_api);
        }

        let mut gate_selector = Arc::new(HyperGateSelector::new(gates));
        skel.cosmos.start_services(&gate_selector).await;
        let gate: Arc<dyn HyperGate> = gate_selector.clone();

        let (machine_point, machine_star) = stars
            .iter()
            .find(|(k, v)| v.kind == StarSub::Machine)
            .map(|(k, v)| (k.clone(), v.clone()))
            .expect("expected Machine Star");

        {
            let machine_api = skel.api.clone();
            ctrlc::set_handler(move || {
                machine_api.terminate();
            });
        }

        let logger = skel.logger.point(machine_point);

        let (term_tx, _) = broadcast::channel(1);
        let stars = Arc::new(stars);
        {
            let mut star_statuses_rx: Vec<watch::Receiver<Status>> =
                stars.values().map(|s| s.status_rx.clone()).collect();
            let status_tx = skel.status_tx.clone();
            let star_count = stars.len();
            tokio::spawn(async move {
                loop {
                    let mut readies = 0;
                    let mut inits = 0;
                    let mut panics = 0;
                    let mut fatals = 0;
                    for status_rx in star_statuses_rx.iter_mut() {
                        match status_rx.borrow().clone() {
                            Status::Unknown => {}
                            Status::Pending => {}
                            Status::Init => {
                                inits = inits + 1;
                            }
                            Status::Ready => {
                                readies = readies + 1;
                            }
                            Status::Paused => {}
                            Status::Resuming => {}
                            Status::Panic => {
                                panics = panics + 1;
                            }
                            Status::Fatal => {
                                fatals = fatals + 1;
                            }
                            Status::Done => {}
                        }
                    }

                    if readies == star_count {
                        status_tx.send(MachineStatus::Ready).await;
                    } else if fatals > 0 {
                        status_tx.send(MachineStatus::Fatal).await;
                    } else if panics > 0 {
                        status_tx.send(MachineStatus::Panic).await;
                    } else if inits > 0 {
                        status_tx.send(MachineStatus::Init).await;
                    }

                    let boxed_status_rx: Vec<BoxFuture<Result<(), watch::error::RecvError>>> =
                        star_statuses_rx
                            .iter_mut()
                            .map(|s| s.changed().boxed())
                            .collect();
                    select_all(boxed_status_rx).await;
                }
            });
        }

        let mut machine = Self {
            skel: skel.clone(),
            logger: logger.clone(),
            machine_star,
            stars,
            gate_selector,
            call_tx,
            call_rx,
            termination_broadcast_tx: term_tx,
        };

        /// SETUP ARTIFAC
        let factory = MachineApiExtFactory {
            machine_api: machine_api.clone(),
            logger: logger.clone(),
        };
        let exchanger = Exchanger::new(
            Point::from_str("artifact").unwrap().to_surface(),
            Timeouts::default(),
            logger.clone(),
        );
        let client =
            HyperClient::new_with_exchanger(Box::new(factory), Some(exchanger), logger.clone())
                .unwrap();

        let fetcher = Arc::new(ClientArtifactFetcher::new(client, skel.registry.clone()));
        skel.artifacts.set_fetcher(fetcher).await;

        machine.start().await;
        Ok(machine_api)
    }

    async fn init0(&self) {
        let logger = self.logger.span();
        let mut inits = vec![];
        for star in self.stars.values() {
            inits.push(star.init().boxed());
        }

        join_all(inits).await;
    }

    async fn start(mut self) -> Result<(), P::Err> {
        self.call_tx
            .send(MachineCall::Init)
            .await
            .unwrap_or_default();

        while let Some(call) = self.call_rx.recv().await {
            match call {
                MachineCall::Init => {
                    self.init0().await;
                }
                MachineCall::Terminate => {
                    self.termination_broadcast_tx.send(Ok(()));
                    return Ok(());
                }
                MachineCall::AwaitTermination(tx) => {
                    tx.send(self.termination_broadcast_tx.subscribe());
                }
                MachineCall::WaitForReady(rtn) => {
                    let mut status_rx = self.skel.status_rx.clone();
                    tokio::spawn(async move {
                        loop {
                            if MachineStatus::Ready == status_rx.borrow().clone() {
                                rtn.send(());
                                break;
                            }
                            match status_rx.changed().await {
                                Ok(_) => {}
                                Err(err) => {
                                    rtn.send(());
                                    break;
                                }
                            }
                        }
                    });
                }
                MachineCall::AddGate { kind, gate, rtn } => {
                    rtn.send(self.gate_selector.add(kind.clone(), gate));
                }
                MachineCall::Knock { knock, rtn } => {
                    let gate_selector = self.gate_selector.clone();
                    let logger = self.skel.logger.point(self.skel.machine_star.point.clone());
                    tokio::spawn(async move {
                        rtn.send(
                            logger
                                .result_ctx("MachineCall::Knock", gate_selector.knock(knock).await),
                        );
                    });
                }
                MachineCall::EndpointFactory { from, to, rtn } => {
                    let factory = Box::new(MachineHyperwayEndpointFactory::new(
                        from,
                        to,
                        self.call_tx.clone(),
                    ));
                    rtn.send(factory).unwrap_or_default();
                }
                #[cfg(test)]
                MachineCall::GetMachineStar(rtn) => {
                    rtn.send(self.machine_star.clone());
                }
                #[cfg(test)]
                MachineCall::GetStar { key, rtn } => {
                    rtn.send(
                        self.stars
                            .get(&key.to_point())
                            .ok_or(format!("could not find star: {}", key.to_string()).into())
                            .cloned(),
                    )
                    .unwrap_or_default();
                }
                #[cfg(test)]
                MachineCall::GetRegistry(rtn) => {
                    rtn.send(self.skel.registry.clone());
                }
            }

            self.termination_broadcast_tx
                .send(Err(P::Err::new("machine quit unexpectedly.")));
        }

        Ok(())
    }
}

pub enum MachineCall<P>
where
    P: Cosmos,
{
    Init,
    Terminate,
    AwaitTermination(oneshot::Sender<broadcast::Receiver<Result<(), P::Err>>>),
    WaitForReady(oneshot::Sender<()>),
    AddGate {
        kind: InterchangeKind,
        gate: Arc<dyn HyperGate>,
        rtn: oneshot::Sender<Result<(), SpaceErr>>,
    },
    Knock {
        knock: Knock,
        rtn: oneshot::Sender<Result<HyperwayEndpoint, SpaceErr>>,
    },
    EndpointFactory {
        from: StarKey,
        to: StarKey,
        rtn: oneshot::Sender<Box<dyn HyperwayEndpointFactory>>,
    },
    #[cfg(test)]
    GetMachineStar(oneshot::Sender<HyperStarApi<P>>),
    #[cfg(test)]
    GetStar {
        key: StarKey,
        rtn: oneshot::Sender<Result<HyperStarApi<P>, SpaceErr>>,
    },
    #[cfg(test)]
    GetRegistry(oneshot::Sender<Registry<P>>),
}

#[derive(Clone, Eq, PartialEq, strum_macros::Display)]
pub enum MachineStatus {
    Pending,
    Init,
    Ready,
    Panic,
    Fatal,
}

pub struct MachineTemplate {
    pub stars: Vec<StarTemplate>,
}

impl MachineTemplate {
    pub fn star_set(&self) -> HashSet<StarKey> {
        let mut rtn = HashSet::new();
        for star in self.stars.iter() {
            rtn.insert(star.key.clone());
        }
        rtn
    }

    pub fn with_machine_star(&self, machine: MachineName) -> Vec<StarTemplate> {
        let mut stars = self.stars.clone();
        let mut machine = StarTemplate::new(StarKey::machine(machine), StarSub::Machine);
        for star in stars.iter_mut() {
            star.connect(machine.to_stub());
            machine.receive(star.to_stub());
        }

        stars.push(machine);

        stars
    }
}

impl Default for MachineTemplate {
    fn default() -> Self {
        let constellation = "central".to_string();

        let mut central = StarTemplate::new(StarKey::central(), StarSub::Central);
        let mut nexus = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("nexus")),
            StarSub::Nexus,
        );
        let mut supe = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("super")),
            StarSub::Super,
        );
        let mut maelstrom = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("maelstrom")),
            StarSub::Maelstrom,
        );
        let mut scribe = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("scribe")),
            StarSub::Scribe,
        );
        let mut jump = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("jump")),
            StarSub::Jump,
        );
        let mut fold = StarTemplate::new(
            StarKey::new(&constellation, &StarHandle::name("fold")),
            StarSub::Fold,
        );

        nexus.receive(central.to_stub());
        nexus.receive(supe.to_stub());
        nexus.receive(maelstrom.to_stub());
        nexus.receive(scribe.to_stub());
        nexus.receive(jump.to_stub());
        nexus.receive(fold.to_stub());

        central.connect(nexus.to_stub());
        supe.connect(nexus.to_stub());
        maelstrom.connect(nexus.to_stub());
        scribe.connect(nexus.to_stub());
        jump.connect(nexus.to_stub());
        fold.connect(nexus.to_stub());

        let mut stars = vec![];
        stars.push(central);
        stars.push(nexus);
        stars.push(supe);
        stars.push(maelstrom);
        stars.push(scribe);
        stars.push(jump);
        stars.push(fold);

        Self { stars }
    }
}

pub struct MachineHyperwayEndpointFactory<P>
where
    P: Cosmos,
{
    from: StarKey,
    to: StarKey,
    call_tx: mpsc::Sender<MachineCall<P>>,
}

impl<P> MachineHyperwayEndpointFactory<P>
where
    P: Cosmos,
{
    pub fn new(from: StarKey, to: StarKey, call_tx: mpsc::Sender<MachineCall<P>>) -> Self {
        Self { from, to, call_tx }
    }
}

#[async_trait]
impl<P> HyperwayEndpointFactory for MachineHyperwayEndpointFactory<P>
where
    P: Cosmos,
{
    async fn create(
        &self,
        status_tx: mpsc::Sender<HyperConnectionDetails>,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
        let knock = Knock::new(
            InterchangeKind::Star(self.to.clone()),
            self.from
                .clone()
                .to_point()
                .to_surface()
                .with_layer(Layer::Gravity),
            Substance::Empty,
        );
        let (rtn, mut rtn_rx) = oneshot::channel();
        self.call_tx.send(MachineCall::Knock { knock, rtn }).await;
        tokio::time::timeout(Duration::from_secs(60), rtn_rx).await??
    }
}

pub struct MachineApiExtFactory<P>
where
    P: Cosmos,
{
    pub machine_api: MachineApi<P>,
    pub logger: PointLogger,
}

#[async_trait]
impl<P> HyperwayEndpointFactory for MachineApiExtFactory<P>
where
    P: Cosmos,
{
    async fn create(
        &self,
        status_tx: mpsc::Sender<HyperConnectionDetails>,
    ) -> Result<HyperwayEndpoint, SpaceErr> {
        let knock = Knock {
            kind: InterchangeKind::DefaultControl,
            auth: Box::new(Substance::Empty),
            remote: None,
        };
        self.logger
            .result_ctx("machine_api.knock()", self.machine_api.knock(knock).await)
    }
}

pub struct ClientArtifactFetcher<P>
where
    P: Cosmos,
{
    pub registry: Registry<P>,
    pub client: HyperClient,
}

impl<P> ClientArtifactFetcher<P>
where
    P: Cosmos,
{
    pub fn new(client: HyperClient, registry: Registry<P>) -> Self {
        Self { client, registry }
    }
}

#[async_trait]
impl<P> ArtifactFetcher for ClientArtifactFetcher<P>
where
    P: Cosmos,
{
    async fn stub(&self, point: &Point) -> Result<Stub, SpaceErr> {
        let record = self
            .registry
            .record(point)
            .await
            .map_err(|e| e.to_space_err())?;
        Ok(record.details.stub)
    }

    async fn fetch(&self, point: &Point) -> Result<Bin, SpaceErr> {
        let transmitter = self.client.transmitter_builder().await?.build();

        let mut wave = DirectedProto::ping();
        wave.method(CmdMethod::Read);
        wave.to(point.clone().to_surface().with_layer(Layer::Core));
        let pong: Wave<Pong> = transmitter.direct(wave).await?;

        pong.ok_or()?;

        if let Substance::Bin(bin) = pong.variant.core.body {
            Ok(bin)
        } else {
            Err("expecting Bin encountered some other substance when fetching artifact".into())
        }
    }
}
