use std::cell::Cell;

use std::collections::HashMap;
use std::convert::TryFrom;
use std::fs;
use std::path::Path;
use std::str::FromStr;

use std::sync::{Arc, Mutex};

use std::time::Duration;
use dashmap::DashMap;

use futures::future::join_all;
use futures::{FutureExt, StreamExt, TryFutureExt};
use mesh_portal::version::latest::frame::PrimitiveFrame;
use mesh_portal_api_server::Portal;
use mesh_portal::version::latest::id::{Point, Port};
use mesh_portal::version::latest::path;
use mesh_portal_tcp_client::PortalTcpClient;
use mesh_portal_tcp_common::{FrameReader, FrameWriter, PrimitiveFrameReader, PrimitiveFrameWriter};
use mesh_portal_tcp_server::{PointFactory, PortalTcpServer, TcpServerCall};

use serde::{Deserialize, Serialize};
use tokio::io::AsyncReadExt;
use tokio::net::{TcpListener, TcpStream};
use tokio::net::tcp::OwnedReadHalf;
use tokio::sync::oneshot;
use tokio::sync::{broadcast, mpsc};
use mesh_portal::version::latest::messaging::{Agent, Response};
use mesh_portal_versions::version::v0_0_1::id::id::ToPort;
use mesh_portal_versions::version::v0_0_1::wave::AsyncTransmitterWithAgent;
use crate::artifact::ArtifactRef;

use crate::cache::{ArtifactBundleSrc, ArtifactCaches, ProtoArtifactCachesFactory};
use crate::command::cli::CliServer;
use crate::constellation::{Constellation, ConstellationStatus};
use crate::endpoint::ServicesEndpoint;
use crate::error::Error;
use crate::file_access::FileAccess;
use crate::global::GlobalApi;

use crate::lane::{ClientSideTunnelConnector, LocalTunnelConnector, ProtoLaneEnd, ServerSideTunnelConnector, OnCloseAction};
use crate::logger::{Flags, Logger};
use crate::mechtron::portal_client::MechtronPortalClient;
use crate::message::StarlaneMessenger;

use crate::proto::{local_tunnels, ProtoStar, ProtoStarController, ProtoStarEvolution, ProtoStarKey, ProtoTunnel};
use crate::registry::{Registry, RegistryApi};

use crate::star::surface::SurfaceApi;
use crate::star::{ConstellationBroadcast, StarKind, StarStatus};
use crate::star::{Request, Star, StarCommand, StarController, StarInfo, StarKey, StarTemplateId};
use crate::star::core::particle::driver::mechtron::MechtronPortalServer;
use crate::starlane::api::StarlaneApi;
use crate::starlane::files::MachineFileSystem;
use crate::template::{
    ConstellationData, ConstellationLayout, ConstellationSelector, ConstellationTemplate,
    ConstellationTemplateHandle, MachineName, StarInConstellationTemplateHandle,
    StarInConstellationTemplateSelector, StarKeyConstellationIndexTemplate,
    StarKeySubgraphTemplate, StarKeyTemplate, StarSelector, StarTemplate, StarHandle,
};
use crate::user::HyperUser;
use crate::util::{AsyncHashMap, JwksCache};

pub mod files;
pub mod api;

lazy_static! {
//    pub static ref DATA_DIR: Mutex<String> = Mutex::new("data".to_string());
    pub static ref DEFAULT_PORT: usize = std::env::var("STARLANE_PORT").unwrap_or("4343".to_string()).parse::<usize>().unwrap_or(4343);

    pub static ref VERSION: VersionFrame = VersionFrame{ product: "Starlane".to_string(), version: "1.0.0".to_string() };
    pub static ref STARLANE_MECHTRON_PORT: usize = std::env::var("STARLANE_MECHTRON_PORT").unwrap_or("4345".to_string()).parse::<usize>().unwrap_or(4345);
    pub static ref STARLANE_DATA_DIR: String= std::env::var("STARLANE_DATA_DIR").unwrap_or("data".to_string());
    pub static ref STARLANE_CACHE_DIR: String = std::env::var("STARLANE_CACHE_DIR").unwrap_or("data".to_string());
}

#[derive(Clone)]
pub struct StarlaneMachine {
    pub tx: mpsc::Sender<StarlaneCommand>,
    run_complete_signal_tx: broadcast::Sender<()>,
    machine_filesystem: Arc<MachineFileSystem>,
    portals: Arc<DashMap<String,Portal>>,
    pub registry: RegistryApi,
    pub global: GlobalApi
}

impl StarlaneMachine {
    pub async fn new(name: MachineName) -> Result<Self, Error> {
        Self::new_with_artifact_caches(name, Option::None).await
    }

    pub async fn new_with_artifact_caches(
        name: MachineName,
        artifact_caches: Option<Arc<ProtoArtifactCachesFactory>>
    ) -> Result<Self, Error> {

        // presently we favor deletion since the persistence is not really working
        let delete_cache_on_start = std::env::var("STARLANE_DELETE_CACHE_ON_START").unwrap_or("true".to_string()).parse::<bool>().unwrap_or(true);
        let delete_data_on_start = std::env::var("STARLANE_DELETE_DATA_ON_START").unwrap_or("true".to_string()).parse::<bool>().unwrap_or(true);

        if delete_cache_on_start {
            match fs::remove_dir_all(STARLANE_CACHE_DIR.to_string() ) {
                Ok(_) => {}
                Err(err) => {
                    error!("{}",err.to_string());
                }
            }
        }
        if delete_data_on_start {
            fs::remove_dir_all(STARLANE_DATA_DIR.to_string() ).unwrap_or_default();
        }

        let runner = StarlaneMachineRunner::new_with_artifact_caches(name, artifact_caches)?;
        let tx = runner.command_tx.clone();
        let run_complete_signal_tx = runner.run();
        let registry = Registry::new().await?;
        let registry = Arc::new(registry);
        let global = GlobalApi::new( registry.clone(), Arc::new(StarlaneMessenger::new(tx.clone())) );
        let starlane = Self {
            tx: tx,
            run_complete_signal_tx: run_complete_signal_tx,
            machine_filesystem: Arc::new(MachineFileSystem::new()?),
            portals: Arc::new(DashMap::new()),
            registry,
            global
        };

        Result::Ok(starlane)
    }

    pub async fn cache( &self, artifact: &ArtifactRef)  -> Result<ArtifactCaches,Error> {
        let mut cache = self.get_proto_artifact_caches_factory().await?.create();
        cache.cache(vec![artifact.clone()]).await?;
        Ok(cache.to_caches().await?)
    }

    pub async fn get_proto_artifact_caches_factory(
        &self,
    ) -> Result<Arc<ProtoArtifactCachesFactory>,Error> {
        let (tx, rx) = oneshot::channel();
        self.tx
            .send(StarlaneCommand::GetProtoArtifactCachesFactory(tx))
            .await?;
        Ok(rx.await?.ok_or("expected proto artifact cache")?)
    }

    pub async fn start_mechtron_portal_server( &self ) -> Result<mpsc::Sender<TcpServerCall>,Error> {
        let (tx,rx) = oneshot::channel();
        self.tx
            .send(StarlaneCommand::StartMechtronPortal(tx))
            .await?;
        Ok(rx.await??)
    }

    pub fn machine_filesystem(&self) -> Arc<MachineFileSystem> {
        self.machine_filesystem.clone()
    }

    pub fn shutdown(&self) {
        let tx = self.tx.clone();
        tokio::spawn(async move {
            tx.send(StarlaneCommand::Shutdown).await;
        });
    }

    pub async fn create_constellation(
        &self,
        name: &str,
        layout: ConstellationLayout,
    ) -> Result<(), Error> {
        let name = name.to_string();
        let (tx, rx) = oneshot::channel();
        let create = ConstellationCreate {
            name,
            layout,
            tx,
            machine: self.clone(),
        };

        self.tx
            .send(StarlaneCommand::ConstellationCreate(create))
            .await?;
        rx.await?
    }

    pub async fn get_starlane_api(&self, port: Port) -> Result<StarlaneApi, Error> {
        let (tx, rx) = oneshot::channel();
        self.tx
            .send(StarlaneCommand::StarlaneApiSelectBest{port,tx})
            .await?;
        rx.await?
    }

    pub async fn listen(&self) -> Result<(), Error> {
        let command_tx = self.tx.clone();
        let (tx, rx) = oneshot::channel();
        command_tx.send(StarlaneCommand::Listen{ machine: self.clone(), tx }).await;
        rx.await?
    }

    pub async fn join(self) {
        let mut run_complete_signal_rx = self.run_complete_signal_tx.subscribe();
        join!(run_complete_signal_rx.recv());
    }
}

pub struct StarlaneMachineRunner {
    name: MachineName,
    pub command_tx: mpsc::Sender<StarlaneCommand>,
    command_rx: mpsc::Receiver<StarlaneCommand>,
    star_controllers: AsyncHashMap<StarInConstellationTemplateHandle, StarController>,
    //    star_core_ext_factory: Arc<dyn StarCoreExtFactory>,
    data_access: FileAccess,
    cache_access: FileAccess,
    pub logger: Logger,
    pub flags: Flags,
    pub artifact_caches: Option<Arc<ProtoArtifactCachesFactory>>,
    constellations: HashMap<String, Constellation>,
    port: usize,
    inner_flags: Arc<Mutex<Cell<StarlaneInnerFlags>>>,
}

impl StarlaneMachineRunner {
    pub fn new(machine: String, api: StarlaneMessenger) -> Result<Self, Error> {

        Self::new_with_artifact_caches(machine, Option::None)
    }

    pub fn new_with_artifact_caches(
        machine: String,
        artifact_caches: Option<Arc<ProtoArtifactCachesFactory>>,
    ) -> Result<Self, Error> {

        let (command_tx, command_rx) = mpsc::channel(32);
        let data_access = FileAccess::new(
            std::env::var("STARLANE_DATA_DIR").unwrap_or("data".to_string()),
        )?;
        let cache_access = FileAccess::new(
            std::env::var("STARLANE_CACHE_DIR").unwrap_or("cache".to_string()),
        )?;


        Ok(StarlaneMachineRunner {
            name: machine,
            star_controllers: AsyncHashMap::new(),
            command_tx,
            command_rx,
            //            star_core_ext_factory: Arc::new(ExampleStarCoreExtFactory::new() ),
            logger: Logger::new(),
            flags: Flags::new(),
            data_access,
            cache_access,
            artifact_caches: artifact_caches,
            port: DEFAULT_PORT.clone(),
            constellations: HashMap::new(),
            inner_flags: Arc::new(Mutex::new(Cell::new(StarlaneInnerFlags::new()))),
        })
    }

    pub async fn get_starlane_api(&self, port: Port ) -> Result<StarlaneApi, Error> {
        let messenger = StarlaneMessenger::new(
            self.command_tx.clone()
        );
        let messenger = AsyncTransmitterWithAgent::new(Agent::Anonymous, port, Arc::new(messenger)  );
        let api = StarlaneApi::new(messenger);
        Ok(api)
    }

    async fn get_best_surface_api(&self) -> Result<SurfaceApi, Error> {

        let map = match self.star_controllers.clone().into_map().await {
            Ok(map) => map,
            Err(err) => {
                return Err(err);
            }
        };
        if map.is_empty() {
            return Err(
                "ERROR: cannot create StarlaneApi: no StarControllers available."
                    .into(),
            );
        }
        let values: Vec<StarController> =
            map.into_iter().map(|(_k, v)| v).collect();

        let mut best = Option::None;

        for star_ctrl in values {
            let info = star_ctrl.get_star_info().await.unwrap().unwrap();
            if best.is_none() {
                best = Option::Some((info, star_ctrl));
            } else {
                let (prev_info, _) = best.as_ref().unwrap();
                match info.kind {
                    StarKind::Relay => {
                        best = Option::Some((info, star_ctrl));
                    }
                    StarKind::Client => {
                        if prev_info.kind != StarKind::Relay {
                            best = Option::Some((info, star_ctrl));
                        }
                    }
                    _ => {}
                }
            }
        }

        let (info, star_ctrl) = best.unwrap();

        Ok(star_ctrl.surface_api.clone())
    }


    pub fn run(mut self) -> broadcast::Sender<()> {
        let command_tx = self.command_tx.clone();
        tokio::spawn(async move {
            let mut shutdown_rx = crate::util::SHUTDOWN_TX.subscribe();
            shutdown_rx.recv().await;
            command_tx.try_send(StarlaneCommand::Shutdown);
        });

        let (run_complete_signal_tx, _) = broadcast::channel(1);
        let run_complete_signal_tx_rtn = run_complete_signal_tx.clone();

        tokio::spawn(async move {

            while let Option::Some(command) = self.command_rx.recv().await {
                match command {
                    StarlaneCommand::Request { request, tx } => {
                        match self.get_best_surface_api().await {
                            Ok(surface_api) => {
                                tokio::spawn( async move {
                                    tx.send(surface_api.exchange(request).await);
                                });
                            }
                            Err(err) => {
                                error!("{}",err.to_string());
                                tx.send(request.status(503));
                            }
                        }
                    }
                    StarlaneCommand::ConstellationCreate(command) => {
                        let result = self
                            .constellation_create(command.layout, command.name, command.machine)
                            .await;

                        //sleep(Duration::from_secs(10)).await;
                        if let Err(error) = &result {
                            error!("CONSTELLATION CREATE ERROR: {}", error.to_string());
                        }
                        command.tx.send(result);
                    }
                    StarlaneCommand::StarlaneApiSelectBest{ port, tx} =>{
                        tx.send(self.get_starlane_api(port).await);
                    }
                    StarlaneCommand::Shutdown => {
                        let listening = {
                            let mut inner_flags = self.inner_flags.lock().unwrap();
                            let mut_flags = inner_flags.get_mut();
                            mut_flags.shutdown = true;
                            mut_flags.listening
                        };

                        if listening {
                            Self::unlisten(self.inner_flags.clone(), self.port.clone());
                        }

                        for (_, star_ctrl) in self
                            .star_controllers
                            .clone()
                            .into_map()
                            .await
                            .unwrap_or(Default::default())
                        {
                            star_ctrl.star_tx.try_send(StarCommand::Shutdown);
                        }

                        self.star_controllers.clear();
                        self.command_rx.close();
                        break;
                    }
                    StarlaneCommand::Listen{machine, tx} => {
                        self.listen(machine, tx);
                    }

                    StarlaneCommand::GetProtoArtifactCachesFactory(tx) => {
                        match self.artifact_caches.as_ref() {
                            None => {
                                tx.send(Option::None);
                            }
                            Some(caches) => {
                                tx.send(Option::Some(caches.clone()));
                            }
                        }
                    }
                    StarlaneCommand::StartMechtronPortal(tx) => {
                        self.start_mechtron_portal_server(tx).await;
                    }
                }
            }
            run_complete_signal_tx.send(());
        });
        run_complete_signal_tx_rtn
    }


    async fn select_star_kind(&self, kind: &StarKind) -> Result<Option<StarController>, Error> {
        let map = self.star_controllers.clone().into_map().await?;
        let values: Vec<StarController> = map.into_iter().map(|(_k, v)| v).collect();

        for star_ctrl in values {
            let info = star_ctrl
                .get_star_info()
                .await?
                .ok_or("expected StarInfo")?;
            if info.kind == *kind {
                return Ok(Option::Some(star_ctrl));
            }
        }

        Ok(Option::None)
    }

    async fn constellation_create(
        &mut self,
        layout: ConstellationLayout,
        name: String,
        starlane_machine: StarlaneMachine,
    ) -> Result<(), Error> {
        if self.constellations.contains_key(&name) {
            return Err(format!(
                "constellation named '{}' already exists in this StarlaneMachine.",
                name
            )
            .into());
        }

        let mut constellation = Constellation::new(name.clone());
        let mut evolve_rxs = vec![];
        let (constellation_broadcaster, _) = broadcast::channel(16);

        for star_template in layout.template.stars.clone() {
            constellation.stars.push(star_template.clone());

            let star_template_id =
                StarInConstellationTemplateHandle::new(name.clone(), star_template.handle.clone());

            let machine = layout
                .handles_to_machine
                .get(&star_template.handle)
                .ok_or(format!(
                    "expected machine mapping for star template handle: {}",
                    star_template.handle.to_string()
                ))?;
            if self.name == *machine {
                let star_key = StarKey::new(&name, &star_template.handle ) ;
                // hacking to ProtoStarKey so we don't have to trouble with fixing the old setup yet
                let proto_star_key = ProtoStarKey::Key(star_key.clone());

                let (evolve_tx, evolve_rx) = oneshot::channel();
                evolve_rxs.push(evolve_rx);

                let (star_tx, star_rx) = mpsc::channel(1024);
                let (surface_tx, surface_rx) = mpsc::channel(1024);
                let surface_api = SurfaceApi::new(surface_tx);

                let star_ctrl = StarController {
                    star_tx: star_tx.clone(),
                    surface_api: surface_api.clone(),
                };
                self.star_controllers.put(star_template_id, star_ctrl).await;

                if self.artifact_caches.is_none() {
                    let messenger = Arc::new(StarlaneMessenger::new(self.command_tx.clone()));
                    let messenger = AsyncTransmitterWithAgent::new(Agent::Anonymous, star_key.into(), messenger );
                    let api = StarlaneApi::new(messenger);

                    let caches = Arc::new(ProtoArtifactCachesFactory::new(
                        ArtifactBundleSrc::STARLANE_API(api),
                        self.cache_access.clone(),
                        starlane_machine.clone()
                    )?);
                    self.artifact_caches = Option::Some(caches);
                }

                let (proto_star, _star_ctrl) = ProtoStar::new(
                    proto_star_key.clone(),
                    star_template.kind.clone(),
                    star_tx.clone(),
                    star_rx,
                    surface_api,
                    surface_rx,
                    self.data_access.clone(),
                    constellation_broadcaster.subscribe(),
                    self.flags.clone(),
                    self.logger.clone(),
                    starlane_machine.clone(),
                );

                tokio::spawn(async move {
                    let star = proto_star.evolve().await;
                    if let Ok(star) = star {
                        let key = star.star_key().clone();

                        let star_tx = star.star_tx();
                        let surface_api = star.surface_api();
                        tokio::spawn(async move {
                            star.run().await;
                        });
                        evolve_tx.send(ProtoStarEvolution {
                            star: key.clone(),
                            controller: StarController {
                                star_tx,
                                surface_api,
                            },
                        });
                        /*
                        println!(
                            "created star: {:?} key: {}",
                            &star_template.kind,
                            &key.to_string()
                        );

                         */
                    } else {
                        eprintln!("experienced serious error could not evolve the proto_star");
                    }
                });
            } else {
                println!(
                    "skipping star not hosted on this machine: {}",
                    star_template.handle.to_string()
                )
            }
        }

        // now connect the LANES
        let mut proto_lane_evolution_rxs = vec![];
        for star_template in &layout.template.stars {
            let machine = layout
                .handles_to_machine
                .get(&star_template.handle)
                .ok_or(format!(
                    "expected machine mapping for star template handle: {}",
                    star_template.handle.to_string()
                ))?;
            let local_star =
                StarInConstellationTemplateHandle::new(name.clone(), star_template.handle.clone());
            if self.name == *machine {
                for lane in &star_template.lanes {
                    match &lane.star_selector.constellation {
                        ConstellationSelector::Local => {
                            let second_star = constellation
                                .select(lane.star_selector.star.clone())
                                .ok_or("cannot select star from local constellation")?
                                .clone();
                            let second_star = StarInConstellationTemplateHandle::new(
                                name.clone(),
                                second_star.handle,
                            );
                            let mut evolution_rxs =
                                self.add_local_lane(local_star.clone(), second_star).await?;
                            proto_lane_evolution_rxs.append(&mut evolution_rxs);
                        }
                        ConstellationSelector::Named(constellation_name) => {
                            let constellation = self
                                .constellations
                                .get(constellation_name)
                                .ok_or(format!(
                                "cannot select constellation named '{}' on this StarlaneMachine",
                                constellation_name
                            ))?;
                            let second_star = constellation
                                .select(lane.star_selector.star.clone())
                                .ok_or(format!(
                                    "cannot select star from constellation {}",
                                    constellation_name
                                ))?
                                .clone();
                            let second_star = StarInConstellationTemplateHandle::new(
                                constellation.name.clone(),
                                second_star.handle,
                            );
                            let mut evolution_rxs =
                                self.add_local_lane(local_star.clone(), second_star).await?;
                            proto_lane_evolution_rxs.append(&mut evolution_rxs);
                        }
                    }
                }
            }
        }

        let proto_lane_evolutions =
            join_all(proto_lane_evolution_rxs.iter_mut().map(|x| x.recv())).await;

        for result in proto_lane_evolutions {
            result??;
        }

        // announce that the local constellation is now complete
        constellation_broadcaster.send(ConstellationBroadcast::Status(
            ConstellationStatus::Assembled,
        ));

        let evolutions = join_all(evolve_rxs).await;

        for evolve in evolutions {
            if let Ok(evolve) = evolve {
                evolve.controller.surface_api.init();
            } else if let Err(error) = evolve {
                return Err(error.to_string().into());
            }
        }

        let mut ready_futures = vec![];
        for star_template in &layout.template.stars {
            let machine = layout
                .handles_to_machine
                .get(&star_template.handle)
                .ok_or(format!(
                    "expected machine mapping for star template handle: {}",
                    star_template.handle.to_string()
                ))?;
            if self.name == *machine {
                let local_star = StarInConstellationTemplateHandle::new(
                    name.clone(),
                    star_template.handle.clone(),
                );
                let star_ctrl =
                    self.star_controllers
                        .get(local_star.clone())
                        .await?
                        .ok_or(format!(
                            "expected star controller: {}",
                            local_star.to_string()
                        ))?;
                let (tx, rx) = oneshot::channel();
                star_ctrl
                    .star_tx
                    .send(StarCommand::GetStatusListener(tx))
                    .await;
                let mut star_status_receiver = rx.await?;
                let (ready_status_tx, ready_status_rx) = oneshot::channel();
                tokio::spawn(async move {
                    while let Result::Ok(status) = star_status_receiver.recv().await {
                        if status == StarStatus::Ready {
                            ready_status_tx.send(());
                            break;
                        }
                    }
                });
                ready_futures.push(ready_status_rx);
            }
        }

        // wait for all stars to be StarStatus::Ready
        join_all(ready_futures).await;

        Ok(())
    }

    async fn start_mechtron_portal_server(&mut self, result_tx: oneshot::Sender<Result<mpsc::Sender<TcpServerCall>,Error>>) {
        {
            /*
            async fn process( runner: &mut StarlaneMachineRunner) -> Result<mpsc::Sender<TcpServerCall>,Error> {

                let starlane_api = runner.get_starlane_api().await?;
                let mut inner_flags = runner.inner_flags.lock().unwrap();
                let flags = inner_flags.get_mut();
                if let Some(serve_tx) = &flags.mechtron_portal_server {
                    Ok(serve_tx.clone() )
                } else {

                    pub struct HackedPointFactory{ };

                    impl PointFactory for HackedPointFactory {
                        fn point(&self) -> Point {
                            Point::from_str("<<HACK>>::portal").unwrap()
                        }
                    }


                    let server_tx = PortalTcpServer::new( STARLANE_MECHTRON_PORT.clone() , Box::new(MechtronPortalServer::new(starlane_api ) ), Arc::new( HackedPointFactory{}) );
                    flags.mechtron_portal_server = Option::Some(server_tx.clone());
                    Ok(server_tx)
                }
            }

             */
           // result_tx.send(process(self).await);
           unimplemented!()
        }
    }
    fn listen(&mut self, machine: StarlaneMachine, result_tx: oneshot::Sender<Result<(), Error>>) {
        let port = self.port.clone();
        tokio::spawn( async move {
            let result = ServicesEndpoint::new(machine, port).await;
println!("listening? {}", result.is_ok());
            result_tx.send(result);
        });
    }

    pub fn caches(&self) -> Result<Arc<ProtoArtifactCachesFactory>, Error> {
        Ok(self
            .artifact_caches
            .as_ref()
            .ok_or("expected caches to be set")?
            .clone())
    }

    async fn add_local_lane(
        &mut self,
        local: StarInConstellationTemplateHandle,
        second: StarInConstellationTemplateHandle,
    ) -> Result<Vec<broadcast::Receiver<Result<(), Error>>>, Error> {
        let (high, low) = crate::util::sort(local, second)?;

        let high_star_ctrl = {
            let high_star_ctrl = self.star_controllers.get(high.clone()).await?;
            match high_star_ctrl {
                None => {
                    return Err(format!(
                        "lane cannot construct. missing local star key: {}",
                        high.star.to_string()
                    )
                    .into());
                }
                Some(high_star_ctrl) => high_star_ctrl.clone(),
            }
        };

        let low_star_ctrl = {
            let low_star_ctrl = self.star_controllers.get(low.clone()).await?;
            match low_star_ctrl {
                None => {
                    return Err(format!(
                        "lane cannot construct. missing second star key: {}",
                        low.star.to_string()
                    )
                    .into());
                }
                Some(low_star_ctrl) => low_star_ctrl.clone(),
            }
        };
        self.add_local_lane_ctrl(high_star_ctrl, low_star_ctrl)
            .await
    }

    async fn add_local_lane_ctrl(
        &mut self,
        high_star_ctrl: StarController,
        low_star_ctrl: StarController,
    ) -> Result<Vec<broadcast::Receiver<Result<(), Error>>>, Error> {
        let high_lane = ProtoLaneEnd::new(Option::None, OnCloseAction::Remove );
        let low_lane = ProtoLaneEnd::new(Option::None, OnCloseAction::Remove );
        let rtn = vec![high_lane.get_evoltion_rx(), low_lane.get_evoltion_rx()];
        let connector = LocalTunnelConnector::new(&high_lane, &low_lane).await?;
        high_star_ctrl
            .star_tx
            .send(StarCommand::AddProtoLaneEndpoint(high_lane))
            .await?;
        low_star_ctrl
            .star_tx
            .send(StarCommand::AddProtoLaneEndpoint(low_lane))
            .await?;
        high_star_ctrl
            .star_tx
            .send(StarCommand::AddConnectorController(connector))
            .await?;

        Ok(rtn)
    }

    async fn add_server_side_lane_ctrl(
        &mut self,
        low_star_ctrl: StarController,
        stream: TcpStream,
        on_close_action: OnCloseAction
    ) -> Result<broadcast::Receiver<Result<(), Error>>, Error> {
        let low_lane = ProtoLaneEnd::new(Option::None, on_close_action  );
        let rtn = low_lane.get_evoltion_rx();

        let connector_ctrl = ServerSideTunnelConnector::new(&low_lane, stream).await?;

        low_star_ctrl
            .star_tx
            .send(StarCommand::AddProtoLaneEndpoint(low_lane))
            .await?;

        low_star_ctrl
            .star_tx
            .send(StarCommand::AddConnectorController(connector_ctrl))
            .await?;

        Ok(rtn)
    }

    async fn add_client_side_lane_ctrl(
        &mut self,
        star_ctrl: StarController,
        host_address: String,
        selector: StarInConstellationTemplateSelector,
        key_requestor: bool,
        on_close_action: OnCloseAction

    ) -> Result<broadcast::Receiver<Result<(), Error>>, Error> {
        let mut lane = ProtoLaneEnd::new(Option::None, on_close_action);
        lane.key_requestor = key_requestor;

        let rtn = lane.get_evoltion_rx();

        let connector = ClientSideTunnelConnector::new(&lane, host_address, selector).await?;

        star_ctrl
            .star_tx
            .send(StarCommand::AddProtoLaneEndpoint(lane))
            .await?;

        star_ctrl
            .star_tx
            .send(StarCommand::AddConnectorController(connector))
            .await?;

        Ok(rtn)
    }

    fn unlisten(inner_flags: Arc<Mutex<Cell<StarlaneInnerFlags>>>, port: usize) {
        {
            let mut flags = inner_flags.lock().unwrap();
            flags.get_mut().shutdown = true;
        }
        std::net::TcpStream::connect(format!("localhost:{}", port));
    }
}

impl Drop for StarlaneMachineRunner {
    fn drop(&mut self) {
        {
            let mut flags = self.inner_flags.lock().unwrap();

            let flags_mut = flags.get_mut();

            if !flags_mut.shutdown {
                warn!("dropping Starlane( {} ) unexpectedly", self.name);
            }

            if !flags_mut.listening {
                Self::unlisten(self.inner_flags.clone(), self.port.clone());
            }
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct VersionFrame {
    product: String,
    version: String,
}

#[derive(strum_macros::Display)]
pub enum StarlaneCommand {
    ConstellationCreate(ConstellationCreate),
    StarlaneApiSelectBest{port: Port, tx: oneshot::Sender<Result<StarlaneApi, Error>>},
    Listen{ machine: StarlaneMachine, tx: oneshot::Sender<Result<(), Error>> },
    GetProtoArtifactCachesFactory(oneshot::Sender<Option<Arc<ProtoArtifactCachesFactory>>>),
    StartMechtronPortal(oneshot::Sender<Result<mpsc::Sender<TcpServerCall>,Error>>),
    Request{ request: mesh_portal::version::latest::messaging::Request, tx: oneshot::Sender<mesh_portal::version::latest::messaging::Response>},
    Shutdown,
}

pub struct StarlaneApiRequestByKey {
    pub star: StarKey,
    pub tx: oneshot::Sender<StarlaneApi>,
}

pub struct StarlaneApiRequest {
    pub selector: StarSelector,
    pub tx: oneshot::Sender<StarlaneApi>,
}

impl StarlaneApiRequest {
    pub fn new(selector: StarSelector) -> (Self, oneshot::Receiver<StarlaneApi>) {
        let (tx, rx) = oneshot::channel();
        (
            Self {
                selector: selector,
                tx: tx,
            },
            rx,
        )
    }
}

pub struct ConstellationCreate {
    name: String,
    layout: ConstellationLayout,
    tx: oneshot::Sender<Result<(), Error>>,
    machine: StarlaneMachine,
}

impl ConstellationCreate {
    pub fn new(
        layout: ConstellationLayout,
        name: String,
        machine: StarlaneMachine,
    ) -> (Self, oneshot::Receiver<Result<(), Error>>) {
        let (tx, rx) = oneshot::channel();
        (
            ConstellationCreate {
                name: name,
                layout: layout,
                tx: tx,
                machine: machine,
            },
            rx,
        )
    }
}

pub enum StarAddress {
    Local,
}

#[derive(Clone)]
struct StarlaneInnerFlags {
    pub shutdown: bool,
    pub listening: bool,
    pub mechtron_portal_server: Option<mpsc::Sender<TcpServerCall>>
}

impl StarlaneInnerFlags {
    pub fn new() -> Self {
        Self {
            shutdown: false,
            listening: false,
            mechtron_portal_server: Option::None
        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct UsernameAndPasswordAuth {
    pub userbase: String,
    pub username: String,
    pub password: String
}

