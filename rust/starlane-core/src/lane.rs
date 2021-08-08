use std::cell::Cell;

use std::convert::TryInto;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};

use futures::FutureExt;

use lru::LruCache;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::tcp::{OwnedReadHalf, OwnedWriteHalf};
use tokio::net::TcpStream;
use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::{broadcast, mpsc};

use tokio::time::Duration;

use crate::error::Error;
use crate::frame::{Frame, StarPattern};

use crate::proto::{local_tunnels, ProtoTunnel};
use crate::star::{StarCommand, StarKey};

use crate::template::StarInConstellationTemplateSelector;
use crate::star::shell::lanes::LaneMuxerCall;

pub static STARLANE_PROTOCOL_VERSION: i32 = 1;
pub static LANE_QUEUE_SIZE: usize = 32;
pub type LaneKey = StarKey;

#[derive(Clone)]
pub struct OutgoingSide {
    pub out_tx: Sender<LaneCommand>,
}

pub struct IncomingSide {
    rx: Receiver<Frame>,
    tunnel_receiver_rx: Receiver<TunnelInState>,
    tunnel: TunnelInState,
}

impl IncomingSide {
    #[instrument]
    pub async fn recv(&mut self) -> Option<LaneMuxerCall> {
        loop {
            match &mut self.tunnel {
                TunnelInState::None => match self.tunnel_receiver_rx.recv().await {
                    None => {
                        return Option::Some(LaneMuxerCall::Frame(Frame::Close));
                    }
                    Some(tunnel) => {
                        self.tunnel = tunnel;
                    }
                },
                TunnelInState::In(tunnel) => match tunnel.rx.recv().await {
                    None => {
                        self.tunnel = TunnelInState::None;
                        return Option::Some(LaneMuxerCall::Frame(Frame::Close));
                    }
                    Some(frame) => {
                        return Option::Some(LaneMuxerCall::Frame(frame));
                    }
                },
            }
        }
    }
}

impl Debug for IncomingSide {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("IncomingSide")
    }
}

pub struct LaneMiddle {
    rx: Receiver<LaneCommand>,
    tx: Sender<Frame>,
    tunnel: TunnelOutState,
    queue: Vec<Frame>,
}

impl LaneMiddle {
    async fn die(&self, message: String) {
        eprintln!("{}", message.as_str());
    }

    pub async fn run(mut self) {
        while let Option::Some(command) = self.rx.recv().await {
            match command {
                LaneCommand::Tunnel(tunnel) => {
                    if let TunnelOutState::Out(tunnel) = &tunnel {
                        for frame in self.queue.drain(..) {
                            tunnel.tx.send(frame).await;
                        }
                    }
                    self.tunnel = tunnel;
                }
                LaneCommand::Frame(frame) => match &self.tunnel {
                    TunnelOutState::Out(tunnel) => {
                        tunnel.tx.send(frame).await;
                    }
                    TunnelOutState::None => {
                        self.queue.push(frame);
                    }
                },
                LaneCommand::Shutdown => {
                    if let TunnelOutState::Out(tunnel) = &self.tunnel {
                        tunnel.tx.send(Frame::Close).await;
                    }
                    self.rx.close();
                    break;
                }
            }
        }
        // need to signal to Connector that this lane is now DEAD
    }

    async fn process_command(&mut self, _command: Option<LaneCommand>) {}
}

pub enum LaneCommand {
    Tunnel(TunnelOutState),
    Frame(Frame),
    Shutdown,
}

pub struct Chamber<T> {
    pub holding: Option<T>,
}

impl<T> Chamber<T> {
    pub fn new() -> Self {
        Chamber {
            holding: Option::None,
        }
    }
}

pub enum LaneWrapper {
    Proto(LaneMeta<ProtoLaneEnd>),
    Lane(LaneMeta<LaneEnd>),
}

impl LaneWrapper {
    pub fn pattern(&self) -> StarPattern{
        match self {
            LaneWrapper::Proto(meta) => {
                meta.pattern.clone()
            }
            LaneWrapper::Lane(meta) => {
                meta.pattern.clone()
            }
        }
    }

    pub fn expect_proto_lane(self) -> LaneMeta<ProtoLaneEnd> {
        match self {
            LaneWrapper::Proto(lane) => lane,
            _ => {
                panic!("expected proto lane")
            }
        }
    }

    pub fn expect_lane(self) -> LaneMeta<LaneEnd> {
        match self {
            LaneWrapper::Lane(lane) => lane,
            _ => {
                panic!("expected proto lane")
            }
        }
    }

    pub fn set_remote_star(&mut self, remote_star: StarKey) {
        match self {
            LaneWrapper::Proto(lane) => lane.remote_star = Option::Some(remote_star),
            LaneWrapper::Lane(_lane) => {
                error!("cannot set the remote star for a lane, it should be already set.");
            }
        }
    }

    pub fn get_remote_star(&self) -> Option<StarKey> {
        match self {
            LaneWrapper::Proto(lane) => lane.get_remote_star(),
            LaneWrapper::Lane(lane) => lane.get_remote_star(),
        }
    }

    pub fn outgoing(&self) -> &OutgoingSide {
        match self {
            LaneWrapper::Proto(lane) => &lane.outgoing,
            LaneWrapper::Lane(lane) => &lane.outgoing,
        }
    }

    pub fn incoming(&mut self) -> &mut IncomingSide {
        match self {
            LaneWrapper::Proto(lane) => &mut lane.incoming,
            LaneWrapper::Lane(lane) => &mut lane.incoming,
        }
    }


    pub fn is_proto(&self) -> bool {
        match self {
            LaneWrapper::Proto(_) => {
                true
            }
            LaneWrapper::Lane(_) => {
                false
            }
        }

    }
}

pub struct ProtoLaneEnd {
    pub remote_star: Option<StarKey>,
    pub incoming: IncomingSide,
    pub outgoing: OutgoingSide,
    tunnel_receiver_tx: Sender<TunnelInState>,
    evolution_tx: broadcast::Sender<Result<(), Error>>,
    pub key_requestor: bool,
}

impl ProtoLaneEnd {
    pub fn new(star_key: Option<StarKey>) -> Self {
        let (mid_tx, mid_rx) = mpsc::channel(LANE_QUEUE_SIZE);
        let (in_tx, in_rx) = mpsc::channel(LANE_QUEUE_SIZE);
        let (tunnel_receiver_tx, tunnel_receiver_rx) = mpsc::channel(1);
        let (evolution_tx, _) = broadcast::channel(1);

        let midlane = LaneMiddle {
            rx: mid_rx,
            tx: in_tx,
            tunnel: TunnelOutState::None,
            queue: vec![],
        };

        tokio::spawn(async move {
            midlane.run().await;
        });

        ProtoLaneEnd {
            remote_star: star_key,
            tunnel_receiver_tx: tunnel_receiver_tx,
            incoming: IncomingSide {
                rx: in_rx,
                tunnel_receiver_rx: tunnel_receiver_rx,
                tunnel: TunnelInState::None,
            },
            outgoing: OutgoingSide { out_tx: mid_tx },
            evolution_tx,
            key_requestor: false,
        }
    }

    pub fn get_tunnel_in_tx(&self) -> Sender<TunnelInState> {
        self.tunnel_receiver_tx.clone()
    }

    pub fn get_evoltion_rx(&self) -> broadcast::Receiver<Result<(), Error>> {
        self.evolution_tx.subscribe()
    }
}

impl AbstractLaneEndpoint for ProtoLaneEnd {
    fn get_remote_star(&self) -> Option<StarKey> {
        self.remote_star.clone()
    }
}

impl TryInto<LaneEnd> for ProtoLaneEnd {
    type Error = Error;

    fn try_into(self) -> Result<LaneEnd, Self::Error> {
        if self.remote_star.is_some() {
            let evolution_tx = self.evolution_tx;
            tokio::spawn(async move {
                evolution_tx.send(Ok(()));
            });

            Ok(LaneEnd {
                remote_star: self.remote_star.unwrap(),
                incoming: self.incoming,
                outgoing: self.outgoing,
                tunnel_receiver_tx: self.tunnel_receiver_tx,
            })
        } else {
            self.evolution_tx.send(Err(
                "star_key must be set before ProtoLaneEndpoint can evolve into a LaneEndpoint"
                    .into(),
            ));
            Err(
                "star_key must be set before ProtoLaneEndpoint can evolve into a LaneEndpoint"
                    .into(),
            )
        }
    }
}

pub struct LaneEnd {
    pub remote_star: StarKey,
    pub incoming: IncomingSide,
    pub outgoing: OutgoingSide,
    tunnel_receiver_tx: Sender<TunnelInState>,
}

impl LaneEnd {
    pub fn get_tunnel_in_tx(&self) -> Sender<TunnelInState> {
        self.tunnel_receiver_tx.clone()
    }
}

impl AbstractLaneEndpoint for LaneEnd {
    fn get_remote_star(&self) -> Option<StarKey> {
        Option::Some(self.remote_star.clone())
    }
}

pub enum TunnelOutState {
    Out(TunnelOut),
    None,
}

pub enum TunnelInState {
    In(TunnelIn),
    None,
}

impl fmt::Display for TunnelOutState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            TunnelOutState::Out(_) => "Sender".to_string(),
            TunnelOutState::None => "None".to_string(),
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for TunnelInState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            TunnelInState::In(_) => "Receiver".to_string(),
            TunnelInState::None => "None".to_string(),
        };
        write!(f, "{}", r)
    }
}

#[derive(Clone)]
pub struct TunnelOut {
    //    pub remote_star: StarKey,
    pub tx: Sender<Frame>,
}

pub struct TunnelIn {
    //    pub remote_star: StarKey,
    pub rx: Receiver<Frame>,
}

#[derive(Clone)]
pub struct ConnectorController {
    pub command_tx: mpsc::Sender<ConnectorCommand>,
}

#[async_trait]
pub trait TunnelConnector: Send {}

#[derive(Clone)]
pub enum LaneSignal {
    Close,
}

pub enum ConnectorCommand {
    Reset,
    Close,
}

pub struct ClientSideTunnelConnector {
    pub in_tx: Sender<TunnelInState>,
    pub out: OutgoingSide,
    command_rx: Receiver<ConnectorCommand>,
    host_address: String,
    selector: StarInConstellationTemplateSelector,
}

impl ClientSideTunnelConnector {
    pub async fn new(
        lane: &ProtoLaneEnd,
        host_address: String,
        selector: StarInConstellationTemplateSelector,
    ) -> Result<ConnectorController, Error> {
        let (command_tx, command_rx) = mpsc::channel(16);
        let connector = Self {
            out: lane.outgoing.clone(),
            in_tx: lane.get_tunnel_in_tx(),
            command_rx,
            host_address,
            selector,
        };

        tokio::spawn(async move { connector.run().await });

        Ok(ConnectorController {
            command_tx: command_tx,
        })
    }

    #[instrument]
    async fn run(mut self) {
        loop {
            if let Result::Ok(stream) = TcpStream::connect(self.host_address.clone()).await {
                let (tx, rx) = FrameCodex::new(stream);

                let proto_tunnel = ProtoTunnel { tx: tx, rx: rx };

                match proto_tunnel.evolve().await {
                    Ok((tunnel_out, tunnel_in)) => {
                        self.out
                            .out_tx
                            .send(LaneCommand::Tunnel(TunnelOutState::Out(tunnel_out)))
                            .await;
                        self.in_tx.send(TunnelInState::In(tunnel_in)).await;

                        let _command = self.command_rx.recv().await;
                        self.out
                            .out_tx
                            .send(LaneCommand::Tunnel(TunnelOutState::None))
                            .await;
                    }
                    Err(error) => {
                        error!("CONNECTION ERROR: {}", error.error);
                        break;
                    }
                }
            }
        }
    }
}

#[async_trait]
impl TunnelConnector for ClientSideTunnelConnector {}

impl Debug for ClientSideTunnelConnector {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ClientSideTunnelConnector")
    }
}

pub struct ServerSideTunnelConnector {
    pub tunnel_in_tx: Sender<TunnelInState>,
    pub out: OutgoingSide,
    command_rx: Receiver<ConnectorCommand>,
    stream: Cell<Option<TcpStream>>,
}

impl Debug for ServerSideTunnelConnector {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("ServerSideTunnelConnector")
    }
}

impl ServerSideTunnelConnector {
    pub async fn new(
        low_lane: &ProtoLaneEnd,
        stream: TcpStream,
    ) -> Result<ConnectorController, Error> {
        let (command_tx, command_rx) = mpsc::channel(1);
        let connector = Self {
            out: low_lane.outgoing.clone(),
            tunnel_in_tx: low_lane.get_tunnel_in_tx(),
            command_rx,
            stream: Cell::new(Option::Some(stream)),
        };

        tokio::spawn(async move { connector.run().await });

        Ok(ConnectorController {
            command_tx: command_tx,
        })
    }

    #[instrument]
    async fn run(mut self) {
        let stream = match self
            .stream
            .replace(Option::None)
            .ok_or("expected stream to be Some")
        {
            Err(err) => {
                eprintln!("CONNECTION ERROR: {}", err);
                return;
            }
            Ok(stream) => stream,
        };

        let (tx, rx) = FrameCodex::new(stream);
        let proto_tunnel = ProtoTunnel { tx: tx, rx: rx };

        match proto_tunnel.evolve().await {
            Ok((tunnel_out, tunnel_in)) => {
                self.out
                    .out_tx
                    .send(LaneCommand::Tunnel(TunnelOutState::Out(tunnel_out)))
                    .await;
                self.tunnel_in_tx.send(TunnelInState::In(tunnel_in)).await;

                self.command_rx.recv().await;
                self.out
                    .out_tx
                    .send(LaneCommand::Tunnel(TunnelOutState::None))
                    .await;
            }
            Err(error) => {
                error!("CONNECTION ERROR: {}", error.error);
            }
        }
    }
}

#[async_trait]
impl TunnelConnector for ServerSideTunnelConnector {}

pub struct LocalTunnelConnector {
    pub high_star: Option<StarKey>,
    pub low_star: Option<StarKey>,
    pub high: OutgoingSide,
    pub low: OutgoingSide,
    pub high_in_tx: Sender<TunnelInState>,
    pub low_in_tx: Sender<TunnelInState>,
    command_rx: Receiver<ConnectorCommand>,
}

impl LocalTunnelConnector {
    pub async fn new(
        high_lane: &ProtoLaneEnd,
        low_lane: &ProtoLaneEnd,
    ) -> Result<ConnectorController, Error> {
        let high_star = low_lane.remote_star.clone();
        let low_star = high_lane.remote_star.clone();

        let (command_tx, command_rx) = mpsc::channel(1);

        let mut connector = LocalTunnelConnector {
            high_star: high_star.clone(),
            low_star: low_star.clone(),
            high: high_lane.outgoing.clone(),
            low: low_lane.outgoing.clone(),
            high_in_tx: high_lane.get_tunnel_in_tx(),
            low_in_tx: low_lane.get_tunnel_in_tx(),
            command_rx: command_rx,
        };

        tokio::spawn(async move { connector.run().await });

        Ok(ConnectorController {
            command_tx: command_tx,
        })
    }

    async fn run(&mut self) {
        loop {
            let (high, low) = local_tunnels();

            let (high, low) = tokio::join!(high.evolve(), low.evolve());

            if let (Ok((high_out, high_in)), Ok((low_out, low_in))) = (high, low) {
                self.high
                    .out_tx
                    .send(LaneCommand::Tunnel(TunnelOutState::Out(high_out)))
                    .await;
                self.high_in_tx.send(TunnelInState::In(high_in)).await;
                self.low
                    .out_tx
                    .send(LaneCommand::Tunnel(TunnelOutState::Out(low_out)))
                    .await;
                self.low_in_tx.send(TunnelInState::In(low_in)).await;
            } else {
                eprintln!("connection failure... trying again in 10 seconds");
                tokio::time::sleep(Duration::from_secs(10)).await;
            }

            // then wait for next command
            match self.command_rx.recv().await {
                None => {
                    self.high
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    self.low
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    return;
                }
                Some(_Reset) => {
                    // first set olds to None
                    self.high
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    self.low
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    // allow loop to continue
                }
                Some(_Close) => {
                    self.high
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    self.low
                        .out_tx
                        .send(LaneCommand::Tunnel(TunnelOutState::None))
                        .await;
                    return;
                }
            }
        }
    }
}

#[async_trait]
impl TunnelConnector for LocalTunnelConnector {}

pub struct LaneMeta<L: AbstractLaneEndpoint> {
    pub pattern: StarPattern,
    pub lane: L,
}

impl<L: AbstractLaneEndpoint> Deref for LaneMeta<L> {
    type Target = L;

    fn deref(&self) -> &Self::Target {
        &self.lane
    }
}

impl<L: AbstractLaneEndpoint> DerefMut for LaneMeta<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.lane
    }
}

impl<L: AbstractLaneEndpoint> LaneMeta<L> {

    pub fn unwrap(self) -> L {
        self.lane
    }

    pub fn new(lane: L, pattern: StarPattern) -> Self {
        LaneMeta {
            pattern,
            lane
        }
    }


}

impl TryInto<LaneMeta<LaneEnd>> for LaneMeta<ProtoLaneEnd> {
    type Error = Error;

    fn try_into(self) -> Result<LaneMeta<LaneEnd>, Self::Error> {
        let lane: LaneEnd = self.lane.try_into()?;
        Ok(LaneMeta{
           pattern: self.pattern,
           lane
        })
    }
}

pub trait AbstractLaneEndpoint {
    fn get_remote_star(&self) -> Option<StarKey>;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Serialize, Deserialize)]
pub struct ConnectionInfo {
    pub gateway: StarKey,
    pub kind: ConnectionKind,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Serialize, Deserialize)]
pub enum ConnectionKind {
    Starlane,
    Url(String),
}

pub trait TunnelConnectorFactory: Send {
    fn connector(&self, data: &ConnectionInfo) -> Result<Box<dyn TunnelConnector>, Error>;
}

pub struct FrameCodex {}

impl FrameCodex {
    pub fn new<F: Serialize + DeserializeOwned + Send + Sync + ToString + 'static>(
        stream: TcpStream,
    ) -> (mpsc::Sender<F>, mpsc::Receiver<F>) {
        let (mut read, mut write) = stream.into_split();
        let (in_tx, in_rx) = mpsc::channel(64);
        let (out_tx, mut out_rx) = mpsc::channel(64);

        tokio::spawn(async move {
            while let Option::Some(frame) = out_rx.recv().await {
                match FrameCodex::send(&mut write, frame).await {
                    Ok(_) => {}
                    Err(error) => {
                        error!("FrameCodex ERROR: {}", error.to_string());
                        break;
                    }
                }
            }
        });

        tokio::spawn(async move {
            while let Result::Ok(frame) = Self::receive(&mut read).await {
                in_tx.send(frame).await;
                // this HACK appears to be necessary in order for the receiver to
                // consistently receive values, but i do not know why
                tokio::time::sleep(Duration::from_secs(0)).await;
            }
        });

        (out_tx, in_rx)
    }

    async fn receive<F: Serialize + DeserializeOwned + Send + Sync + ToString + 'static>(
        read: &mut OwnedReadHalf,
    ) -> Result<F, Error> {
        let len = read.read_u32().await?;

        let mut buf = vec![0 as u8; len as usize];
        let buf_ref = buf.as_mut_slice();

        read.read_exact(buf_ref).await?;

        let frame: F = bincode::deserialize(buf_ref)?;

        Ok(frame)
    }

    async fn send<F: Serialize + DeserializeOwned + Send + Sync + ToString + 'static>(
        write: &mut OwnedWriteHalf,
        frame: F,
    ) -> Result<(), Error> {
        let data = bincode::serialize(&frame)?;
        write.write_u32(data.len() as _).await?;
        write.write_all(data.as_slice()).await?;
        Ok(())
    }
}

pub enum LaneIndex {
    None,
    Lane(StarKey),
    ProtoLane(usize),
}

impl LaneIndex {
    pub fn expect_proto_lane(&self) -> Result<usize, Error> {
        if let LaneIndex::ProtoLane(index) = self {
            Ok(index.clone())
        } else {
            Err("expected proto lane".into())
        }
    }

    pub fn expect_lane(&self) -> Result<StarKey, Error> {
        if let LaneIndex::Lane(key) = self {
            Ok(key.clone())
        } else {
            Err("expected lane".into())
        }
    }

    pub fn is_lane(&self) -> bool {
        if let LaneIndex::Lane(_) = self {
            return true;
        } else {
            return false;
        }
    }
}

#[cfg(test)]
mod test {
    use std::net::{SocketAddr, ToSocketAddrs};
    use std::str::FromStr;

    use futures::FutureExt;
    use tokio::net::{TcpListener, TcpStream};
    use tokio::runtime::Runtime;
    use tokio::sync::oneshot;
    use tokio::time::Duration;

    use crate::error::Error;
    use crate::frame::{Diagnose, ProtoFrame};
    use crate::id::Id;
    use crate::lane::ConnectorCommand;
    use crate::lane::Frame;
    use crate::lane::LocalTunnelConnector;
    use crate::lane::TunnelConnector;
    use crate::lane::{FrameCodex, LaneCommand, LaneEnd, ProtoLaneEnd};
    use crate::proto::local_tunnels;
    use crate::star::{StarCommand, StarKey};

    #[test]
    fn frame_codex() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let (wait_tx, wait_rx) = oneshot::channel();
            tokio::spawn(async move {
                let std_listener = std::net::TcpListener::bind("127.0.0.1:7788").unwrap();
                let listener = TcpListener::from_std(std_listener).unwrap();
                println!("LISTENING!");
                if let Ok((mut stream, _)) = listener.accept().await {
                    println!("new client!");
                    let (mut tx, mut rx) = FrameCodex::new(stream);

                    tokio::spawn(async move {
                        println!("waiting for frame...");
                        while let frame = rx.recv().await.unwrap() {
                            println!("RECEIVED FRAME: {}", frame);
                            if let Frame::Close = frame {
                                break;
                            }
                        }
                        wait_tx.send(());
                    });
                }
            });

            tokio::time::sleep(Duration::from_secs(1)).await;

            let stream = TcpStream::connect(SocketAddr::from_str("127.0.0.1:7788").unwrap())
                .await
                .unwrap();
            let (mut tx, mut rx) = FrameCodex::new(stream);

            tokio::spawn(async {
                println!("sending PING.");
            });

            tx.send(Frame::Ping).await;
            tx.send(Frame::Close).await;

            tokio::spawn(async {
                println!("PING SENT.");
            });

            wait_rx.await;

            tokio::spawn(async {
                println!("all done.");
            })
        });
    }

    #[test]
    pub fn proto_tunnel() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let (mut p1, mut p2) = local_tunnels();

            let future1 = p1.evolve();
            let future2 = p2.evolve();
            let (result1, result2) = join!(future1, future2);

            assert!(result1.is_ok());
            assert!(result2.is_ok());
        });
    }

    #[test]
    pub fn lane() {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let high = StarKey::new(2);
            let low = StarKey::new(1);

            let mut high_lane = ProtoLaneEnd::new(Option::Some(low.clone()));
            let mut low_lane = ProtoLaneEnd::new(Option::Some(high.clone()));

            let connector_ctrl = LocalTunnelConnector::new(&high_lane, &low_lane)
                .await
                .unwrap();

            high_lane
                .outgoing
                .out_tx
                .send(LaneCommand::Frame(Frame::Diagnose(Diagnose::Ping)))
                .await;

            let result = low_lane.incoming.recv().await;
            if let Some(StarCommand::Frame(Frame::Diagnose(Diagnose::Ping))) = result {
                println!("RECEIVED PING!");
                assert!(true);
            } else if let Some(frame) = result {
                println!("RECEIVED {}", frame.to_string());
                assert!(false);
            } else {
                println!("RECEIVED NONE");
                assert!(false);
            }
            connector_ctrl
                .command_tx
                .send(ConnectorCommand::Reset)
                .await;
            high_lane
                .outgoing
                .out_tx
                .send(LaneCommand::Frame(Frame::Diagnose(Diagnose::Pong)))
                .await;
            let result = low_lane.incoming.recv().await;

            if let Some(StarCommand::Frame(Frame::Diagnose(Diagnose::Pong))) = result {
                println!("RECEIVED PoNG!");
                assert!(true);
            } else if let Some(frame) = result {
                println!("RECEIVED {}", frame.to_string());
                assert!(false);
            } else {
                println!("RECEIVED NONE");
                assert!(false);
            }
        });
    }
}

#[derive(Clone,Hash,Eq,PartialEq)]
pub enum LaneId {
    Proto(u64),
    Lane(LaneKey)
}

impl LaneId {
    pub fn is_proto(&self) -> bool {
        match self {
            Self::Proto(_) => true,
            _ => false
        }
    }
}

impl TryInto<LaneKey> for LaneId {
    type Error = Error;

    fn try_into(self) -> Result<LaneKey, Self::Error> {
        match self {
            LaneId::Proto(_) => {
                Err("cannot turn a proto id into a laneKey".into())
            }
            LaneId::Lane(lane) => {
                Ok(lane)
            }
        }
    }
}


#[derive(Clone)]
pub struct LaneSession{
    pub lane_id: LaneId,
    pub pattern: StarPattern,
    pub tx: mpsc::Sender<LaneCommand>
}

impl LaneSession {
    pub fn new( lane_id: LaneId, pattern: StarPattern, tx: mpsc::Sender<LaneCommand> ) -> Self {
        Self {
            lane_id,
            pattern,
            tx
        }
    }
}
