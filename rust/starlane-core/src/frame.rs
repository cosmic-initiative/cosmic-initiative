use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

use semver::SemVerError;
use serde::{Deserialize, Serialize, Serializer};
use tokio::sync::{broadcast, mpsc, oneshot};
use tokio::sync::oneshot::error::RecvError;
use tokio::time::error::Elapsed;
use tokio::time::Instant;

use starlane_resources::ResourceIdentifier;

use crate::crypt::{Encrypted, HashEncrypted, HashId};
use crate::error::Error;
use crate::id::Id;
use crate::logger::Flags;
use crate::message::{Fail, MessageExpect, MessageId, MessageResult, MessageUpdate, ProtoStarMessage};
use crate::message::resource::{
    ActorMessage, Message, MessageReply, RawState, ResourceRequestMessage, ResourceResponseMessage,
};
use crate::names::Name;
use crate::permissions::{Authentication, AuthToken};
use crate::resource::{ActorKey, AppKey, AssignResourceStateSrc, Labels, ResourceAddress, ResourceAssign, ResourceBinding, ResourceCreate, ResourceId, ResourceKey, ResourceRecord, ResourceRegistration, ResourceSelector, ResourceSliceAssign, ResourceSliceStatus, ResourceStatus, ResourceStub, ResourceType, SubSpaceKey, UserKey};
use crate::star::{
    LocalResourceLocation, Star, StarCommand, StarInfo, StarKey, StarKind, StarNotify,
    StarSubGraphKey, StarWatchInfo,
};

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum Frame {
    Proto(ProtoFrame),
    Diagnose(Diagnose),
    StarWind(StarWind),
    StarMessage(StarMessage),
    //Watch(Watch),
    //Event(Event),
    Ping,
    Pong,
    Close
}




#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum StarWind {
    Up(WindUp),
    Down(WindDown),
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum ProtoFrame {
    StarLaneProtocolVersion(i32),
    ReportStarKey(StarKey),
    GatewaySelect,
    GatewayAssign( Vec<StarSubGraphKey> ),
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum Watch {
    Add(WatchInfo),
    Remove(WatchInfo),
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct WatchInfo {
    pub id: Id,
    pub actor: ActorKey,
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct StarMessageAck {
    pub from: StarKey,
    pub to: StarKey,
    pub id: Id,
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum Diagnose {
    Ping,
    Pong,
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct WindUp {
    pub from: StarKey,
    pub pattern: StarPattern,
    pub hops: Vec<StarKey>,
    pub transactions: Vec<u64>,
    pub max_hops: usize,
    pub action: WindAction,
}

impl WindUp {
    pub fn new(from: StarKey, pattern: StarPattern, action: WindAction) -> Self {
        WindUp {
            from: from,
            pattern: pattern,
            action: action,
            hops: vec![],
            transactions: vec![],
            max_hops: 255,
        }
    }
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum WindAction {
    SearchHits,
    Flags(Flags),
}

impl WindAction {
    pub fn update(
        &self,
        mut new_hits: Vec<WindHit>,
        result: WindResults,
    ) -> Result<WindResults, Error> {
        match self {
            WindAction::SearchHits => {
                if let WindResults::None = result {
                    let mut hits = vec![];
                    hits.append(&mut new_hits);
                    Ok(WindResults::Hits(hits))
                } else if let WindResults::Hits(mut old_hits) = result {
                    let mut hits = vec![];
                    hits.append(&mut old_hits);
                    hits.append(&mut new_hits);
                    Ok(WindResults::Hits(hits))
                } else {
                    Err(
                        "when action is SearchHIts, expecting WindResult::Hits or WindResult::None"
                            .into(),
                    )
                }
            }
            WindAction::Flags(flags) => Ok(WindResults::None),
        }
    }
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum WindResults {
    None,
    Hits(Vec<WindHit>),
}

impl WindUp {
    pub fn inc(&mut self, hop: StarKey, transaction: u64) {
        self.hops.push(hop);
        self.transactions.push(transaction);
    }
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum StarPattern {
    Any,
    None,
    StarKey(StarKey),
    StarKind(StarKind),
}

impl StarPattern {
    pub fn is_match(&self, info: &StarInfo) -> bool {
        match self {
            StarPattern::Any => true,
            StarPattern::None => false,
            StarPattern::StarKey(star) => *star == info.key,
            StarPattern::StarKind(kind) => *kind == info.kind,
        }
    }

    pub fn is_single_match(&self) -> bool {
        match self {
            StarPattern::StarKey(_) => true,
            StarPattern::StarKind(_) => false,
            StarPattern::Any => false,
            StarPattern::None => false,
        }
    }
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct WindDown {
    pub missed: Option<StarKey>,
    pub result: WindResults,
    pub wind_up: WindUp,
    pub transactions: Vec<u64>,
    pub hops: Vec<StarKey>,
}

impl WindDown {
    pub fn pop(&mut self) {
        self.transactions.pop();
        self.hops.pop();
    }
}

#[derive(Debug,Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct WindHit {
    pub star: StarKey,
    pub hops: usize,
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct StarMessage {
    pub from: StarKey,
    pub to: StarKey,
    pub id: MessageId,
    pub payload: StarMessagePayload,
    pub reply_to: Option<MessageId>,
    pub trace: bool,
    pub log: bool,
}

impl StarMessage {
    pub fn new(id: MessageId, from: StarKey, to: StarKey, payload: StarMessagePayload) -> Self {
        StarMessage {
            id: id,
            from: from,
            to: to,
            payload: payload,
            reply_to: Option::None,
            trace: false,
            log: false,
        }
    }

    pub fn to_central(id: MessageId, from: StarKey, payload: StarMessagePayload) -> Self {
        StarMessage {
            id: id,
            from: from,
            to: StarKey::central(),
            payload: payload,
            reply_to: Option::None,
            trace: false,
            log: false,
        }
    }

    pub fn forward(&self, to: &StarKey) -> ProtoStarMessage {
        let mut proto = ProtoStarMessage::new();
        proto.to = self.to.clone().into();
        proto.payload = self.payload.clone();
        proto
    }

    pub async fn reply_tx(
        self,
        star_tx: mpsc::Sender<StarCommand>,
    ) -> oneshot::Sender<StarMessagePayload> {
        let message = self;
        let (tx, rx) = oneshot::channel();
        tokio::spawn(async move {
            match rx.await {
                Ok(payload) => {
                    let proto = message.reply(payload);
                    star_tx.send(StarCommand::SendProtoMessage(proto));
                }
                Err(error) => {
                    let proto = message.reply_err("no reply".to_string());
                    star_tx.send(StarCommand::SendProtoMessage(proto));
                }
            }
        });

        tx
    }

    pub fn fail(&self, fail: Fail) -> ProtoStarMessage {
        self.reply(StarMessagePayload::Reply(SimpleReply::Fail(fail)))
    }

    pub fn ok(&self, reply: Reply) -> ProtoStarMessage {
        self.reply(StarMessagePayload::Reply(SimpleReply::Ok(reply)))
    }

    pub fn reply(&self, payload: StarMessagePayload) -> ProtoStarMessage {
        let mut proto = ProtoStarMessage::new();
        proto.to = self.from.clone().into();
        proto.reply_to = Option::Some(self.id.clone());
        proto.payload = payload;
        proto
    }

    pub fn reply_err(&self, err: String) -> ProtoStarMessage {
        let mut proto = ProtoStarMessage::new();
        proto.to = self.from.clone().into();
        proto.reply_to = Option::Some(self.id.clone());
        proto.payload = StarMessagePayload::Reply(SimpleReply::Fail(Fail::Error(err)));
        proto
    }

    pub fn reply_ok(&self, reply: Reply) -> ProtoStarMessage {
        let mut proto = ProtoStarMessage::new();
        proto.to = self.from.clone().into();
        proto.reply_to = Option::Some(self.id.clone());
        proto.payload = StarMessagePayload::Reply(SimpleReply::Ok(reply));
        proto
    }

    pub fn resubmit(
        self,
        expect: MessageExpect,
        tx: broadcast::Sender<MessageUpdate>,
        rx: broadcast::Receiver<MessageUpdate>,
    ) -> ProtoStarMessage {
        let mut proto = ProtoStarMessage::with_txrx(tx, rx);
        proto.to = self.from.clone().into();
        proto.expect = expect;
        proto.reply_to = Option::Some(self.id.clone());
        proto.payload = self.payload;
        proto
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum StarMessagePayload {
    None,
    MessagePayload(MessagePayload),
    ResourceManager(ChildManagerResourceAction),
    ResourceHost(ResourceHostAction),
    Space(SpaceMessage),
    Reply(SimpleReply),
    UniqueId(ResourceId),
}

impl Debug for StarMessagePayload{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(),std::fmt::Error> {
        f.write_str(match self {
            StarMessagePayload::None => "None",
            StarMessagePayload::MessagePayload(_) => "MessagePayload",
            StarMessagePayload::ResourceManager(_) => "ResourceManager",
            StarMessagePayload::ResourceHost(_) => "ResourceHost",
            StarMessagePayload::Space(_) => "Space",
            StarMessagePayload::Reply(_) => "Reply",
            StarMessagePayload::UniqueId(_) => "UniqueId"
        });
        Ok(())
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum MessagePayload {
    Request(Message<ResourceRequestMessage>),
    Response(MessageReply<ResourceResponseMessage>),
    Actor(Message<ActorMessage>),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ResourceHostAction {
    IsHosting(ResourceKey),
    Assign(ResourceAssign<AssignResourceStateSrc>),
}

pub enum ResourceHostResult {
    Ok,
    Location(LocalResourceLocation),
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub enum ChildManagerResourceAction {
    Register(ResourceRegistration),
    Location(ResourceRecord),
    Find(ResourceIdentifier),
    Status(ResourceStatusReport),
    Create(ResourceCreate),
    UniqueResourceId {
        parent: ResourceIdentifier,
        child_type: ResourceType,
    },
}



impl ToString for ChildManagerResourceAction {
    fn to_string(&self) -> String {
        match self {
            ChildManagerResourceAction::Register(_) => "Register".to_string(),
            ChildManagerResourceAction::Location(_) => "Location".to_string(),
            ChildManagerResourceAction::Find(_) => "Find".to_string(),
            ChildManagerResourceAction::Status(_) => "Status".to_string(),
            ChildManagerResourceAction::Create(_) => "Create".to_string(),
            ChildManagerResourceAction::UniqueResourceId { .. } => "UniqueResourceId".to_string(),
        }
    }
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct ResourceStatusReport {
    pub key: ResourceKey,
    pub status: ResourceStatus,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ResourceSliceStatusReport {
    pub key: ResourceKey,
    pub status: ResourceSliceStatus,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SimpleReply {
    Ok(Reply),
    Fail(Fail),
    Ack(MessageAck),
}

impl ToString for SimpleReply {
    fn to_string(&self) -> String {
        match self {
            SimpleReply::Ok(ok) => format!("Ok({})", ok.to_string()),
            SimpleReply::Fail(fail) => format!("Fail({})", fail.to_string()),
            SimpleReply::Ack(ack) => "Ack".to_string(),
        }
    }
}

impl StarMessagePayload {
    pub fn is_ack(&self) -> bool {
        match self {
            StarMessagePayload::Reply(reply) => match reply {
                SimpleReply::Ack(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Reply {
    Empty,
    Key(ResourceKey),
    Resources(Vec<ResourceRecord>),
    Resource(ResourceRecord),
    Address(ResourceAddress),
    Message(MessageReply<ResourceResponseMessage>),
    Id(ResourceId),
    Seq(u64),
}

impl ToString for Reply {
    fn to_string(&self) -> String {
        match self {
            Reply::Empty => "Empty".to_string(),
            Reply::Key(_) => "Key".to_string(),
            Reply::Resources(_) => "Keys".to_string(),
            Reply::Resource(_) => "ResourceRecord".to_string(),
            Reply::Address(_) => "Address".to_string(),
            Reply::Resource(_) => "Resource".to_string(),
            Reply::Seq(_) => "Seq".to_string(),
            Reply::Id(_) => "Id".to_string(),
            Reply::Message(_) => "Message".to_string(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SequenceMessage {
    Request,
    Response(u64),
}

#[derive(Clone, Serialize, Deserialize)]
pub struct MessageAck {
    pub id: Id,
    pub kind: MessageAckKind,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum MessageAckKind {
    Hop(StarKey),
    Received,
    Processing,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct SpaceMessage {
    pub sub_space: SubSpaceKey,
    pub user: UserKey,
    pub payload: SpacePayload,
}

impl SpaceMessage {
    pub fn with_payload(&self, payload: SpacePayload) -> Self {
        SpaceMessage {
            sub_space: self.sub_space.clone(),
            user: self.user.clone(),
            payload: payload,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SpacePayload {
    Reply(SpaceReply),
    Server(ServerPayload),
    Supervisor(SupervisorPayload),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SupervisorPayload {
    AppSequenceRequest(AppKey),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ServerPayload {
    SequenceResponse(u64),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SpaceReply {
    AppSequenceResponse(u64),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum AssignMessage {}

#[derive(Clone, Serialize, Deserialize)]
pub struct AppLabelRequest {
    pub app: AppKey,
    pub labels: Labels,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum Event {
    App(AppEvent),
    Actor(ActorEvent),
    Star(StarEvent),
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ActorEvent {
    StateChange(RawState),
    Gathered(ActorGathered),
    Scattered(ActorScattered),
    Broadcast(ActorBroadcast),
    Destroyed,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum AppEvent {
    Created,
    Ready,
    Destroyed,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum StarEvent {
    Lane(LaneEvent),
}

#[derive(Clone, Serialize, Deserialize)]
pub struct LaneEvent {
    pub star: StarKey,
    pub kind: LaneEventKind,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum LaneEventKind {
    Connect,
    Disconnect,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorGathered {
    pub to: ResourceKey,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorScattered {
    pub from: ResourceKey,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorBroadcast {
    pub topic: String,
    pub data: Vec<u8>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorLocationRequest {
    pub lookup: ActorLookup,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorLocationReport {
    pub resource: ResourceKey,
    pub location: ResourceRecord,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum ActorLookup {
    Key(ActorKey),
}

impl ActorLookup {
    pub fn app(&self) -> AppKey {
        match self {
            ActorLookup::Key(key) => key.app.clone(),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorNameLookup {
    pub app_id: Id,
    pub name: String,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ActorBind {
    pub key: ResourceKey,
    pub star: StarKey,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Rejection {
    pub message: String,
}

impl fmt::Display for Diagnose {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            Diagnose::Ping => "Ping",
            Diagnose::Pong => "Pong",
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for StarMessagePayload {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            StarMessagePayload::None => "None".to_string(),
            StarMessagePayload::Space(_) => "Space".to_string(),
            StarMessagePayload::Reply(reply) => format!("Reply({})", reply.to_string()),
            StarMessagePayload::ResourceManager(_) => "ResourceManager".to_string(),
            StarMessagePayload::ResourceHost(_) => "ResourceHost".to_string(),
            StarMessagePayload::UniqueId(_) => "UniqueId".to_string(),
            StarMessagePayload::MessagePayload(_) => "MessagePayload".to_string(),
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for Frame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            Frame::Proto(proto) => format!("Proto({})", proto).to_string(),
            Frame::Close => format!("Close").to_string(),
            Frame::Diagnose(diagnose) => format!("Diagnose({})", diagnose).to_string(),
            Frame::StarMessage(inner) => format!("StarMessage({})", inner.payload).to_string(),
            Frame::StarWind(wind) => format!("StarWind({})", wind).to_string(),
//            Frame::Watch(_) => format!("Watch").to_string(),
//            Frame::Event(_) => format!("ActorEvent").to_string(),
            Frame::Ping => "Ping".to_string(),
            Frame::Pong => "Pong".to_string()
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for StarWind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            StarWind::Up(up) => format!("Up({})", &up.pattern).to_string(),
            StarWind::Down(_) => "Down".to_string(),
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for StarPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            StarPattern::Any => "Any".to_string(),
            StarPattern::None => "None".to_string(),
            StarPattern::StarKey(key) => format!("{}", key.to_string()).to_string(),
            StarPattern::StarKind(kind) => format!("{}", kind.to_string()).to_string(),
        };
        write!(f, "{}", r)
    }
}

impl fmt::Display for ProtoFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let r = match self {
            ProtoFrame::StarLaneProtocolVersion(version) => {
                format!("StarLaneProtocolVersion({})", version).to_string()
            }
            ProtoFrame::ReportStarKey(key) => {
                format!("ReportStarKey({})", key.to_string()).to_string()
            }
            ProtoFrame::GatewaySelect => format!("GatewaySelect").to_string(),
            ProtoFrame::GatewayAssign{..} => {
                "GatewayAssign".to_string()
            }
        };
        write!(f, "{}", r)
    }
}

impl From<Error> for Fail {
    fn from(e: Error) -> Self {
        Fail::Error(format!("{}", e.to_string()))
    }
}

impl From<Elapsed> for Fail {
    fn from(e: Elapsed) -> Self {
        Fail::Timeout
    }
}

pub trait FromReply<T, E>: Sized {
    fn from_result(t: Result<T, E>) -> Result<Self, Fail>;
}

impl FromReply<(), Fail> for Reply {
    fn from_result(t: Result<(), Fail>) -> Result<Self, Fail> {
        match t {
            Ok(ok) => Ok(Reply::Empty),
            Err(e) => Err(e),
        }
    }
}

impl FromReply<Vec<ResourceRecord>, Fail> for Reply {
    fn from_result(t: Result<Vec<ResourceRecord>, Fail>) -> Result<Self, Fail> {
        match t {
            Ok(ok) => Ok(Reply::Resources(ok)),
            Err(e) => Err(e),
        }
    }
}

impl From<&str> for Fail {
    fn from(str: &str) -> Self {
        Fail::Error(str.to_string())
    }
}
impl From<String> for Fail {
    fn from(str: String) -> Self {
        Fail::Error(str)
    }
}

impl From<rusqlite::Error> for Fail {
    fn from(error: rusqlite::Error) -> Self {
        Fail::SqlError(error.to_string())
    }
}

impl From<()> for Fail {
    fn from(error: ()) -> Self {
        Fail::Error("() From Error".to_string())

    }
}

/*
impl <A> From<A> for Fail where A: ToString{
    fn from(some: A) -> Self {
        Fail::Error(some)
    }
}
 */

impl From<std::io::Error> for Fail {
    fn from(error: std::io::Error) -> Self {
        Fail::Error(error.to_string())
    }
}

impl From<SemVerError> for Fail {
    fn from(error: SemVerError) -> Self {
        Fail::Error(error.to_string())
    }
}
