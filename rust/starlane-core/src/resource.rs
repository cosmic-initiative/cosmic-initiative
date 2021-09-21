use std::collections::{HashMap, HashSet};
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::fs::DirBuilder;
use std::hash::Hash;
use std::iter::FromIterator;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use rusqlite::{Connection, params, params_from_iter, Row, ToSql, Transaction};
use rusqlite::types::{ToSqlOutput, Value, ValueRef};
use serde::{Deserialize, Serialize};
use tokio::sync::{mpsc, oneshot};
use tokio::sync::oneshot::Receiver;

use starlane_resources::{AddressCreationSrc, AssignKind, AssignResourceStateSrc, FieldSelection, KeyCreationSrc, LabelSelection, MetaSelector, ResourceArchetype, ResourceAssign, ResourceCreate, ResourceIdentifier, ResourceRegistryInfo, ResourceSelector, ResourceStub, Unique, Names, ResourcePath, ResourceCreateStrategy, ResourceAction};
use starlane_resources::ConfigSrc;
use starlane_resources::data::{BinSrc, DataSet};
use starlane_resources::message::{Fail, MessageFrom, MessageReply, MessageTo, ProtoMessage, ResourceRequestMessage, ResourceResponseMessage};

use crate::{error, logger, util};
use crate::error::Error;
use crate::file_access::FileAccess;
use crate::frame::{Reply, ReplyKind, ResourceHostAction, SimpleReply, StarMessagePayload};
use crate::logger::{elog, LogInfo, StaticLogInfo};
use crate::message::{MessageExpect, ProtoStarMessage};
use crate::names::Name;
use crate::star::{ResourceRegistryBacking, StarInfo, StarKey, StarSkel};
use crate::star::shell::pledge::{ResourceHostSelector, StarConscript};
use crate::starlane::api::StarlaneApi;
use crate::util::AsyncHashMap;
use std::collections::hash_map::RandomState;

pub mod artifact;
pub mod config;
pub mod create_args;
pub mod file;
pub mod file_system;
pub mod selector;
pub mod user;

pub type ResourceType = starlane_resources::ResourceType;
pub type ResourceAddress = starlane_resources::ResourceAddress;
pub type Path = starlane_resources::Path;
pub type DomainCase = starlane_resources::DomainCase;
pub type SkwerCase = starlane_resources::SkewerCase;

pub type ResourceKind = starlane_resources::ResourceKind;
pub type DatabaseKind = starlane_resources::DatabaseKind;
pub type FileKind = starlane_resources::FileKind;
pub type ArtifactKind = starlane_resources::ArtifactKind;

pub type ResourceKey = starlane_resources::ResourceKey;
pub type ResourceAddressPart = starlane_resources::ResourcePathSegment;
pub type ResourceAddressPartKind = starlane_resources::ResourcePathSegmentKind;

pub type RootKey = starlane_resources::RootKey;
pub type SpaceKey = starlane_resources::SpaceKey;
pub type AppKey = starlane_resources::AppKey;
pub type DatabaseKey = starlane_resources::DatabaseKey;
pub type ActorKey = starlane_resources::MechtronKey;
pub type ProxyKey = starlane_resources::ProxyKey;
pub type DomainKey = starlane_resources::DomainKey;
pub type UserKey = starlane_resources::UserKey;
pub type ArtifactKey = starlane_resources::ArtifactKey;
pub type FileSystemKey = starlane_resources::FileSystemKey;
pub type FileKey = starlane_resources::FileKey;
pub type Resource = starlane_resources::Resource;

pub type ResourceId = starlane_resources::ResourceId;

pub type ArtifactBundleKey = starlane_resources::ArtifactBundleKey;

pub type Specific = starlane_resources::Specific;

//static RESOURCE_QUERY_FIELDS: &str = "r.key,r.address,r.kind,r.specific,r.owner,r.config,r.host,r.gathering";
static RESOURCE_QUERY_FIELDS: &str = "r.key,r.address,r.kind,r.specific,r.owner,r.config,r.host";

impl ToSql for Name {
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, rusqlite::Error> {
        Ok(ToSqlOutput::Owned(Value::Text(self.to())))
    }
}

pub struct ResourceRegistryAction {
    pub tx: oneshot::Sender<ResourceRegistryResult>,
    pub command: ResourceRegistryCommand,
}

impl ResourceRegistryAction {
    pub fn new(
        command: ResourceRegistryCommand,
    ) -> (Self, oneshot::Receiver<ResourceRegistryResult>) {
        let (tx, rx) = oneshot::channel();
        (
            ResourceRegistryAction {
                tx: tx,
                command: command,
            },
            rx,
        )
    }
}

pub enum ResourceRegistryCommand {
    Close,
    Clear,
    //Accepts(HashSet<ResourceType>),
    Reserve(ResourceNamesReservationRequest),
    Commit(ResourceRegistration),
    Select(ResourceSelector),
    SetLocation(ResourceRecord),
    Get(ResourceIdentifier),
    Next { key: ResourceKey, unique: Unique },
}

pub enum ResourceRegistryResult {
    Ok,
    Error(String),
    Resource(Option<ResourceRecord>),
    Resources(Vec<ResourceRecord>),
    Address(ResourceAddress),
    Reservation(RegistryReservation),
    Key(ResourceKey),
    Unique(u64),
    NotFound,
    NotAccepted,
}

impl ToString for ResourceRegistryResult {
    fn to_string(&self) -> String {
        match self {
            ResourceRegistryResult::Ok => "Ok".to_string(),
            ResourceRegistryResult::Error(err) => format!("Error({})", err),
            ResourceRegistryResult::Resource(_) => "Resource".to_string(),
            ResourceRegistryResult::Resources(_) => "Resources".to_string(),
            ResourceRegistryResult::Address(_) => "Address".to_string(),
            ResourceRegistryResult::Reservation(_) => "Reservation".to_string(),
            ResourceRegistryResult::Key(_) => "Key".to_string(),
            ResourceRegistryResult::Unique(_) => "Unique".to_string(),
            ResourceRegistryResult::NotFound => "NotFound".to_string(),
            ResourceRegistryResult::NotAccepted => "NotAccepted".to_string(),
        }
    }
}

type Blob = Vec<u8>;

struct RegistryParams {
    key: Option<Blob>,
    address: Option<String>,
    resource_type: String,
    kind: String,
    specific: Option<String>,
    config: Option<String>,
    owner: Option<Blob>,
    host: Option<Blob>,
    parent: Option<Blob>,
}

impl RegistryParams {
    pub fn from_registration(registration: ResourceRegistration) -> Result<Self, Error> {
        Self::new(
            registration.resource.stub.archetype,
            registration.resource.stub.key.parent(),
            Option::Some(registration.resource.stub.key),
            registration.resource.stub.owner,
            Option::Some(registration.resource.stub.address),
            Option::Some(registration.resource.location.star),
        )
    }

    pub fn from_archetype(
        archetype: ResourceArchetype,
        parent: Option<ResourceKey>,
    ) -> Result<Self, Error> {
        Self::new(
            archetype,
            parent,
            Option::None,
            Option::None,
            Option::None,
            Option::None,
        )
    }

    pub fn new(
        archetype: ResourceArchetype,
        parent: Option<ResourceKey>,
        key: Option<ResourceKey>,
        owner: Option<UserKey>,
        address: Option<ResourcePath>,
        host: Option<StarKey>,
    ) -> Result<Self, Error> {
        let key = if let Option::Some(key) = key {
            Option::Some(key.bin()?)
        } else {
            Option::None
        };

        let address = if let Option::Some(address) = address {
            Option::Some(address.to_string())
        } else {
            Option::None
        };

        let resource_type = archetype.kind.resource_type().to_string();
        let kind = archetype.kind.to_string();

        let owner = if let Option::Some(owner) = owner {
            Option::Some(owner.bin()?)
        } else {
            Option::None
        };

        let specific = match &archetype.specific {
            None => Option::None,
            Some(specific) => Option::Some(specific.to_string()),
        };

        let config = match &archetype.config {
            None => Option::None,
            Some(config) => Option::Some(config.to_string()),
        };

        let parent = match parent {
            None => Option::None,
            Some(parent) => Option::Some(parent.bin()?),
        };

        let host = match host {
            Some(host) => Option::Some(host.bin()?),
            None => Option::None,
        };

        Ok(RegistryParams {
            key: key,
            address: address,
            resource_type: resource_type,
            kind: kind,
            specific: specific,
            parent: parent,
            config: config,
            owner: owner,
            host: host,
        })
    }
}

pub struct Registry {
    pub conn: Connection,
    pub tx: mpsc::Sender<ResourceRegistryAction>,
    pub rx: mpsc::Receiver<ResourceRegistryAction>,
    star_info: StarInfo,
}

impl Registry {
    pub async fn new(star_info: StarInfo, path: String) -> mpsc::Sender<ResourceRegistryAction> {
        let (tx, rx) = mpsc::channel(8 * 1024);
        let tx_clone = tx.clone();

        // ensure that path directory exists
        let mut dir_builder = DirBuilder::new();
        dir_builder.recursive(true);
        if let Result::Err(_) = dir_builder.create(path.clone()) {
            eprintln!("FATAL: could not create star data directory: {}", path);
            return tx;
        }
        tokio::spawn(async move {
            //let conn = Connection::open(format!("{}/resource_registry.sqlite",path));
            let conn = Connection::open_in_memory();
            if conn.is_ok() {
                let mut db = Registry {
                    conn: conn.unwrap(),
                    tx: tx_clone,
                    rx: rx,
                    star_info: star_info,
                };
                db.run().await.unwrap();
            } else {
                let log_info = StaticLogInfo::new(
                    "ResourceRegistry".to_string(),
                    star_info.log_kind().to_string(),
                    star_info.key.to_string(),
                );
                eprintln!("connection ERROR!");
                logger::elog(
                    &log_info,
                    &star_info,
                    "new()",
                    format!(
                        "ERROR: could not create SqLite connection to database: '{}'",
                        conn.err().unwrap().to_string(),
                    )
                    .as_str(),
                );
            }
        });
        tx
    }

    async fn run(&mut self) -> Result<(), Error> {
        match self.setup() {
            Ok(_) => {}
            Err(err) => {
                eprintln!("error setting up db: {}", err);
                return Err(err);
            }
        };

        while let Option::Some(request) = self.rx.recv().await {
            if let ResourceRegistryCommand::Close = request.command {
                break;
            }
            match self.process(request.command) {
                Ok(ok) => {
                    request.tx.send(ok);
                }
                Err(err) => {
                    eprintln!("{}", err);
                    request
                        .tx
                        .send(ResourceRegistryResult::Error(err.to_string()));
                }
            }
        }

        Ok(())
    }

    fn process(
        &mut self,
        command: ResourceRegistryCommand,
    ) -> Result<ResourceRegistryResult, Error> {
        match command {
            ResourceRegistryCommand::Close => Ok(ResourceRegistryResult::Ok),
            ResourceRegistryCommand::Clear => {
                let trans = self.conn.transaction()?;
                trans.execute("DELETE FROM labels", [])?;
                trans.execute("DELETE FROM names", [])?;
                trans.execute("DELETE FROM resources", [])?;
                trans.execute("DELETE FROM uniques", [])?;
                trans.commit()?;

                Ok(ResourceRegistryResult::Ok)
            }

            ResourceRegistryCommand::Commit(registration) => {
                let params = RegistryParams::from_registration(registration.clone())?;

                let trans = self.conn.transaction()?;

                if params.key.is_some() {
                    trans.execute(
                        "DELETE FROM labels WHERE labels.resource_key=?1",
                        [params.key.clone()],
                    );
                    trans.execute("DELETE FROM resources WHERE key=?1", [params.key.clone()])?;
                }

                trans.execute("INSERT INTO resources (key,address,resource_type,kind,specific,parent,owner,config,host) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9)", params![params.key,params.address,params.resource_type,params.kind,params.specific,params.parent,params.owner,params.config,params.host])?;
                if let Option::Some(info) = registration.info {
                    for name in info.names {
                        trans.execute("UPDATE names SET key=?1 WHERE name=?1", [name])?;
                    }
                    for (name, value) in info.labels {
                        trans.execute(
                            "INSERT INTO labels (resource_key,name,value) VALUES (?1,?2,?3)",
                            params![params.key, name, value],
                        )?;
                    }
                }

                trans.commit()?;
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Select(selector) => {
                let mut params: Vec<FieldSelectionSql> = vec![];
                let mut where_clause = String::new();

                for (index, field) in Vec::from_iter(selector.fields.clone())
                    .iter()
                    .map(|x| x.clone())
                    .enumerate()
                {
                    if index != 0 {
                        where_clause.push_str(" AND ");
                    }

                    let f = match field {
                        FieldSelection::Identifier(_) => {
                            format!("r.key=?{}", index + 1)
                        }
                        FieldSelection::Type(_) => {
                            format!("r.resource_type=?{}", index + 1)
                        }
                        FieldSelection::Kind(_) => {
                            format!("r.kind=?{}", index + 1)
                        }
                        FieldSelection::Specific(_) => {
                            format!("r.specific=?{}", index + 1)
                        }
                        FieldSelection::Owner(_) => {
                            format!("r.owner=?{}", index + 1)
                        }
                        FieldSelection::Parent(_) => {
                            format!("r.parent=?{}", index + 1)
                        }
                    };
                    where_clause.push_str(f.as_str());
                    params.push(field.into());
                }

                /*
                if !params.is_empty() {
                    where_clause.push_str(" AND ");
                }

                where_clause.push_str(" key IS NOT NULL");

                 */

                let mut statement = match &selector.meta {
                    MetaSelector::None => {
                        format!(
                            "SELECT DISTINCT {} FROM resources as r WHERE {}",
                            RESOURCE_QUERY_FIELDS, where_clause
                        )
                    }
                    MetaSelector::Label(label_selector) => {
                        let mut labels = String::new();
                        for (_index, label_selection) in
                            Vec::from_iter(label_selector.labels.clone())
                                .iter()
                                .map(|x| x.clone())
                                .enumerate()
                        {
                            if let LabelSelection::Exact(label) = label_selection {
                                labels.push_str(format!(" AND {} IN (SELECT labels.resource_key FROM labels WHERE labels.name='{}' AND labels.value='{}')", RESOURCE_QUERY_FIELDS, label.name, label.value).as_str())
                            }
                        }

                        format!(
                            "SELECT DISTINCT {} FROM resources as r WHERE {} {}",
                            RESOURCE_QUERY_FIELDS, where_clause, labels
                        )
                    }
                    MetaSelector::Name(name) => {
                        if where_clause.is_empty() {
                            format!(
                                "SELECT DISTINCT {} FROM names as r WHERE r.name='{}'",
                                RESOURCE_QUERY_FIELDS, name
                            )
                        } else {
                            format!(
                                "SELECT DISTINCT {} FROM names as r WHERE {} AND r.name='{}'",
                                RESOURCE_QUERY_FIELDS, where_clause, name
                            )
                        }
                    }
                };

                // in case this search was for EVERYTHING
                if selector.is_empty() {
                    statement = format!(
                        "SELECT DISTINCT {} FROM resources as r",
                        RESOURCE_QUERY_FIELDS
                    )
                    .to_string();
                }

                let mut statement = self.conn.prepare(statement.as_str())?;
                let mut rows = statement.query(params_from_iter(params.iter()))?;

                let mut resources = vec![];
                while let Option::Some(row) = rows.next()? {
                    resources.push(Self::process_resource_row_catch(row)?);
                }
                Ok(ResourceRegistryResult::Resources(resources))
            }
            ResourceRegistryCommand::SetLocation(location_record) => {
                let key = location_record.stub.key.bin()?;
                let host = location_record.location.star.bin()?;
                let trans = self.conn.transaction()?;
                trans.execute(
                    "UPDATE resources SET host=?1 WHERE key=?3",
                    params![host, key],
                )?;
                trans.commit()?;
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Get(identifier) => {

                if identifier.is_root() {
                    return Ok(ResourceRegistryResult::Resource(Option::Some(
                        ResourceRecord::root(),
                    )));
                }

                let result = match &identifier {
                    ResourceIdentifier::Key(key) => {
                        let key = key.bin()?;
                        let statement = format!(
                            "SELECT {} FROM resources as r WHERE key=?1",
                            RESOURCE_QUERY_FIELDS
                        );
                        let mut statement = self.conn.prepare(statement.as_str())?;
                        statement.query_row(params![key], |row| {
                            Ok(Self::process_resource_row_catch(row)?)
                        })
                    }
                    ResourceIdentifier::Address(address) => {
                        let address = address.to_string();
                        let statement = format!(
                            "SELECT {} FROM resources as r WHERE address=?1",
                            RESOURCE_QUERY_FIELDS
                        );
                        let mut statement = self.conn.prepare(statement.as_str())?;
                        statement.query_row(params![address], |row| {
                            Ok(Self::process_resource_row_catch(row)?)
                        })
                    }
                };

                match result {
                    Ok(record) => Ok(ResourceRegistryResult::Resource(Option::Some(record))),
                    Err(rusqlite::Error::QueryReturnedNoRows) => {
                        Ok(ResourceRegistryResult::Resource(Option::None))
                    }
                    Err(err) => match err {
                        rusqlite::Error::QueryReturnedNoRows => {
                            Ok(ResourceRegistryResult::Resource(Option::None))
                        }
                        err => {
                            eprintln!(
                                "for {} SQL ERROR: {}",
                                identifier.to_string(),
                                err.to_string()
                            );
                            Err(err.into())
                        }
                    },
                }
            }

            ResourceRegistryCommand::Reserve(request) => {
                let trans = self.conn.transaction()?;
                trans.execute("DELETE FROM names WHERE key IS NULL AND datetime(reservation_timestamp) < datetime('now')", [] )?;
                let params = RegistryParams::new(
                    request.archetype.clone(),
                    Option::Some(request.parent.clone()),
                    Option::None,
                    Option::None,
                    Option::None,
                    Option::None,
                )?;
                if request.info.is_some() {
                    let params = RegistryParams::from_archetype(
                        request.archetype.clone(),
                        Option::Some(request.parent.clone()),
                    )?;
                    Self::process_names(
                        &trans,
                        &request.info.as_ref().cloned().unwrap().names,
                        &params,
                    )?;
                }
                trans.commit()?;
                let (tx, rx) = oneshot::channel();
                let reservation = RegistryReservation::new(tx);
                let action_tx = self.tx.clone();
                let info = request.info.clone();
                tokio::spawn(async move {
                    let result = rx.await;
                    if let Result::Ok((record, result_tx)) = result {
                        let mut params = params;
                        let key = match record.stub.key.bin() {
                            Ok(key) => Option::Some(key),
                            Err(_) => Option::None,
                        };

                        params.key = key;
                        params.address = Option::Some(record.stub.address.to_string());
                        let registration = ResourceRegistration::new(record.clone(), info);
                        let (action, rx) = ResourceRegistryAction::new(
                            ResourceRegistryCommand::Commit(registration),
                        );
                        action_tx.send(action).await;
                        rx.await;
                        result_tx.send(Ok(()));
                    } else if let Result::Err(error) = result {
                        error!(
                                "ERROR: reservation failed to commit due to RecvErr: '{}'",
                                error.to_string()
                            );
                    } else  {
                     error!("ERROR: reservation failed to commit.");
                    }
                });
                Ok(ResourceRegistryResult::Reservation(reservation))
            }

            ResourceRegistryCommand::Next { key, unique } => {
                let trans = self.conn.transaction()?;
                let key = key.bin()?;
                let column = match unique {
                    Unique::Sequence => "sequence",
                    Unique::Index => "id_index",
                };

                trans.execute(
                    "INSERT OR IGNORE INTO uniques (key) VALUES (?1)",
                    params![key],
                )?;
                trans.execute(
                    format!("UPDATE uniques SET {}={}+1 WHERE key=?1", column, column).as_str(),
                    params![key],
                )?;
                let rtn = trans.query_row(
                    format!("SELECT {} FROM uniques WHERE key=?1", column).as_str(),
                    params![key],
                    |r| {
                        let rtn: u64 = r.get(0)?;
                        Ok(rtn)
                    },
                )?;
                trans.commit()?;

                Ok(ResourceRegistryResult::Unique(rtn))
            }
        }
    }

    fn process_resource_row_catch(row: &Row) -> Result<ResourceRecord, Error> {
        match Self::process_resource_row(row) {
            Ok(ok) => Ok(ok),
            Err(error) => {
                eprintln!("process_resource_rows: {}", error);
                Err(error)
            }
        }
    }

    fn process_resource_row(row: &Row) -> Result<ResourceRecord, Error> {
        let key: Vec<u8> = row.get(0)?;
        let key = ResourceKey::from_bin(key)?;

        let address: String = row.get(1)?;
        let address = ResourcePath::from_str(address.as_str())?;

        let kind: String = row.get(2)?;
        let kind = ResourceKind::from_str(kind.as_str())?;

        let specific = if let ValueRef::Null = row.get_ref(3)? {
            Option::None
        } else {
            let specific: String = row.get(3)?;
            let specific = Specific::from_str(specific.as_str())?;
            Option::Some(specific)
        };

        let owner = if let ValueRef::Null = row.get_ref(4)? {
            Option::None
        } else {
            let owner: Vec<u8> = row.get(4)?;
            let owner: UserKey = UserKey::from_bin(owner)?;
            Option::Some(owner)
        };

        let config = if let ValueRef::Null = row.get_ref(5)? {
            Option::None
        } else {
            let config: String = row.get(5)?;
            let config = ConfigSrc::from_str(config.as_str())?;
            Option::Some(config)
        };

        let host: Vec<u8> = row.get(6)?;
        let host = StarKey::from_bin(host)?;

        let stub = ResourceStub {
            key: key,
            address: address,
            archetype: ResourceArchetype {
                kind: kind,
                specific: specific,
                config: config,
            },
            owner: owner,
        };

        let record = ResourceRecord {
            stub: stub,
            location: ResourceLocation { star: host },
        };

        Ok(record)
    }

    fn process_names(
        trans: &Transaction,
        names: &Names,
        params: &RegistryParams,
    ) -> Result<(), Error> {
        for name in names {
            trans.execute("INSERT INTO names (key,name,resource_type,kind,specific,parent,owner,config,reservation_timestamp) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,timestamp('now','+5 minutes')", params![params.key,name,params.resource_type,params.kind,params.specific,params.parent,params.owner,params.config])?;
        }
        Ok(())
    }

    pub fn setup(&mut self) -> Result<(), Error> {
        let labels = r#"
       CREATE TABLE IF NOT EXISTS labels (
	      key INTEGER PRIMARY KEY AUTOINCREMENT,
	      resource_key BLOB,
	      name TEXT NOT NULL,
	      value TEXT NOT NULL,
          UNIQUE(key,name),
          FOREIGN KEY (resource_key) REFERENCES resources (key)
        )"#;

        let names = r#"
       CREATE TABLE IF NOT EXISTS names(
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          key BLOB,
	      name TEXT NOT NULL,
	      resource_type TEXT NOT NULL,
          kind BLOB NOT NULL,
          specific TEXT,
          parent BLOB,
          app TEXT,
          owner BLOB,
          reservation_timestamp TEXT,
          UNIQUE(name,resource_type,kind,specific,parent)
        )"#;

        let resources = r#"CREATE TABLE IF NOT EXISTS resources (
         key BLOB PRIMARY KEY,
         address TEXT NOT NULL,
         resource_type TEXT NOT NULL,
         kind BLOB NOT NULL,
         specific TEXT,
         config TEXT,
         parent BLOB,
         owner BLOB,
         host BLOB
        )"#;

        let address_index = "CREATE UNIQUE INDEX resource_address_index ON resources(address)";

        let uniques = r#"CREATE TABLE IF NOT EXISTS uniques(
         key BLOB PRIMARY KEY,
         sequence INTEGER NOT NULL DEFAULT 0,
         id_index INTEGER NOT NULL DEFAULT 0
        )"#;

        let transaction = self.conn.transaction()?;
        transaction.execute(labels, [])?;
        transaction.execute(names, [])?;
        transaction.execute(resources, [])?;
        transaction.execute(uniques, [])?;
        transaction.execute(address_index, [])?;
        transaction.commit()?;

        Ok(())
    }
}

impl LogInfo for Registry {
    fn log_identifier(&self) -> String {
        self.star_info.log_identifier()
    }

    fn log_kind(&self) -> String {
        self.star_info.log_kind()
    }

    fn log_object(&self) -> String {
        "Registry".to_string()
    }
}

#[async_trait]
pub trait ResourceIdSeq: Send + Sync {
    async fn next(&self) -> ResourceId;
}

#[async_trait]
pub trait HostedResource: Send + Sync {
    fn key(&self) -> ResourceKey;
}

#[derive(Clone)]
pub struct HostedResourceStore {
    map: AsyncHashMap<ResourceKey, Arc<LocalHostedResource>>,
}

impl HostedResourceStore {
    pub async fn new() -> Self {
        HostedResourceStore {
            map: AsyncHashMap::new(),
        }
    }

    pub async fn store(&self, resource: Arc<LocalHostedResource>) -> Result<(), Error> {
        self.map.put(resource.resource.key.clone(), resource).await
    }

    pub async fn get(&self, key: ResourceKey) -> Result<Option<Arc<LocalHostedResource>>, Error> {
        self.map.get(key).await
    }

    pub async fn remove(
        &self,
        key: ResourceKey,
    ) -> Result<Option<Arc<LocalHostedResource>>, Error> {
        self.map.remove(key).await
    }

    pub async fn contains(&self, key: &ResourceKey) -> Result<bool, Error> {
        self.map.contains(key.clone()).await
    }
}

#[derive(Clone)]
pub struct RemoteHostedResource {
    key: ResourceKey,
    star_host: StarKey,
    local_skel: StarSkel,
}

pub struct LocalHostedResource {
    //    pub manager: Arc<dyn ResourceManager>,
    pub unique_src: Box<dyn UniqueSrc>,
    pub resource: ResourceStub,
}
impl HostedResource for LocalHostedResource {
    fn key(&self) -> ResourceKey {
        self.resource.key.clone()
    }
}

#[async_trait]
pub trait ResourceManager: Send + Sync {
    async fn create(
        &self,
        create: ResourceCreate,
    ) -> oneshot::Receiver<Result<ResourceRecord, Fail>>;
}

pub struct RemoteResourceManager {
    pub key: ResourceKey,
}

impl RemoteResourceManager {
    pub fn new(key: ResourceKey) -> Self {
        RemoteResourceManager { key: key }
    }
}

#[async_trait]
impl ResourceManager for RemoteResourceManager {
    async fn create(&self, _create: ResourceCreate) -> Receiver<Result<ResourceRecord, Fail>> {
        unimplemented!();
    }
}

#[derive(Clone)]
pub struct ParentCore {
    pub skel: StarSkel,
    pub stub: ResourceStub,
    pub selector: ResourceHostSelector,
    pub child_registry: Arc<dyn ResourceRegistryBacking>,
}

impl Debug for ParentCore {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_tuple("ParentCore")
            .field(&self.skel)
            .field(&self.stub)
            .finish()
    }
}

pub struct Parent {
    pub core: ParentCore,
}

impl Parent {
    #[instrument]
    async fn create_child(
        core: ParentCore,
        create: ResourceCreate,
        tx: oneshot::Sender<Result<ResourceRecord, Fail>>,
    ) {
        let parent = match create
            .parent
            .clone()
            .key_or("expected create.parent to already be a key")
        {
            Ok(key) => key,
            Err(error) => {
                tx.send(Err(Fail::from(error)));
                return;
            }
        };

        if let Ok(reservation) = core
            .child_registry
            .reserve(ResourceNamesReservationRequest {
                parent: parent,
                archetype: create.archetype.clone(),
                info: create.registry_info.clone(),
            })
            .await
        {
            let rx =
                ResourceCreationChamber::new(core.stub.clone(), create.clone(), core.skel.clone())
                    .await;

            tokio::spawn(async move {

                match Self::process_action(core.clone(), create.clone(), reservation, rx).await {
                    Ok(resource) => {
                        tx.send(Ok(resource));
                    }
                    Err(fail) => {
                        error!("Failed to create child: FAIL: {}", fail.to_string());
                        tx.send(Err(fail.into()));
                    }
                }
            });
        } else {
            error!("ERROR: reservation failed.");

            tx.send(Err("RESERVATION FAILED!".into()));
        }
    }

    async fn process_action(
        core: ParentCore,
        create: ResourceCreate,
        reservation: RegistryReservation,
        rx: oneshot::Receiver<
            Result<ResourceAction<AssignResourceStateSrc<DataSet<BinSrc>>>, Fail>,
        >,
    ) -> Result<ResourceRecord, Error> {
        let action = rx.await??;

        match action {
            ResourceAction::Create(assign) => {
                let host = core
                    .selector
                    .select(create.archetype.kind.resource_type())
                    .await?;
                let record = ResourceRecord::new(assign.stub.clone(), host.star_key());
                host.assign(assign.clone().try_into()?).await?;
                let (commit_tx, _commit_rx) = oneshot::channel();
                reservation.commit(record.clone(), commit_tx)?;
                host.init(assign.stub.key).await?;
                Ok(record)
            }
            ResourceAction::Update(resource) => {
               // save resource state...
               let mut proto = ProtoMessage::new();
               proto.payload(ResourceRequestMessage::UpdateState(resource.state_src()));
               proto.to(resource.key.clone().into());
               proto.from(create.from.clone());
               let reply  = core.skel.messaging_api.exchange(proto.try_into()?, ReplyKind::Empty, "updating the state of a record " ).await;
               match reply {
                   Ok(reply) => {
                       let record = core.skel.resource_locator_api.locate(resource.key.into()).await;
                       record
                   }
                   Err(err) => {
                       Err(err.into())
                   }
               }

//               reservation.cancel();

            }
        }

    }

    /*
    if let Ok(Ok(assign)) = rx.await {
    if let Ok(mut host) = core.selector.select(create.archetype.kind.resource_type()).await
    {
    let record = ResourceRecord::new(assign.stub.clone(), host.star_key());
    match host.assign(assign).await
    {
    Ok(_) => {}
    Err(err) => {
    eprintln!("host assign failed.");
    return;
    }
    }
    let (commit_tx, commit_rx) = oneshot::channel();
    match reservation.commit(record.clone(), commit_tx) {
    Ok(_) => {
    if let Ok(Ok(_)) = commit_rx.await {
    tx.send(Ok(record));
    } else {
    elog( &core, &record.stub, "create_child()", "commit failed" );
    tx.send(Err("commit failed".into()));
    }
    }
    Err(err) => {
    elog( &core, &record.stub, "create_child()", format!("ERROR: commit failed '{}'",err.to_string()).as_str() );
    tx.send(Err("commit failed".into()));
    }
    }
    } else {
    elog( &core, &assign.stub, "create_child()", "ERROR: could not select a host" );
    tx.send(Err("could not select a host".into()));
    }
    }

     */

    /*
    async fn process_create(core: ChildResourceManagerCore, create: ResourceCreate ) -> Result<ResourceRecord,Fail>{



        if !create.archetype.kind.resource_type().parent().matches(Option::Some(&core.key.resource_type())) {
            return Err(Fail::WrongParentResourceType {
                expected: HashSet::from_iter(core.key.resource_type().parent().types()),
                received: Option::Some(create.parent.resource_type())
            });
        };

        create.validate()?;

        let reservation = core.registry.reserve(ResourceNamesReservationRequest{
            parent: create.parent.clone(),
            archetype: create.archetype.clone(),
            info: create.registry_info } ).await?;

        let key = match create.key {
            KeyCreationSrc::None => {
                ResourceKey::new(core.key.clone(), ResourceId::new(&create.archetype.kind.resource_type(), core.id_seq.next() ) )?
            }
            KeyCreationSrc::Key(key) => {
                if key.parent() != Option::Some(core.key.clone()){
                    return Err("parent keys do not match".into());
                }
                key
            }
        };

        let address = match create.address{
            AddressCreationSrc::None => {
                let address = format!("{}:{}", core.address.to_parts_string(), key.generate_address_tail()? );
                create.archetype.kind.resource_type().address_structure().from_str(address.as_str())?
            }
            AddressCreationSrc::Append(tail) => {
                create.archetype.kind.resource_type().append_address(core.address.clone(), tail )?
            }
            AddressCreationSrc::Space(space_name) => {
                if core.key.resource_type() != ResourceType::Nothing{
                    return Err(format!("Space creation can only be used at top level (Nothing) not by {}",core.key.resource_type().to_string()).into());
                }
                ResourceAddress::for_space(space_name.as_str())?
            }
        };

        let stub = ResourceStub {
            key: key,
            address: address.clone(),
            archetype: create.archetype.clone(),
            owner: None
        };


        let assign = ResourceAssign {
            stub: stub.clone(),
            state_src: create.src.clone(),
        };

        let mut host = core.selector.select(create.archetype.kind.resource_type() ).await?;
        host.assign(assign).await?;
        let record = ResourceRecord::new( stub, host.star_key() );
        let (tx,rx) = oneshot::channel();
        reservation.commit( record.clone(), tx )?;

        Ok(record)
    }

     */
}

impl LogInfo for ParentCore {
    fn log_identifier(&self) -> String {
        self.skel.info.log_identifier()
    }

    fn log_kind(&self) -> String {
        self.skel.info.log_kind()
    }

    fn log_object(&self) -> String {
        "Parent".to_string()
    }
}

#[async_trait]
impl ResourceManager for Parent {
    async fn create(
        &self,
        create: ResourceCreate,
    ) -> oneshot::Receiver<Result<ResourceRecord, Fail>> {
        let (tx, rx) = oneshot::channel();

        let core = self.core.clone();
        tokio::spawn(async move {
            Parent::create_child(core, create, tx).await;
        });
        rx
    }
}

pub struct ResourceCreationChamber {
    parent: ResourceStub,
    create: ResourceCreate,
    skel: StarSkel,
    tx: oneshot::Sender<Result<ResourceAction<AssignResourceStateSrc<DataSet<BinSrc>>>, Fail>>,
}

impl ResourceCreationChamber {
    pub async fn new(
        parent: ResourceStub,
        create: ResourceCreate,
        skel: StarSkel,
    ) -> oneshot::Receiver<Result<ResourceAction<AssignResourceStateSrc<DataSet<BinSrc>>>, Fail>>
    {
        let (tx, rx) = oneshot::channel();
        let chamber = ResourceCreationChamber {
            parent: parent,
            create: create,
            skel: skel,
            tx: tx,
        };
        chamber.run().await;
        rx
    }

    async fn run(self) {
        tokio::spawn(async move {
            if !self.create.parent.is_key() {
                self.tx.send( Err(Fail::Error("ResourceCreationChamber requires keyed ResourceCreate object.  Call ResourceCreate::to_keyed(starlane_api) to modify".to_string())) );
                return;
            }

            if !self
                .create
                .archetype
                .kind
                .resource_type()
                .parents()
                .contains(&self.parent.archetype.kind.resource_type())
            {
                println!("!!! -> Throwing Fail::WrongParentResourceType for kind {} & ResourceType {} <- !!!", self.create.archetype.kind.to_string(), self.create.archetype.kind.resource_type().to_string() );

                self.tx.send(Err(Fail::WrongParentResourceType {
                    expected: HashSet::from_iter(
                        self.create.archetype.kind.resource_type().parents(),
                    ),
                    received: Option::None,
                }));
                return;
            };

            match self.create.validate() {
                Ok(_) => {}
                Err(error) => {
                    self.tx.send(Err(error));
                    return;
                }
            }

            fn create_address( src: &AddressCreationSrc, parent: &ResourcePath ) -> Result<ResourcePath,Error>{
                match src {
                    AddressCreationSrc::Append(tail) => {
                        Ok(parent.append(tail.as_str() )?)
                    }
                    AddressCreationSrc::Just(space_name) => {
                        Ok(ResourcePath::from_str(space_name.as_str())?)
                    }
                    AddressCreationSrc::Exact(address) =>
                        Ok(address.clone()),
                }
            }

            let address = match create_address( &self.create.address, &self.parent.address ) {
                Ok(address) => {address}
                Err(err) => {
                    self.tx.send(Err(err.into()));
                    return;
                }
            };

            let record = self.skel.resource_locator_api.locate(address.clone().into() ).await;

            let key = match record{
                Ok(record) => {
                    match self.create.strategy {
                        ResourceCreateStrategy::Create => {
                            self.tx.send(Err(format!("resource with address already exists: '{}'",address.to_string()).into()));
                            return;
                        }
                        ResourceCreateStrategy::Ensure => {
                            // we've proven it's here, now we can go home
                            return;
                        }
                        ResourceCreateStrategy::CreateOrUpdate => {
                            if record.stub.archetype != self.create.archetype {
                                self.tx.send(Err("cannot update a resource with a different archetype (Type,Kind,Specific & ConfigSrc)".into()));
                                return;
                            }
                            match self.create.state_src {
                                AssignResourceStateSrc::Stateless => {
                                    self.tx.send(Err("cannot update a stateless resource".into()));
                                    return;
                                }
                                AssignResourceStateSrc::CreateArgs(_) => {
                                    self.tx.send(Err("cannot execute create-args on an existing resource".into()));
                                    return;
                                }
                                AssignResourceStateSrc::Direct(state) => {
                                    let resource = Resource::new(record.stub.key, record.stub.address, record.stub.archetype,state );
                                    self.tx.send(Ok(ResourceAction::Update(resource)) );
                                    return;
                                }
                            }

                        }
                    }
                }
                Err(_) => {
                    let _key = match &self.create.key {
                        KeyCreationSrc::None => {
                            let mut proto = ProtoMessage::new();
                            proto.to(MessageTo::from(self.parent.key.clone()));
                            proto.from(MessageFrom::Resource(self.parent.key.clone().into()));
                            proto.payload = Option::Some(ResourceRequestMessage::Unique(
                                self.create.archetype.kind.resource_type(),
                            ));

                            let mut proto_star_message = match proto.try_into() {
                                Ok(proto_star_message) => proto_star_message,
                                Err(error) => {
                                    eprintln!(
                                        "ERROR when process proto_star_message from ProtoMessage: {}",
                                        error
                                    );
                                    return;
                                }
                            };

                            let skel = self.skel.clone();

                            tokio::spawn(async move {
                                match skel.messaging_api.exchange(proto_star_message, ReplyKind::Id, "ResourceCreationChamber requesting unique id from parent to create unique ResourceKey" ).await
                                {
                                    Ok(Reply::Id(id)) => {
                                        match ResourceKey::new(self.parent.key.clone(), id.clone()) {
                                            Ok(key) => {
                                                let final_create = self.finalize_create(key.clone(), address.clone() ).await;
                                                self.tx.send(final_create);
                                                return;
                                            }
                                            Err(error) => {
                                                self.tx.send(Err(error.into()));
                                                return;
                                            }
                                        }
                                    }
                                    Err(fail) => self.tx.send(Err(fail.into())).unwrap_or_default(),
                                    _ => {
                                        unimplemented!("ResourceCreationChamber: it should not be possible to get any other message Result other than a Result::Ok(Reply::Id(_)) or Result::Err(Fail) when expecting ReplyKind::Id" )
                                    }
                                }
                            });
                        }
                        KeyCreationSrc::Key(key) => {
                            if key.parent() != Option::Some(self.parent.key.clone()) {
                                let final_create = self.finalize_create(key.clone(), address.clone() ).await;
                                self.tx.send(final_create);
                                return;
                            }
                        }
                    };
                }
            };
        });
    }

    async fn finalize_create(
        &self,
        key: ResourceKey,
        address: ResourcePath,
    ) -> Result<ResourceAction<AssignResourceStateSrc<DataSet<BinSrc>>>, Fail> {


        let stub = ResourceStub {
            key: key,
            address: address.clone(),
            archetype: self.create.archetype.clone(),
            owner: None,
        };

        let assign = ResourceAssign {
            kind: AssignKind::Create,
            stub: stub,
            state_src: self.create.state_src.clone(),
        };
        Ok(ResourceAction::Create(assign))
    }
}

#[async_trait]
pub trait ResourceHost: Send + Sync {
    fn star_key(&self) -> StarKey;
    async fn assign(
        &self,
        assign: ResourceAssign<AssignResourceStateSrc<DataSet<BinSrc>>>,
    ) -> Result<(), Error>;

    async fn init(
        &self,
        key: ResourceKey,
    ) -> Result<(), Error>;
}

pub struct ResourceNamesReservationRequest {
    pub info: Option<ResourceRegistryInfo>,
    pub parent: ResourceKey,
    pub archetype: ResourceArchetype,
}

pub struct RegistryReservation {
    tx: Option<oneshot::Sender<(ResourceRecord, oneshot::Sender<Result<(), Fail>>)>>,
}

impl RegistryReservation {
    pub fn commit(
        self,
        record: ResourceRecord,
        result_tx: oneshot::Sender<Result<(), Fail>>,
    ) -> Result<(), Fail> {
        if let Option::Some(tx) = self.tx {
            tx.send((record, result_tx))
                .or(Err(Fail::Error("could not send to tx".to_string())));
        }
        Ok(())
    }

    pub fn new(tx: oneshot::Sender<(ResourceRecord, oneshot::Sender<Result<(), Fail>>)>) -> Self {
        Self {
            tx: Option::Some(tx),
        }
    }

    pub fn empty() -> Self {
        RegistryReservation { tx: Option::None }
    }
}



pub struct FieldSelectionSql{
    selection: FieldSelection
}

impl From<FieldSelection> for FieldSelectionSql {
    fn from(selection: FieldSelection) -> Self {
        Self{
            selection
        }
    }
}

impl ToSql for FieldSelectionSql {
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, rusqlite::Error> {
        match self.to_sql_error() {
            Ok(ok) => Ok(ok),
            Err(err) => {
                eprintln!("{}", err.to_string());
                Err(rusqlite::Error::InvalidQuery)
            }
        }
    }
}

impl FieldSelectionSql {
    fn to_sql_error(&self) -> Result<ToSqlOutput<'_>, error::Error> {
        match &self.selection {
            FieldSelection::Identifier(id) => Ok(ToSqlOutput::Owned(Value::Blob(id.clone().key_or("(Identifier) selection fields must be turned into ResourceKeys before they can be used by the ResourceRegistry")?.bin()?))),
            FieldSelection::Type(resource_type) => {
                Ok(ToSqlOutput::Owned(Value::Text(resource_type.to_string())))
            }
            FieldSelection::Kind(kind) => Ok(ToSqlOutput::Owned(Value::Text(kind.to_string()))),
            FieldSelection::Specific(specific) => {
                Ok(ToSqlOutput::Owned(Value::Text(specific.to_string())))
            }
            FieldSelection::Owner(owner) => {
                Ok(ToSqlOutput::Owned(Value::Blob(owner.clone().bin()?)))
            }
            FieldSelection::Parent(parent_id) => Ok(ToSqlOutput::Owned(Value::Blob(parent_id.clone().key_or("(Parent) selection fields must be turned into ResourceKeys before they can be used by the ResourceRegistry")?.bin()?))),
        }
    }
}

pub struct RegistryUniqueSrc {
    parent_resource_type: ResourceType,
    parent_key: ResourceIdentifier,
    tx: mpsc::Sender<ResourceRegistryAction>,
}

impl RegistryUniqueSrc {
    pub fn new(parent_resource_type: ResourceType, parent_key: ResourceIdentifier, tx: mpsc::Sender<ResourceRegistryAction>) -> Self {
        RegistryUniqueSrc {
            parent_resource_type,
            parent_key: parent_key,
            tx: tx,
        }
    }
}

#[async_trait]
impl UniqueSrc for RegistryUniqueSrc {
    async fn next(&self, resource_type: &ResourceType) -> Result<ResourceId, Error> {
        if !resource_type
            .parents()
            .contains(&self.parent_resource_type)
        {
            eprintln!("WRONG RESOURCE TYPE IN UNIQUE SRC");
            return Err(Fail::WrongResourceType {
                //                expected: HashSet::from_iter(self.parent_key.resource_type().children()),
                expected: HashSet::new(),
                received: resource_type.clone(),
            }.into());
        }
        let (tx, rx) = oneshot::channel();

        let parent_key = match &self.parent_key {
            ResourceIdentifier::Key(key) => key.clone(),
            ResourceIdentifier::Address(address) => {
                let (tx, rx) = oneshot::channel();
                self.tx
                    .send(ResourceRegistryAction {
                        tx: tx,
                        command: ResourceRegistryCommand::Get(address.clone().into()),
                    })
                    .await?;
                if let ResourceRegistryResult::Resource(Option::Some(record)) = rx.await? {
                    record.stub.key
                } else {
                    return Err(
                        format!("could not find key for address: {}", address.to_string()).into(),
                    );
                }
            }
        };

        self.tx
            .send(ResourceRegistryAction {
                tx: tx,
                command: ResourceRegistryCommand::Next {
                    key: parent_key.clone(),
                    unique: Unique::Index,
                },
            })
            .await?;

        match rx.await? {
            ResourceRegistryResult::Unique(index) => Ok(resource_type.to_resource_id(index as _)),
            what => Err(Fail::Unexpected {
                expected: "ResourceRegistryResult::Unique".to_string(),
                received: what.to_string(),
            }.into()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRegistration {
    pub resource: ResourceRecord,
    pub info: Option<ResourceRegistryInfo>,
}

impl ResourceRegistration {
    pub fn new(resource: ResourceRecord, info: Option<ResourceRegistryInfo>) -> Self {
        ResourceRegistration {
            resource: resource,
            info: info,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceLocationAffinity {
    pub star: StarKey,
}

impl From<ResourceRecord> for ResourceKey {
    fn from(record: ResourceRecord) -> Self {
        record.stub.key
    }
}



pub enum ResourceManagerKey {
    Central,
    Key(ResourceKey),
}

/*
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct ResourceAddress {
    resource_type: ResourceType,
    parts: Vec<ResourceAddressPart>,
}

impl ResourceAddress {
    pub fn path(&self) -> Result<Path, Error> {
        if self.parts.len() == 0 {
            Path::new("/")
        } else if let ResourceAddressPart::Path(path) = self.parts.last().unwrap() {
            Ok(path.clone())
        } else {
            Path::new(base64::encode(self.parts.last().unwrap().to_string().as_bytes()).as_str())
        }
    }

    pub fn last(&self) -> Option<ResourceAddressPart> {
        self.parts.last().cloned()
    }
}


impl ResourceSelectorId for ResourceAddress {}


impl ResourceAddress {
    pub fn root() -> Self {
        Self {
            resource_type: ResourceType::Root,
            parts: vec![],
        }
    }

    pub fn last_to_string(&self) -> Result<String, Error> {
        Ok(self.parts.last().ok_or("couldn't find last")?.to_string())
    }

    pub fn parent(&self) -> Option<ResourceAddress> {
        match &self.resource_type {
            ResourceType::Root => Option::None,
            ResourceType::FileSystem => match self.parts.last().unwrap().is_wildcard() {
                true => self.chop(1, ResourceType::App),
                false => self.chop(2, ResourceType::SubSpace),
            },
            ResourceType::Database => match self.parts.last().unwrap().is_wildcard() {
                true => self.chop(1, ResourceType::App),
                false => self.chop(2, ResourceType::SubSpace),
            },
            ResourceType::Space => Option::Some(Self::root()),
            ResourceType::SubSpace => self.chop(1, ResourceType::Space),
            ResourceType::App => self.chop(1, ResourceType::SubSpace),
            ResourceType::Actor => self.chop(1, ResourceType::Actor),
            ResourceType::User => self.chop(1, ResourceType::User),
            ResourceType::File => self.chop(1, ResourceType::FileSystem),
            ResourceType::Domain => self.chop(1, ResourceType::SubSpace),
            ResourceType::UrlPathPattern => self.chop(1, ResourceType::Space),
            ResourceType::Proxy => self.chop(1, ResourceType::SubSpace),
            ResourceType::ArtifactBundle => self.chop(2, ResourceType::SubSpace),
            ResourceType::Artifact => self.chop(1, ResourceType::ArtifactBundle),
        }
    }

    fn chop(&self, indices: usize, resource_type: ResourceType) -> Option<Self> {
        let mut parts = self.parts.clone();
        for i in 0..indices {
            if !parts.is_empty() {
                parts.pop();
            }
        }
        Option::Some(Self {
            resource_type: resource_type,
            parts: parts,
        })
    }

    pub fn parent_resource_type(&self) -> Option<ResourceType> {
        match self.resource_type {
            ResourceType::Root => Option::None,
            ResourceType::Space => Option::Some(ResourceType::Root),
            ResourceType::SubSpace => Option::Some(ResourceType::Space),
            ResourceType::App => Option::Some(ResourceType::SubSpace),
            ResourceType::Actor => Option::Some(ResourceType::App),
            ResourceType::User => Option::Some(ResourceType::Space),
            ResourceType::FileSystem => match self.parts.last().unwrap().is_wildcard() {
                true => Option::Some(ResourceType::App),
                false => Option::Some(ResourceType::SubSpace),
            },
            ResourceType::File => Option::Some(ResourceType::FileSystem),
            ResourceType::Domain => Option::Some(ResourceType::Space),
            ResourceType::UrlPathPattern => Option::Some(ResourceType::Domain),
            ResourceType::Proxy => Option::Some(ResourceType::Space),
            ResourceType::ArtifactBundle => Option::Some(ResourceType::SubSpace),
            ResourceType::Artifact => Option::Some(ResourceType::ArtifactBundle),
            ResourceType::Database => match self.parts.last().unwrap().is_wildcard() {
                true => Option::Some(ResourceType::App),
                false => Option::Some(ResourceType::SubSpace),
            },
        }
    }
    /*
    pub fn from_filename(value: &str) -> Result<Self,Error>{
        let split = value.split("_");
    }

    pub fn to_filename(&self) -> String {
        let mut rtn = String::new();
        for (index,part) in self.parts.iter().enumerate() {
            if index != 0 {
                rtn.push_str("_" );
            }
            let part = match part {
                ResourceAddressPart::Wildcard => {
                    "~"
                }
                ResourceAddressPart::SkewerCase(skewer) => {
                    skewer.to_string()
                }
                ResourceAddressPart::Domain(domain) => {
                    domain.to_string()
                }
                ResourceAddressPart::Base64Encoded(base) => {
                    base.to_string()
                }
                ResourceAddressPart::Path(path) => {
                    path.to_relative().replace("/", "+")
                }
                ResourceAddressPart::Version(version) => {
                    version.to_string()
                }
                ResourceAddressPart::Email(email) => {
                    email.to_string()
                }
                ResourceAddressPart::Url(url) => {
                    url.replace("/", "+")
                }
                ResourceAddressPart::UrlPathPattern(pattern) => {
                    let result = Base64Encoded::encoded(pattern.to_string());
                    if result.is_ok() {
                        result.unwrap().encoded
                    }
                    else{
                        "+++"
                    }
                }
            };
            rtn.push_str(part);
        }
        rtn
    }

     */

    pub fn for_space(string: &str) -> Result<Self, Error> {
        ResourceType::Space.address_structure().from_str(string)
    }

    pub fn for_sub_space(string: &str) -> Result<Self, Error> {
        ResourceType::SubSpace.address_structure().from_str(string)
    }

    pub fn for_app(string: &str) -> Result<Self, Error> {
        ResourceType::App.address_structure().from_str(string)
    }

    pub fn for_actor(string: &str) -> Result<Self, Error> {
        ResourceType::Actor.address_structure().from_str(string)
    }

    pub fn for_filesystem(string: &str) -> Result<Self, Error> {
        ResourceType::FileSystem
            .address_structure()
            .from_str(string)
    }

    pub fn for_file(string: &str) -> Result<Self, Error> {
        ResourceType::File.address_structure().from_str(string)
    }

    pub fn for_user(string: &str) -> Result<Self, Error> {
        ResourceType::User.address_structure().from_str(string)
    }

    pub fn test_address(key: &ResourceKey) -> Result<Self, Error> {
        let mut parts = vec![];

        let mut mark = Option::Some(key.clone());
        while let Option::Some(key) = mark {
            match &key {
                ResourceKey::Root => {
                    // do nothing
                }
                ResourceKey::Space(space) => {
                    parts.push(ResourceAddressPart::SkewerCase(SkewerCase::new(
                        format!("space-{}", space.id()).as_str(),
                    )?));
                }
                ResourceKey::SubSpace(sub_space) => {
                    parts.push(ResourceAddressPart::SkewerCase(SkewerCase::new(
                        format!("sub-{}", sub_space.id).as_str(),
                    )?));
                }
                ResourceKey::App(app) => {
                    parts.push(app.address_part()?);
                }
                ResourceKey::Actor(actor) => {
                    parts.push(actor.address_part()?);
                }
                ResourceKey::User(user) => {
                    parts.push(ResourceAddressPart::SkewerCase(SkewerCase::new(
                        format!("user-{}", user.id).as_str(),
                    )?));
                }
                ResourceKey::File(file) => {
                    parts.push(ResourceAddressPart::Path(Path::new(
                        format!("/files/{}", file.id).as_str(),
                    )?));
                }
                ResourceKey::FileSystem(filesystem) => match filesystem {
                    FileSystemKey::App(app) => {
                        parts.push(ResourceAddressPart::SkewerCase(SkewerCase::new(
                            format!("filesystem-{}", app.id).as_str(),
                        )?));
                    }
                    FileSystemKey::SubSpace(sub_space) => {
                        parts.push(ResourceAddressPart::SkewerCase(SkewerCase::new(
                            format!("filesystem-{}", sub_space.id).as_str(),
                        )?));
                        parts.push(ResourceAddressPart::Wildcard);
                    }
                },
                ResourceKey::Domain(domain) => {
                    parts.push(ResourceAddressPart::Domain(DomainCase::new(
                        format!("domain-{}", domain.id).as_str(),
                    )?));
                }
                ResourceKey::UrlPathPattern(pattern) => {
                    parts.push(ResourceAddressPart::UrlPathPattern(format!(
                        "url-path-pattern-{}",
                        pattern.id
                    )));
                }
                ResourceKey::Proxy(proxy) => {
                    parts.push(ResourceAddressPart::SkewerCase(SkewerCase::from_str(
                        format!("proxy-{}", proxy.id).as_str(),
                    )?));
                }
                ResourceKey::ArtifactBundle(bundle) => {
                    parts.push(ResourceAddressPart::SkewerCase(SkewerCase::from_str(
                        format!("artifact-bundle-{}", bundle.id).as_str(),
                    )?));
                    parts.push(ResourceAddressPart::Version(Version::from_str("1.0.0")?));
                }
                ResourceKey::Artifact(artifact) => {
                    parts.push(ResourceAddressPart::Path(Path::new(
                        format!("/artifacts/{}", artifact.id).as_str(),
                    )?));
                }
                ResourceKey::Database(_) => {
                    unimplemented!()
                }
            }

            mark = key.parent();
        }
        Ok(ResourceAddress::from_parts(&key.resource_type(), parts)?)
    }

    pub fn resource_type(&self) -> ResourceType {
        self.resource_type.clone()
    }

    pub fn space(&self) -> Result<ResourceAddress, Error> {
        Ok(SPACE_ADDRESS_STRUCT.from_str(
            self.parts
                .get(0)
                .ok_or("expected space")?
                .to_string()
                .as_str(),
        )?)
    }

    pub fn sub_space(&self) -> Result<ResourceAddress, Error> {
        if self.resource_type == ResourceType::Space {
            Err("Space ResourceAddress does not have a SubSpace".into())
        } else {
            Ok(SUB_SPACE_ADDRESS_STRUCT.from_str(
                format!(
                    "{}:{}",
                    self.parts.get(0).ok_or("expected space")?.to_string(),
                    self.parts.get(1).ok_or("expected sub_space")?.to_string()
                )
                .as_str(),
            )?)
        }
    }
    pub fn from_parent(
        resource_type: &ResourceType,
        parent: Option<&ResourceAddress>,
        part: ResourceAddressPart,
    ) -> Result<ResourceAddress, Error> {
        if !resource_type.parent().matches_address(parent) {
            return Err(format!(
                "resource type parent is wrong: expected: {}",
                resource_type.parent().to_string()
            )
            .into());
        }

        let mut parts = vec![];
        if let Option::Some(parent) = parent {
            parts.append(&mut parent.parts.clone());
        }
        parts.push(part);

        Self::from_parts(resource_type, parts)
    }

    pub fn from_parts(
        resource_type: &ResourceType,
        mut parts: Vec<ResourceAddressPart>,
    ) -> Result<ResourceAddress, Error> {
        for (index, part_struct) in resource_type.address_structure().parts.iter().enumerate() {
            let part = parts.get(index).ok_or("missing part")?;
            if !part_struct.kind.matches(part) {
                return Err(format!("part does not match {}", part_struct.kind.to_string()).into());
            }
        }

        Ok(ResourceAddress {
            parts: parts,
            resource_type: resource_type.clone(),
        })
    }

    pub fn part_to_string(&self, name: &str) -> Result<String, Error> {
        for (index, part_struct) in self
            .resource_type
            .address_structure()
            .parts
            .iter()
            .enumerate()
        {
            if part_struct.name == name.to_string() {
                let part = self.parts.get(index).ok_or(format!(
                    "missing part index {} for part name {}",
                    index, name
                ))?;
                return Ok(part.to_string());
            }
        }

        Err(format!("could not find part with name {}", name).into())
    }

    pub fn to_parts_string(&self) -> String {
        let mut rtn = String::new();

        for (index, part) in self.parts.iter().enumerate() {
            if index != 0 {
                rtn.push_str(RESOURCE_ADDRESS_DELIM)
            }
            rtn.push_str(part.to_string().as_str());
        }
        rtn
    }
}

impl ToString for ResourceAddress {
    fn to_string(&self) -> String {
        if self.resource_type == ResourceType::Root {
            return "<Root>".to_string();
        } else {
            let mut rtn = self.to_parts_string();

            rtn.push_str("::");
            rtn.push_str("<");
            rtn.push_str(self.resource_type.to_string().as_str());
            rtn.push_str(">");

            rtn
        }
    }
}

impl FromStr for ResourceAddress {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.trim() == "<Root>" {
            return Ok(ResourceAddress {
                parts: vec![],
                resource_type: ResourceType::Root,
            });
        }

        let mut split = s.split("::<");
        let address_structure = split
            .next()
            .ok_or("missing address structure at beginning i.e: 'space::sub_space::<SubSpace>")?;
        let mut resource_type_gen = split
            .next()
            .ok_or("missing resource type at end i.e.: 'space::sub_space::<SubSpace>")?
            .to_string();

        // chop off the generics i.e. <Space> remove '<' and '>'
        if resource_type_gen.len() < 1 {
            return Err(
                format!("not a valid resource type generic '{}'", resource_type_gen).into(),
            );
        }
        //        resource_type_gen.remove(0);
        resource_type_gen.remove(resource_type_gen.len() - 1);

        let resource_type = ResourceType::from_str(resource_type_gen.as_str())?;
        let resource_address = resource_type
            .address_structure()
            .from_str(address_structure)?;

        Ok(resource_address)
    }
}

*/
#[derive(Clone, Serialize, Deserialize)]
pub struct ResourceBinding {
    pub key: ResourceKey,
    pub address: ResourceAddress,
}

#[derive(Debug,Clone, Serialize, Deserialize)]
pub struct ResourceLocation {
    pub star: StarKey
}

impl ResourceLocation {
    pub fn new(star: StarKey) -> Self {
        Self{
            star
        }
    }
    pub fn root()-> Self {
        Self{
            star: StarKey::central()
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct DisplayValue {
    string: String,
}

impl DisplayValue {
    pub fn new(string: &str) -> Result<Self, Error> {
        if string.is_empty() {
            return Err("cannot be empty".into());
        }

        Ok(DisplayValue {
            string: string.to_string(),
        })
    }
}

impl ToString for DisplayValue {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl FromStr for DisplayValue {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(DisplayValue::new(s)?)
    }
}

#[derive(Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum ResourceSliceStatus {
    Unknown,
    Preparing,
    Waiting,
    Ready,
}

impl ToString for ResourceSliceStatus {
    fn to_string(&self) -> String {
        match self {
            ResourceSliceStatus::Unknown => "Unknown".to_string(),
            ResourceSliceStatus::Preparing => "Preparing".to_string(),
            ResourceSliceStatus::Waiting => "Waiting".to_string(),
            ResourceSliceStatus::Ready => "Ready".to_string(),
        }
    }
}

impl FromStr for ResourceSliceStatus {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Unknown" => Ok(Self::Unknown),
            "Preparing" => Ok(Self::Preparing),
            "Waiting" => Ok(Self::Waiting),
            "Ready" => Ok(Self::Ready),
            what => Err(format!("not recognized: {}", what).into()),
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct ResourceSliceAssign {
    key: ResourceKey,
    archetype: ResourceArchetype,
}

/*j
impl TryInto<ResourceAssign<DataSet<BinSrc>>> for ResourceAssign<AssignResourceStateSrc<DataSet<BinSrc>>> {
    type Error = Error;

    fn try_into(self) -> Result<ResourceAssign<DataSet<BinSrc>>, Self::Error> {
        Ok(ResourceAssign {
            stub: self.stub,
            state_src: self.state_src.try_into()?,
        })
    }
}

 */


pub struct RemoteResourceHost {
    pub skel: StarSkel,
    pub handle: StarConscript,
}

#[async_trait]
impl ResourceHost for RemoteResourceHost {
    fn star_key(&self) -> StarKey {
        self.handle.key.clone()
    }

    async fn assign(
        &self,
        assign: ResourceAssign<AssignResourceStateSrc<DataSet<BinSrc>>>,
    ) -> Result<(), Error> {
        if !self
            .handle
            .kind
            .hosted()
            .contains(&assign.stub.key.resource_type())
        {
            return Err(Fail::WrongResourceType {
                expected: self.handle.kind.hosted().clone(),
                received: assign.stub.key.resource_type().clone(),
            }.into());
        }

        let mut proto = ProtoStarMessage::new();
        proto.to = self.handle.key.clone().into();
        proto.payload =
            StarMessagePayload::ResourceHost(ResourceHostAction::Assign(assign.try_into()?));

        self.skel
            .messaging_api
            .exchange(
                proto,
                ReplyKind::Empty,
                "RemoteResourceHost: assign resource to host",
            )
            .await?;

        Ok(())
    }

    async fn init(&self, key: ResourceKey) -> Result<(), Error> {
        let mut proto = ProtoStarMessage::new();
        proto.to = self.handle.key.clone().into();
        proto.payload =
            StarMessagePayload::ResourceHost(ResourceHostAction::Init(key));

        self.skel
            .messaging_api
            .exchange(
                proto,
                ReplyKind::Empty,
                "RemoteResourceHost: create resource on host",
            )
            .await?;

        Ok(())
    }
}

pub trait ResourceSelectorId:
    Debug
    + Clone
    + Serialize
    + for<'de> Deserialize<'de>
    + Eq
    + PartialEq
    + Hash
    + Into<ResourceIdentifier>
    + Sized
{
}

#[async_trait]
pub trait UniqueSrc: Send + Sync {
    async fn next(&self, resource_type: &ResourceType) -> Result<ResourceId, Error>;
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceRecord {
    pub stub: ResourceStub,
    pub location: ResourceLocation,
}

impl ResourceRecord {
    pub fn new(stub: ResourceStub, host: StarKey) -> Self {
        ResourceRecord {
            stub: stub,
            location: ResourceLocation::new(host),
        }
    }

    pub fn root() -> Self {
        Self {
            stub: ResourceStub::root(),
            location: ResourceLocation::root(),
        }
    }
}

impl Into<ResourceStub> for ResourceRecord {
    fn into(self) -> ResourceStub {
        self.stub
    }
}

    pub async fn to_keyed_for_field_selection( selection: FieldSelection, starlane_api: &StarlaneApi) -> Result<FieldSelection, Error> {
        match selection{
            FieldSelection::Identifier(id) => Ok(FieldSelection::Identifier(
                starlane_api.to_key(id).await?.into(),
            )),
            FieldSelection::Type(resource_type) => Ok(FieldSelection::Type(resource_type)),
            FieldSelection::Kind(kind) => Ok(FieldSelection::Kind(kind)),
            FieldSelection::Specific(specific) => Ok(FieldSelection::Specific(specific)),
            FieldSelection::Owner(owner) => Ok(FieldSelection::Owner(owner)),
            FieldSelection::Parent(id) => Ok(FieldSelection::Parent(
                starlane_api.to_key(id).await?.into(),
            )),
        }
    }


pub async fn to_keyed_for_reasource_create(create: ResourceCreate, starlane_api: StarlaneApi) -> Result<ResourceCreate, Error> {
    Ok(ResourceCreate{
        parent: starlane_api.to_key(create.parent).await?.into(),
        key: create.key,
        address: create.address,
        archetype: create.archetype,
        state_src: create.state_src,
        registry_info: create.registry_info,
        owner: create.owner,
        strategy: create.strategy,
        from: create.from
    })
}

pub async fn to_keyed_for_resource_selector(selector: ResourceSelector, starlane_api: StarlaneApi) -> Result<ResourceSelector, Error> {
    let mut fields: HashSet<FieldSelection> = HashSet::new();

    for field in selector.fields {
        fields.insert(to_keyed_for_field_selection(field,&starlane_api).await?.into());
    }

    Ok(ResourceSelector {
        meta: selector.meta,
        fields: fields,
    })
}
