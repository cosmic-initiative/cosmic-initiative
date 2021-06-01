use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::FromIterator;
use std::str::FromStr;
use std::string::FromUtf8Error;
use std::sync::Arc;
use std::time::Duration;

use base64::DecodeError;
use bincode::ErrorKind;
use rusqlite::{Connection, params, params_from_iter, Rows, Statement, ToSql, Transaction};
use rusqlite::types::{ToSqlOutput, Value, ValueRef};
use serde::{Deserialize, Serialize};
use serde_json::to_string;
use tokio::sync::{mpsc, oneshot};

use crate::actor::{ActorArchetype, ActorKey, ActorKind, ActorProfile};
use crate::app::{AppArchetype, AppKind, AppProfile, ConfigSrc, InitData};
use crate::artifact::{ArtifactKey, ArtifactKind};
use crate::error::Error;
use crate::frame::{Reply, ResourceHostAction, SimpleReply, StarMessagePayload};
use crate::id::{Id, IdSeq};
use crate::keys::{FileKey, ResourceId, UniqueSrc, Unique};
use crate::keys::{AppFilesystemKey, AppKey, FileSystemKey, GatheringKey, ResourceKey, SpaceKey, SubSpaceFilesystemKey, SubSpaceId, SubSpaceKey, UserKey};
use crate::message::{Fail, ProtoMessage};
use crate::names::{Name, Specific};
use crate::permissions::User;
use crate::star::{ResourceRegistryBacking, StarComm, StarCommand, StarKey, StarSkel, StarKind};
use crate::star::pledge::{StarHandle, ResourceHostSelector};
use crate::util::AsyncHashMap;
use tokio::sync::oneshot::Receiver;

pub mod space;
pub mod sub_space;
pub mod user;

lazy_static!
{
    pub static ref NOTHING_STRUCT :ResourceAddressStructure = ResourceAddressStructure::new( vec![], ResourceType::Nothing );

    pub static ref SPACE_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new( vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer)], ResourceType::Space );
    pub static ref SUB_SPACE_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new( vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer)], ResourceType::SubSpace );

     pub static ref APP_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new(      vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("app",ResourceAddressPartKind::Skewer)], ResourceType::App );

     pub static ref ACTOR_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new(    vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("app",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("actor",ResourceAddressPartKind::Skewer)], ResourceType::Actor );

     pub static ref USER_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new(     vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("user",ResourceAddressPartKind::Skewer)], ResourceType::User );


     pub static ref FILE_SYSTEM_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure::new(vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("app",ResourceAddressPartKind::WildcardOrSkewer),
                                                                                                           ResourceAddressPartStruct::new("file-system",ResourceAddressPartKind::Skewer)], ResourceType::FileSystem );


     pub static ref FILE_ADDRESS_STRUCT:ResourceAddressStructure =      ResourceAddressStructure::new(vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("app",ResourceAddressPartKind::WildcardOrSkewer),
                                                                                                           ResourceAddressPartStruct::new("file-system",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("path",ResourceAddressPartKind::Path)], ResourceType::File );


     pub static ref ARTIFACT_ADDRESS_STRUCT:ResourceAddressStructure = ResourceAddressStructure ::new(vec![ResourceAddressPartStruct::new("space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("sub-space",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("bundle",ResourceAddressPartKind::Skewer),
                                                                                                           ResourceAddressPartStruct::new("version",ResourceAddressPartKind::Version),
                                                                                                           ResourceAddressPartStruct::new("path",ResourceAddressPartKind::Path)], ResourceType::Artifact );

    pub static ref HYPERSPACE_ADDRESS: ResourceAddress = SPACE_ADDRESS_STRUCT.from_str("hyperspace").unwrap();
    pub static ref HYPERSPACE_DEFAULT_ADDRESS: ResourceAddress = SUB_SPACE_ADDRESS_STRUCT.from_str("hyperspace:default").unwrap();
}

pub type Labels = HashMap<String,String>;
pub type Names = Vec<String>;



#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceSelector
{
    pub meta: MetaSelector,
    pub fields: HashSet<FieldSelection>
}

#[derive(Clone,Serialize,Deserialize)]
pub enum MetaSelector
{
    None,
    Name(String),
    Label(LabelSelector)
}

#[derive(Clone,Serialize,Deserialize)]
pub struct LabelSelector
{
    pub labels: HashSet<LabelSelection>
}

impl ResourceSelector {
    pub fn new()-> ResourceSelector {
        ResourceSelector {
            meta: MetaSelector::None,
            fields: HashSet::new()
        }
    }

    pub fn resource_types(&self)->HashSet<ResourceType>
    {
        let mut rtn = HashSet::new();
        for field in &self.fields
        {
            if let FieldSelection::Type(resource_type) = field
            {
                rtn.insert(resource_type.clone());
            }
        }
        rtn
    }

    pub fn add( &mut self, field: FieldSelection )
    {
        self.fields.retain( |f| !f.is_matching_kind(&field));
        self.fields.insert(field);
    }

    pub fn is_empty(&self) -> bool
    {
        if !self.fields.is_empty()
        {
            return false;
        }

        match &self.meta
        {
            MetaSelector::None => {
                return true;
            }
            MetaSelector::Name(_) => {
                return false;
            }
            MetaSelector::Label(labels) => {
                return labels.labels.is_empty();
            }
        };
    }

    pub fn name( &mut self, name: String ) -> Result<(),Error>
    {
        match &mut self.meta
        {
            MetaSelector::None => {
                self.meta = MetaSelector::Name(name.clone());
                Ok(())
            }
            MetaSelector::Name(_) => {
                self.meta = MetaSelector::Name(name.clone());
                Ok(())
            }
            MetaSelector::Label(selector) => {
                Err("Selector is already set to a LABEL meta selector".into())

            }
        }
    }

    pub fn add_label( &mut self, label: LabelSelection ) -> Result<(),Error>
    {
        match &mut self.meta
        {
            MetaSelector::None => {
                self.meta = MetaSelector::Label(LabelSelector{
                    labels : HashSet::new()
                });
                self.add_label(label)
            }
            MetaSelector::Name(_) => {
                Err("Selector is already set to a NAME meta selector".into())
            }
            MetaSelector::Label(selector) => {
                selector.labels.insert( label );
                Ok(())
            }
        }
    }

    pub fn add_field( &mut self, field: FieldSelection )
    {
        self.fields.insert(field);
    }
}

pub type AppSelector = ResourceSelector;
pub type ActorSelector = ResourceSelector;

impl ResourceSelector {

    pub fn app_selector()->AppSelector {
      let mut selector = AppSelector::new();
      selector.add(FieldSelection::Type(ResourceType::App));
      selector
    }

    pub fn actor_selector()->ActorSelector {
        let mut selector = ActorSelector::new();
        selector.add(FieldSelection::Type(ResourceType::Actor));
        selector
    }

}

#[derive(Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub enum LabelSelection
{
    Exact(Label)
}

impl LabelSelection
{
    pub fn exact( name: &str, value: &str )->Self
    {
        LabelSelection::Exact(Label{
            name: name.to_string(),
            value: value.to_string()
        })
    }
}


#[derive(Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub enum FieldSelection
{
    Type(ResourceType),
    Kind(ResourceKind),
    Specific(Specific),
    Owner(UserKey),
    Space(SpaceKey),
    SubSpace(SubSpaceKey),
    App(AppKey),
}


impl ToSql for Name
{
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, rusqlite::Error> {
        Ok(ToSqlOutput::Owned(Value::Text(self.to())))
    }
}

impl FieldSelection
{
    pub fn is_matching_kind(&self, field: &FieldSelection ) ->bool
    {
        match self
        {
            FieldSelection::Type(_) => {
                if let FieldSelection::Type(_) = field {
                    return true;
                }
            }
            FieldSelection::Kind(_) => {
                if let FieldSelection::Kind(_) = field {
                    return true;
                }
            }
            FieldSelection::Specific(_) => {
                if let FieldSelection::Specific(_) = field {
                    return true;
                }
            }
            FieldSelection::Owner(_) => {
                if let FieldSelection::Owner(_) = field {
                    return true;
                }
            }
            FieldSelection::Space(_) => {
                if let FieldSelection::Space(_) = field {
                    return true;
                }
            }
            FieldSelection::SubSpace(_) => {
                if let FieldSelection::SubSpace(_) = field {
                    return true;
                }
            }
            FieldSelection::App(_) => {
                if let FieldSelection::App(_) = field {
                    return true;
                }
            }
        };
        return false;
    }
}

impl ToSql for FieldSelection
{
    fn to_sql(&self) -> Result<ToSqlOutput<'_>, rusqlite::Error> {
        match self
        {
            FieldSelection::Type(resource_type) => {
                Ok(ToSqlOutput::Owned(Value::Text(resource_type.to_string())))
            }
            FieldSelection::Kind(kind) => {
                Ok(ToSqlOutput::Owned(Value::Text(kind.to_string())))
            }
            FieldSelection::Specific(specific) => {
                Ok(ToSqlOutput::Owned(Value::Text(specific.to_string())))
            }
            FieldSelection::Owner(owner) => {
                Ok(ToSqlOutput::Owned(Value::Blob(owner.clone().bin()?)))
            }
            FieldSelection::Space(space) => {
                Ok(ToSqlOutput::Owned(Value::Integer(space.id() as _)))
            }
            FieldSelection::SubSpace(sub_space) => {
                Ok(ToSqlOutput::Owned(Value::Blob(ResourceKey::SubSpace(sub_space.clone()).bin()?)))
            }
            FieldSelection::App(app) => {
                Ok(ToSqlOutput::Owned(Value::Blob(ResourceKey::App(app.clone()).bin()?)))
            }
        }
    }
}

#[derive(Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub struct Label
{
    pub name: String,
    pub value: String
}

#[derive(Clone,Serialize,Deserialize)]
pub struct LabelConfig
{
    pub name: String,
    pub index: bool
}

pub struct ResourceRegistryAction
{
    pub tx: oneshot::Sender<ResourceRegistryResult>,
    pub command: ResourceRegistryCommand
}

impl ResourceRegistryAction
{
    pub fn new(command: ResourceRegistryCommand) ->(Self, oneshot::Receiver<ResourceRegistryResult>)
    {
        let (tx,rx) = oneshot::channel();
        (ResourceRegistryAction { tx: tx, command: command }, rx)
    }
}

pub enum ResourceRegistryCommand
{
    Close,
    Clear,
    Accepts(HashSet<ResourceType>),
    Reserve(ResourceNamesReservationRequest),
    Commit(ResourceRegistration),
    Select(ResourceSelector),
    SetLocation(ResourceLocationRecord),
    Find(ResourceKey),
    Bind(ResourceBinding),
    GetAddress(ResourceKey),
    GetKey(ResourceAddress),
    Next{key:ResourceKey,unique:Unique}
}

pub enum ResourceRegistryResult
{
    Ok,
    Error(String),
    Resource(ResourceStub),
    Resources(Vec<ResourceStub>),
    Address(ResourceAddress),
    Location(ResourceLocationRecord),
    Reservation(RegistryReservation),
    Key(ResourceKey),
    Unique(u64),
    NotFound,
    NotAccepted
}

type Blob = Vec<u8>;

struct RegistryParams {
   key: Option<Blob>,
   resource_type: String,
   kind: String,
   specific: Option<String>,
   space: Option<u32>,
   sub_space: Option<Blob>,
   config: Option<String>,
   app: Option<Blob>,
   owner: Option<Blob>
}

impl RegistryParams {
    pub fn from_registration(registration: ResourceRegistration) -> Result<Self, Error> {
        Self::new( registration.resource.archetype, registration.resource.key.parent().ok_or("cannot register the parent of a nothing ResourceType::Nothing")?, Option::Some(registration.resource.key), registration.resource.owner)
    }

    pub fn from_archetype(archetype: ResourceArchetype, parent: ResourceKey ) -> Result<Self, Error> {
        Self::new( archetype, parent, Option::None, Option::None )
    }

    pub fn new(archetype: ResourceArchetype, parent: ResourceKey,  key: Option<ResourceKey>, owner: Option<UserKey>) -> Result<Self, Error> {
        let key = if let Option::Some(key) = key
        {
            Option::Some(key.bin()?)
        } else {
            Option::None
        };

        let resource_type = archetype.kind.resource_type().to_string();
            let kind = archetype.kind.to_string();

            let owner = if let Option::Some(owner) = owner
            {
                Option::Some(owner.bin()?)
            } else {
                Option::None
            };



            let specific = match &archetype.specific {
                None => Option::None,
                Some(specific) => {
                    Option::Some(specific.to_string())
                }
            };


            let config = match &archetype.config {
                None => Option::None,
                Some(config) => {
                    Option::Some(config.to_string())
                }
            };



            let space = match parent.resource_type(){
                ResourceType::Nothing => Option::None,
                ResourceType::Space => Option::None,
                _ => Option::Some(parent.space()?.id())
            };

            let sub_space = match parent.sub_space()
                {
                    Ok(sub_space) => {
                        Option::Some(ResourceKey::SubSpace(sub_space).bin()?)
                    }
                    Err(_) => Option::None
                };

            let app =
                match parent.app()
                {
                    Ok(app) => {
                        Option::Some(ResourceKey::App(app).bin()?)
                    }
                    Err(_) => Option::None
                };


        Ok(RegistryParams {
                key: key,
                resource_type: resource_type,
                kind: kind,
                specific: specific,
                space: space,
                sub_space: sub_space,
                config: config,
                app: app,
                owner: owner
            })

        }
}



pub struct Registry {
   pub conn: Connection,
   pub tx: mpsc::Sender<ResourceRegistryAction>,
   pub rx: mpsc::Receiver<ResourceRegistryAction>,
   pub accepted: Option<HashSet<ResourceType>>
}

impl Registry {
    pub async fn new() -> mpsc::Sender<ResourceRegistryAction>
    {
        let (tx, rx) = mpsc::channel(8 * 1024);

        let tx_clone = tx.clone();
        tokio::spawn(async move {

            let conn = Connection::open_in_memory();
            if conn.is_ok()
            {
                let mut db = Registry {
                    conn: conn.unwrap(),
                    tx: tx_clone,
                    rx: rx,
                    accepted: Option::None
                };
                db.run().await.unwrap();
            }
        });
        tx
    }

    async fn run(&mut self) -> Result<(), Error>
    {
        match self.setup()
        {
            Ok(_) => {}
            Err(err) => {
eprintln!("error setting up db: {}", err );
                return Err(err);
            }
        };

        while let Option::Some(request) = self.rx.recv().await {
            if let ResourceRegistryCommand::Close = request.command
            {
                break;
            }
            match self.process( request.command )
            {
                Ok(ok) => {
                    request.tx.send(ok);
                }
                Err(err) => {
                    eprintln!("{}",err);
                    request.tx.send(ResourceRegistryResult::Error(err.to_string()));
                }
            }
        }

        Ok(())
    }

    fn accept( &self, resource_type: ResourceType )->bool
    {
        if self.accepted.is_none() {
            return true;
        }

        let accepted = self.accepted.as_ref().unwrap();

        return accepted.contains(&resource_type);
    }


    fn process(&mut self, command: ResourceRegistryCommand) -> Result<ResourceRegistryResult, Error> {
        match command
        {
            ResourceRegistryCommand::Close => {
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Clear => {
                let trans = self.conn.transaction()?;
                trans.execute("DELETE FROM labels", [] )?;
                trans.execute("DELETE FROM names", [] )?;
                trans.execute("DELETE FROM resources", [])?;
                trans.execute("DELETE FROM locations", [])?;
                trans.execute("DELETE FROM slices", [])?;
                trans.execute("DELETE FROM uniques", [])?;
                trans.commit()?;

                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Accepts(accept)=> {
                self.accepted= Option::Some(accept);
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Commit(registration) => {
                if !self.accept(registration.resource.key.resource_type() ) {
                    return Ok(ResourceRegistryResult::NotAccepted);
                }
                let params= RegistryParams::from_registration(registration.clone())?;

                let trans = self.conn.transaction()?;

                if params.key.is_some() {
                    trans.execute("DELETE FROM labels WHERE labels.resource_key=?1", [params.key.clone()]);
                    trans.execute("DELETE FROM resources WHERE key=?1", [params.key.clone()])?;
                }

                trans.execute("INSERT INTO resources (key,resource_type,kind,specific,space,sub_space,owner,app,config) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9)", params![params.key,params.resource_type,params.kind,params.specific,params.space,params.sub_space,params.owner,params.app,params.config])?;
                if !registration.info.names.is_empty() {
                    for name in registration.info.names
                    {
                        trans.execute("UPDATE names SET key=?1 WHERE name=?1", [name] )?;
                    }
                }

                for (name, value) in registration.info.labels
                {
                    trans.execute("INSERT INTO labels (resource_key,name,value) VALUES (?1,?2,?3)", params![params.key,name, value])?;
                }

                trans.commit()?;
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Select(selector) => {

                for resource_type in selector.resource_types() {
                    if !self.accept(resource_type ) {
                        return Ok(ResourceRegistryResult::NotAccepted);
                    }
                }

                let mut params = vec![];
                let mut where_clause = String::new();

                for (index, field) in Vec::from_iter(selector.fields.clone()).iter().map(|x| x.clone() ).enumerate()
                {
                    if index != 0 {
                        where_clause.push_str(" AND ");
                    }

                    let f = match field {
                        FieldSelection::Type(_) => {
                            format!("resource_type=?{}", index + 1)
                        }
                        FieldSelection::Kind(_) => {
                            format!("kind=?{}", index + 1)
                        }
                        FieldSelection::Specific(_) => {
                            format!("specific=?{}", index + 1)
                        }
                        FieldSelection::Owner(_) => {
                            format!("owner=?{}", index + 1)
                        }
                        FieldSelection::Space(_) => {
                            format!("space=?{}", index + 1)
                        }
                        FieldSelection::SubSpace(_) => {
                            format!("sub_space=?{}", index + 1)
                        }
                        FieldSelection::App(_) => {
                            format!("app=?{}", index + 1)
                        }

                    };
                    where_clause.push_str(f.as_str());
                    params.push(field);
                }

                if !params.is_empty() {
                    where_clause.push_str(" AND ");
                }

                where_clause.push_str( " key IS NOT NULL");

                let mut statement = match &selector.meta
                {
                    MetaSelector::None => {
                        format!("SELECT DISTINCT r.key,r.kind,r.specific,r.owner,r.config FROM resources as r WHERE {}", where_clause )
                    }
                    MetaSelector::Label(label_selector) => {

                        let mut labels = String::new();
                        for (index, label_selection ) in Vec::from_iter(label_selector.labels.clone() ).iter().map(|x| x.clone() ).enumerate()
                        {
                            if let LabelSelection::Exact(label) = label_selection
                            {
                                labels.push_str(format!(" AND r.key,r.kind,r.specific,r.owner,r.config IN (SELECT labels.resource_key FROM labels WHERE labels.name='{}' AND labels.value='{}')", label.name, label.value).as_str())
                            }
                        }

                        format!("SELECT DISTINCT r.key,r.kind,r.specific,r.owner,r.config FROM resources as r WHERE {} {}", where_clause, labels )
                    }
                    MetaSelector::Name(name) => {
                        if where_clause.is_empty() {
                            format!("SELECT DISTINCT r.key,r.kind,r.specific,r.owner,r.config FROM names as r WHERE r.name='{}'", name)
                        }
                        else {
                            format!("SELECT DISTINCT r.key,r.kind,r.specific,r.owner,r.config FROM names as r WHERE {} AND r.name='{}'", where_clause, name)
                        }
                    }
                };

                // in case this search was for EVERYTHING
                if selector.is_empty()
                {
                    statement = "SELECT DISTINCT r.key,r.kind,r.specific,r.owner,r.config FROM resources as r".to_string();
                }

                println!("STATEMENT {}",statement);

                let mut statement = self.conn.prepare(statement.as_str())?;
                let mut rows= statement.query( params_from_iter(params.iter() ) )?;

                let mut resources = vec![];
                while let Option::Some(row) = rows.next()?
                {
                    let key:Vec<u8> = row.get(0)?;
                    let key = ResourceKey::from_bin(key)?;

                    let kind:String = row.get(1)?;
                    let kind = ResourceKind::from_str(kind.as_str())?;

                    let specific= if let ValueRef::Null = row.get_ref(2)? {
                        Option::None
                    } else {
                        let specific: String = row.get(2)?;
                        let specific= Specific::from_str(specific.as_str())?;
                        Option::Some(specific)
                    };

                    let owner= if let ValueRef::Null = row.get_ref(3)? {
                        Option::None
                    } else {
                        let owner: Vec<u8> = row.get(3)?;
                        let owner: UserKey= UserKey::from_bin(owner)?;
                        Option::Some(owner)
                    };


                    let config= if let ValueRef::Null = row.get_ref(4)? {
                        Option::None
                    } else {
                        let config:String = row.get(4)?;
                        let config = ConfigSrc::from_str(config.as_str())?;
                        Option::Some(config)
                    };

                    let address: String = row.get(5)?;
                    let address = ResourceAddress::from_str(address.as_str())?;

                    let resource = ResourceStub {
                        key: key,
                        address: address,
                        archetype: ResourceArchetype {
                            kind:kind,
                            specific: specific,
                            config: config
                        },
                        owner: owner
                    };

                    resources.push(resource );
                }
                Ok(ResourceRegistryResult::Resources(resources) )
            }
            ResourceRegistryCommand::SetLocation(location_record) => {
                if !self.accept(location_record.key.resource_type()) {
                   return Ok(ResourceRegistryResult::NotAccepted);
                }

                let key = location_record.key.bin()?;
                let host = location_record.location.host.bin()?;
                let gathering = match location_record.location.gathering{
                    None => Option::None,
                    Some(key) => Option::Some(key.bin()?)
                };
                let mut trans = self.conn.transaction()?;
                trans.execute("INSERT INTO locations (key,host,gathering) VALUES (?1,?2,?3)", params![key,host,gathering])?;
                trans.commit()?;
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::Find(key) => {

                let key = key.bin()?;
                let statement = "SELECT (key,host,gathering) FROM locations WHERE key=?1";
                let mut statement = self.conn.prepare(statement)?;
                let result = statement.query_row( params![key], |row| {
                    let key: Vec<u8> = row.get(0)?;
                    let key = ResourceKey::from_bin(key)?;

                    let host: Vec<u8> = row.get(1)?;
                    let host = StarKey::from_bin(host)?;

                    let gathering = if let ValueRef::Null = row.get_ref(2)? {
                        Option::None
                    } else {
                        let gathering: Vec<u8> = row.get(2)?;
                        let gathering: GatheringKey = GatheringKey::from_bin(gathering)?;
                        Option::Some(gathering)
                    };
                    let location = ResourceLocationRecord {
                        key: key,
                        location: ResourceLocation {
                            host: host,
                            gathering: gathering
                        }
                    };
                    Ok(location)
                });

                match result
                {
                    Ok(location) => {
                        Ok(ResourceRegistryResult::Location(location))
                    }
                    Err(err) => {
                        Ok(ResourceRegistryResult::NotFound)
                    }
                }
            }
            ResourceRegistryCommand::Bind(bind) => {
                let key = bind.key.bin()?;
                let address = bind.address.to_string();

                let mut trans = self.conn.transaction()?;
                trans.execute("DELETE addresses WHERE key=?1 OR address=?2)", params![key,address])?;
                trans.execute("INSERT INTO addresses (key,address) VALUES (?1,?2)", params![key,address])?;
                trans.commit()?;
                Ok(ResourceRegistryResult::Ok)
            }
            ResourceRegistryCommand::GetAddress(key) => {
                let key_blob = key.bin()?;
                let statement = "SELECT address FROM addresses WHERE key=?1";
                let mut statement = self.conn.prepare(statement)?;
                let result = statement.query_row( params![key_blob], |row| {

                    let address: String = row.get(0)?;
                    let address=key.resource_type().address_structure().from_str(address.as_str())?;
                    Ok(address)
                });

                match result
                {
                    Ok(address) => {
                        Ok(ResourceRegistryResult::Address(address))
                    }
                    Err(err) => {
                        Ok(ResourceRegistryResult::NotFound)
                    }
                }
            }
            ResourceRegistryCommand::GetKey(address) => {
                let address = address.to_string();
                let statement = "SELECT key FROM addresses WHERE address=?1";
                let mut statement = self.conn.prepare(statement)?;
                let result = statement.query_row( params![address], |row| {

                    let key: Vec<u8>= row.get(0)?;
                    let key=ResourceKey::from_bin(key)?;
                    Ok(key)
                });

                match result
                {
                    Ok(key) => {
                        Ok(ResourceRegistryResult::Key(key))
                    }
                    Err(err) => {
                        Ok(ResourceRegistryResult::NotFound)
                    }
                }
            }
            ResourceRegistryCommand::Reserve(request) => {
                let trans = self.conn.transaction()?;
                trans.execute("DELETE FROM names WHERE key IS NULL AND datetime(reservation_timestamp) < datetime('now')", [] )?;
                let params = RegistryParams::new(request.archetype.clone(),request.parent.clone(),Option::None, Option::None)?;
                if request.info.is_some() {
                    let params = RegistryParams::from_archetype(request.archetype, request.parent)?;
                    Self::process_names(&trans, &request.info.unwrap().names, &params)?;
                }
                trans.commit()?;
                let (tx,rx) = oneshot::channel();
                let reservation = RegistryReservation::new(tx);
                tokio::spawn( async move {

                    if let Result::Ok( resource ) = rx.await {
                        let mut params = params;
                        let key = match resource.key.bin(){
                            Ok(key) => {
                                Option::Some(key)
                            }
                            Err(_) => Option::None
                        };
                        params.key = key
                    }
                } );
                Ok(ResourceRegistryResult::Reservation(reservation))
            }

            ResourceRegistryCommand::Next{key,unique} => {
                let mut trans = self.conn.transaction()?;
                let key = key.bin()?;
                let column = match unique{
                    Unique::Sequence => "sequence",
                    Unique::Index => "id_index"
                };
                trans.execute("INSERT OR IGNORE INTO uniques (key) VALUES (?1)", params![key])?;
                trans.execute(format!("UPDATE uniques SET {}={}+1 WHERE key=?1",column,column).as_str(), params![key])?;
                let rtn = trans.query_row(format!("SELECT {} FROM uniques WHERE key=?1",column).as_str(), params![key], |r| {
                    let rtn:u64 = r.get(0)?;
                    Ok(rtn)
                })?;

                trans.commit()?;

                Ok(ResourceRegistryResult::Unique(rtn))
            }
        }
    }

    fn process_names( trans: &Transaction, names: &Names, params: &RegistryParams) -> Result<(),Error> {
        for name in names{
            trans.execute("INSERT INTO names (key,name,resource_type,kind,specific,space,sub_space,owner,app,config,reservation_timestamp) VALUES (?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,timestamp('now','+5 minutes')", params![params.key,name,params.resource_type,params.kind,params.specific,params.space,params.sub_space,params.owner,params.app,params.config])?;
        }
        Ok(())
    }



    pub fn setup(&mut self)->Result<(),Error>
    {
      let labels= r#"
       CREATE TABLE IF NOT EXISTS labels (
	      key INTEGER PRIMARY KEY AUTOINCREMENT,
	      resource_key BLOB,
	      name TEXT NOT NULL,
	      value TEXT NOT NULL,
          UNIQUE(key,name),
          FOREIGN KEY (resource_key) REFERENCES resources (key)
        )"#;

        let names= r#"
       CREATE TABLE IF NOT EXISTS names(
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          key BLOB,
	      name TEXT NOT NULL,
	      resource_type TEXT NOT NULL,
          kind BLOB NOT NULL,
          specific TEXT,
          space INTEGER NOT NULL,
          sub_space BLOB NOT NULL,
          app TEXT,
          owner BLOB,
          reservation_timestamp TEXT,
          UNIQUE(name,resource_type,kind,specific,space,sub_space,app)
        )"#;



        let resources = r#"CREATE TABLE IF NOT EXISTS resources (
         key BLOB PRIMARY KEY,
         resource_type TEXT NOT NULL,
         kind BLOB NOT NULL,
         specific TEXT,
         space INTEGER NOT NULL,
         sub_space BLOB NOT NULL,
         app TEXT,
         owner BLOB
        )"#;

        let locations = r#"CREATE TABLE IF NOT EXISTS locations (
         key BLOB PRIMARY KEY,
         host BLOB NOT NULL,
         gathering BLOB
        )"#;

        let addresses = r#"CREATE TABLE IF NOT EXISTS addresses (
         key BLOB PRIMARY KEY,
         address TEXT NOT NULL,
         UNIQUE(address)
        )"#;

        let slices = r#"CREATE TABLE IF NOT EXISTS slices (
         key BLOB PRIMARY KEY,
         host BLOB NOT NULL,
         status TEXT NOT NULL,
         UNIQUE(key,host)
        )"#;

        let uniques = r#"CREATE TABLE IF NOT EXISTS uniques(
         key BLOB PRIMARY KEY,
         sequence INTEGER NOT NULL DEFAULT 0,
         id_index INTEGER NOT NULL DEFAULT 0
        )"#;

        /*
      let labels_to_resources = r#"CREATE TABLE IF NOT EXISTS labels_to_resources
        (
           resource_key BLOB,
           label_key INTEGER,
           PRIMARY KEY (resource_key, label_key),
           FOREIGN KEY (resource_key) REFERENCES resources (key),
           FOREIGN KEY (label_key) REFERENCES labels (key)
        )
        "#;

         */

        let transaction = self.conn.transaction()?;
        transaction.execute(labels, [])?;
        transaction.execute(names, [])?;
        transaction.execute(resources, [])?;
        transaction.execute(locations, [])?;
        transaction.execute(addresses, [])?;
        transaction.execute(slices, [])?;
        transaction.execute(uniques, [])?;
        transaction.commit()?;

        Ok(())
    }
}


#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum ResourceKind
{
    Nothing,
    Space,
    SubSpace,
    App(AppKind),
    Actor(ActorKind),
    User,
    File,
    FileSystem(FileSystemKind),
    Artifact(ArtifactKind)
}

impl ResourceKind{
    pub fn resource_type(&self) -> ResourceType{
       match self {
           ResourceKind::Nothing=> ResourceType::Nothing,
           ResourceKind::Space => ResourceType::Space,
           ResourceKind::SubSpace => ResourceType::SubSpace,
           ResourceKind::App(_) => ResourceType::App,
           ResourceKind::Actor(_) => ResourceType::Actor,
           ResourceKind::User => ResourceType::User,
           ResourceKind::File => ResourceType::File,
           ResourceKind::FileSystem(_) => ResourceType::FileSystem,
           ResourceKind::Artifact(_) => ResourceType::Artifact
       }
    }
}

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum FileSystemKind
{
    App,
    SubSpace
}

impl ResourceType
{
    pub fn magic(&self) -> u8
    {
        match self
        {
            ResourceType::Nothing => u8::max_value(),
            ResourceType::Space => 0,
            ResourceType::SubSpace => 1,
            ResourceType::App => 2,
            ResourceType::Actor => 3,
            ResourceType::User => 4,
            ResourceType::File => 5,
            ResourceType::Artifact => 6,
            ResourceType::FileSystem => 7
        }
    }

    pub fn from_magic(magic: u8)->Result<Self,Error>
    {
        let max = u8::max_value();
        match magic
        {
            max => Ok(ResourceType::Nothing),
            0 => Ok(ResourceType::Space),
            1 => Ok(ResourceType::SubSpace),
            2 => Ok(ResourceType::App),
            3 => Ok(ResourceType::Actor),
            4 => Ok(ResourceType::User),
            5 => Ok(ResourceType::File),
            6 => Ok(ResourceType::Artifact),
            7 => Ok(ResourceType::FileSystem),
            _ => Err(format!("no resource type for magic number {}",magic).into())
        }
    }
}

impl fmt::Display for ResourceKind{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}",
                match self{
                    ResourceKind::Nothing=> "Nothing".to_string(),
                    ResourceKind::Space=> "Space".to_string(),
                    ResourceKind::SubSpace=> "SubSpace".to_string(),
                    ResourceKind::App(kind)=> format!("App:{}",kind).to_string(),
                    ResourceKind::Actor(kind)=> format!("Actor:{}",kind).to_string(),
                    ResourceKind::User=> "User".to_string(),
                    ResourceKind::File=> "File".to_string(),
                    ResourceKind::FileSystem(kind)=> format!("Filesystem:{}", kind).to_string(),
                    ResourceKind::Artifact(kind)=>format!("Artifact:{}",kind).to_string()
                })
    }

}

impl ResourceKind {
    pub fn test_key(&self, sub_space: SubSpaceKey, index: usize )->ResourceKey
    {
        match self
        {
            ResourceKind::Nothing => {
                ResourceKey::Nothing
            }
            ResourceKind::Space => {
                ResourceKey::Space(SpaceKey::from_index(index as u32 ))
            }
            ResourceKind::SubSpace => {
                ResourceKey::SubSpace(SubSpaceKey::new( sub_space.space, index as _))
            }
            ResourceKind::App(_) => {
                ResourceKey::App(AppKey::new(sub_space, index as _))
            }
            ResourceKind::Actor(_) => {
                let app = AppKey::new(sub_space,index as _);
                ResourceKey::Actor(ActorKey::new(app, Id::new(0, index as _)))
            }
            ResourceKind::User => {
                ResourceKey::User(UserKey::new(sub_space.space, index as _))
            }
            ResourceKind::File => {
                ResourceKey::File(FileKey{
                    filesystem: FileSystemKey::SubSpace(SubSpaceFilesystemKey{ sub_space, id: 0}),
                    id: index as _
                } )
            }
            ResourceKind::Artifact(_) => {
                ResourceKey::Artifact(ArtifactKey{
                    sub_space: sub_space,
                    id: index as _
                })
            }
            ResourceKind::FileSystem(kind) => {
                match kind
                {
                    FileSystemKind::App => {
                        ResourceKey::FileSystem(FileSystemKey::App(AppFilesystemKey{
                            app:AppKey::new(sub_space, index as _),
                            id: index as _
                        }))
                    }
                    FileSystemKind::SubSpace => {
                        ResourceKey::FileSystem(FileSystemKey::SubSpace(SubSpaceFilesystemKey{
                            sub_space: sub_space,
                            id: index as _
                        }))

                    }
                }

            }
        }
    }
}

impl FromStr for ResourceKind
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {

        if s.starts_with("App:") {
            let mut split = s.split(":");
            split.next().ok_or("error")?;
            return Ok( ResourceKind::App( AppKind::from_str(split.next().ok_or("error")?)? ));
        } else if s.starts_with("Actor:") {
            let mut split = s.split(":");
            split.next().ok_or("error")?;
            return Ok( ResourceKind::Actor( ActorKind::from_str(split.next().ok_or("error")?)? ) );
        } else if s.starts_with("Artifact:") {
            let mut split = s.split(":");
            split.next().ok_or("error")?;
            return Ok( ResourceKind::Artifact( ArtifactKind::from_str(split.next().ok_or("error")?)? ) );
        }


        match s
        {
            "Nothing" => Ok(ResourceKind::Nothing),
            "Space" => Ok(ResourceKind::Space),
            "SubSpace" => Ok(ResourceKind::SubSpace),
            "User" => Ok(ResourceKind::User),
            "File" => Ok(ResourceKind::File),
            _ => {
                Err(format!("cannot match ResourceKind: {}", s).into())
            }
        }
    }
}

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum ResourceType
{
    Nothing,
    Space,
    SubSpace,
    App,
    Actor,
    User,
    FileSystem,
    File,
    Artifact
}

impl ResourceType{
    pub fn requires_owner(&self)->bool {
        match self{
            Self::Nothing => false,
            Self::Space => false,
            Self::SubSpace => true,
            Self::App => true,
            Self::Actor => true,
            Self::User => false,
            Self::FileSystem => true,
            Self::File => true,
            Self::Artifact => true
        }
    }

    pub fn from( parent: &ResourceKey ) -> Option<Self> {
        match parent {
            ResourceKey::Nothing => Option::None,
            parent => Option::Some(parent.resource_type())
        }
    }

    pub fn hash_to_string(set: &HashSet<ResourceType> ) -> String {
        let mut string = String::new();
        for (index,resource_type) in set.iter().enumerate(){
            if index > 0 {
                string.push_str(",");
            }
            string.push_str(resource_type.to_string().as_str() );
        }
        string
    }
}

impl ResourceType{

    pub fn star_host(&self) -> StarKind {
        match self {
            ResourceType::Nothing => StarKind::Central,
            ResourceType::Space => StarKind::SpaceHost,
            ResourceType::SubSpace => StarKind::SpaceHost,
            ResourceType::App => StarKind::AppHost,
            ResourceType::Actor => StarKind::ActorHost,
            ResourceType::User => StarKind::SpaceHost,
            ResourceType::FileSystem => StarKind::FileStore,
            ResourceType::File => StarKind::FileStore,
            ResourceType::Artifact => StarKind::FileStore
        }
    }

    pub fn star_manager(&self) -> HashSet<StarKind>{
        match self {
            ResourceType::Nothing => HashSet::from_iter(vec![] ),
            ResourceType::Space => HashSet::from_iter(vec![StarKind::Central] ),
            ResourceType::SubSpace => HashSet::from_iter(vec![StarKind::SpaceHost] ),
            ResourceType::App => HashSet::from_iter(vec![StarKind::SpaceHost] ),
            ResourceType::Actor => HashSet::from_iter(vec![StarKind::AppHost] ),
            ResourceType::User => HashSet::from_iter(vec![StarKind::SpaceHost] ),
            ResourceType::FileSystem => HashSet::from_iter(vec![StarKind::SpaceHost,StarKind::AppHost] ),
            ResourceType::File => HashSet::from_iter(vec![StarKind::FileStore] ),
            ResourceType::Artifact => HashSet::from_iter(vec![StarKind::SpaceHost] )
        }
    }


}

impl ResourceType{
    pub fn children(&self)->HashSet<ResourceType> {
        let mut children = match self{
            Self::Nothing => vec![Self::Space],
            Self::Space => vec![Self::SubSpace,Self::User],
            Self::SubSpace => vec![Self::App,Self::FileSystem,Self::Artifact],
            Self::App => vec![Self::Actor,Self::FileSystem],
            Self::Actor => vec![],
            Self::User => vec![],
            Self::FileSystem => vec![Self::File],
            Self::File => vec![],
            Self::Artifact => vec![]
        };

        HashSet::from_iter(children.drain(..) )
    }

    pub fn supports_automatic_key_generation(&self) -> bool {

        match self{
            ResourceType::Nothing => false,
            ResourceType::Space => false,
            ResourceType::SubSpace => false,
            ResourceType::App => true,
            ResourceType::Actor => false,
            ResourceType::User => true,
            ResourceType::FileSystem => false,
            ResourceType::File => false,
            ResourceType::Artifact => false
        }
    }
}

impl ToString for ResourceType{
    fn to_string(&self) -> String {
        match self
        {
            Self::Nothing => "Nothing".to_string(),
            Self::Space => "Space".to_string(),
            Self::SubSpace => "SubSpace".to_string(),
            Self::App => "App".to_string(),
            Self::Actor => "Actor".to_string(),
            Self::User => "User".to_string(),
            Self::FileSystem => "FileSystem".to_string(),
            Self::File => "File".to_string(),
            Self::Artifact => "Artifact".to_string(),
        }
    }
}

impl FromStr for ResourceType {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Nothing" => Ok(ResourceType::Nothing),
            "Space" => Ok(ResourceType::Space),
            "SubSpace" => Ok(ResourceType::SubSpace),
            "App" => Ok(ResourceType::App),
            "Actor" => Ok(ResourceType::Actor),
            "User" => Ok(ResourceType::User),
            "FileSystem" => Ok(ResourceType::FileSystem),
            "File" => Ok(ResourceType::File),
            "Artifact" => Ok(ResourceType::Artifact),
            what => Err(format!("could not find resource type {}",what).into())
        }
    }
}



#[derive(Eq,PartialEq)]
pub enum ResourceParent
{
    None,
    Some(ResourceType),
    Multi(Vec<ResourceType>)
}

impl ResourceParent {
    pub fn matches_address( &self, address: Option<&ResourceAddress> ) -> bool
    {
        match address{
            None => {
                self.matches(Option::None)
            }
            Some(address) => {
                self.matches(Option::Some(address.resource_type()))
            }
        }
    }

    pub fn matches( &self, resource_type: Option<&ResourceType> ) -> bool
    {
        match resource_type{
            None => *self == Self::None,
            Some(resource_type) => {
                match self{
                    ResourceParent::None => false,
                    ResourceParent::Some(parent_type) => *parent_type == *resource_type,
                    ResourceParent::Multi(multi) => multi.contains(resource_type)
                }
            }
        }
    }

    pub fn types(&self)->Vec<ResourceType> {
        match self{
            ResourceParent::None => vec![],
            ResourceParent::Some(some) => vec![some.to_owned()],
            ResourceParent::Multi(multi) => multi.to_owned()
        }
    }


}

impl ToString for ResourceParent{
    fn to_string(&self) -> String {
        match self {
            ResourceParent::None =>  "None".to_string(),
            ResourceParent::Some(parent) => parent.to_string(),
            ResourceParent::Multi(_) => "Multi".to_string(),
        }
    }
}

impl ResourceType
{
    pub fn parent (&self) -> ResourceParent
    {
        match self
        {
            ResourceType::Nothing => ResourceParent::None,
            ResourceType::Space => ResourceParent::Some(ResourceType::Nothing),
            ResourceType::SubSpace => ResourceParent::Some(ResourceType::Space),
            ResourceType::App => ResourceParent::Some(ResourceType::SubSpace),
            ResourceType::Actor => ResourceParent::Some(ResourceType::App),
            ResourceType::User => ResourceParent::Some(ResourceType::Space),
            ResourceType::File => ResourceParent::Some(ResourceType::FileSystem),
            ResourceType::FileSystem => ResourceParent::Multi(vec![ResourceType::SubSpace,ResourceType::App]),
            ResourceType::Artifact => ResourceParent::Some(ResourceType::SubSpace)
        }
    }

    pub fn is_specific_required(&self) ->bool
    {
        match self
        {
            ResourceType::Nothing => false,
            ResourceType::Space => false,
            ResourceType::SubSpace => false,
            ResourceType::App => true,
            ResourceType::Actor => true,
            ResourceType::User => false,
            ResourceType::File => false,
            ResourceType::FileSystem => false,
            ResourceType::Artifact => true
        }
    }

    pub fn is_config_required(&self) ->bool
    {
        match self
        {
            ResourceType::Nothing => false,
            ResourceType::Space => false,
            ResourceType::SubSpace => false,
            ResourceType::App => true,
            ResourceType::Actor => true,
            ResourceType::User => false,
            ResourceType::File => false,
            ResourceType::FileSystem => false,
            ResourceType::Artifact => false
        }
    }


    // meaning it's operation can be handled by numerous stars.
    // this of course will only work if the resource is stateless
    pub fn is_sliced(&self)->bool
    {
        match self
        {
            ResourceType::Nothing => false,
            ResourceType::Space => false,
            ResourceType::SubSpace => false,
            ResourceType::App => true,
            ResourceType::Actor => false,
            ResourceType::User => false,
            ResourceType::File => false,
            ResourceType::FileSystem => false,
            ResourceType::Artifact => false
        }
    }

    pub fn has_state(&self)->bool
    {
        match self
        {
            ResourceType::Nothing => false,
            ResourceType::Space => false,
            ResourceType::SubSpace => false,
            ResourceType::App => false,
            ResourceType::Actor => true,
            ResourceType::User => false,
            ResourceType::File => true,
            ResourceType::FileSystem => false,
            ResourceType::Artifact => true
        }
    }


    pub fn address_required(&self)->bool
    {
        match self
        {
            ResourceType::Nothing => false,
            ResourceType::Space => true,
            ResourceType::SubSpace => true,
            ResourceType::App => false,
            ResourceType::Actor => false,
            ResourceType::User => false,
            ResourceType::File => true,
            ResourceType::FileSystem => true,
            ResourceType::Artifact => true
        }
    }

    pub fn address_structure(&self) ->&ResourceAddressStructure{
        match self{
            ResourceType::Nothing => {
                &NOTHING_STRUCT
            }
            ResourceType::Space => {
                &SPACE_ADDRESS_STRUCT
            }
            ResourceType::SubSpace => {
                &SUB_SPACE_ADDRESS_STRUCT
            }
            ResourceType::App => {
                &APP_ADDRESS_STRUCT
            }
            ResourceType::Actor => {
                &ACTOR_ADDRESS_STRUCT
            }
            ResourceType::User => {
                &USER_ADDRESS_STRUCT
            }
            ResourceType::FileSystem => {
                &FILE_SYSTEM_ADDRESS_STRUCT
            }
            ResourceType::File => {
                &FILE_ADDRESS_STRUCT
            }
            ResourceType::Artifact => {
                &ARTIFACT_ADDRESS_STRUCT
            }
        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceInit
{
    pub init: InitData,
    pub archetype: ResourceArchetype
}




#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceArchetype {
    pub kind: ResourceKind,
    pub specific: Option<Specific>,
    pub config: Option<ConfigSrc>
}

impl ResourceArchetype{
    pub fn valid(&self)->Result<(),Fail>{
        if self.kind.resource_type() == ResourceType::Nothing{
            return Err(Fail::CannotCreateNothingResourceTypeItIsThereAsAPlaceholderDummy);
        }
        Ok(())
    }
}

pub struct ResourceControl{
    pub resource_type: ResourceType
}

impl ResourceControl{

    pub async fn create_child(resource: ResourceInit, register: Option<ResourceRegistryInfo> ) -> Result<ResourceStub,Fail>
    {
        unimplemented!()
    }
}

#[async_trait]
pub trait ResourceIdSeq: Send+Sync {
    async fn next(&self)->ResourceId;
}

#[async_trait]
pub trait HostedResource: Send+Sync {
    fn key(&self)->ResourceKey;
}

#[derive(Clone)]
pub struct HostedResourceStore{
   map: AsyncHashMap<ResourceKey,Arc<LocalHostedResource>>
}

impl HostedResourceStore{
    pub async fn new( ) -> Self{
        HostedResourceStore{
            map: AsyncHashMap::new().await
        }
    }

    pub async fn store(&self, resource: Arc<LocalHostedResource> ) -> Result<(),Error> {
        self.map.put(resource.resource.key.clone(), resource ).await
    }

    pub async fn get( &self, key: ResourceKey ) -> Result<Option<Arc<LocalHostedResource>>,Error> {
        self.map.get(key).await
    }

    pub async fn remove( &self, key: ResourceKey ) -> Result<Option<Arc<LocalHostedResource>>,Error> {
        self.map.remove(key).await
    }

    pub async fn contains( &self, key: &ResourceKey ) -> Result<bool,Error> {
        self.map.contains(key.clone()).await
    }
}



#[derive(Clone)]
pub struct RemoteHostedResource{
    key: ResourceKey,
    star_host: StarKey,
    local_skel: StarSkel
}

pub struct LocalHostedResource {
//    pub manager: Arc<dyn ResourceManager>,
    pub unique_src: Box<dyn UniqueSrc>,
    pub resource: ResourceStub
}
impl HostedResource for LocalHostedResource {
    fn key(&self)->ResourceKey{
       self.resource.key.clone()
    }
}

#[async_trait]
pub trait ResourceManager: Send+Sync {
    async fn create( &self, create: ResourceCreate ) -> oneshot::Receiver<Result<ResourceLocationRecord,Fail>>;
}

pub struct RemoteResourceManager{
    pub key: ResourceKey
}

impl RemoteResourceManager{
    pub fn new(key: ResourceKey) -> Self {
        RemoteResourceManager{
            key: key
        }
    }
}

#[async_trait]
impl ResourceManager for RemoteResourceManager {
    async fn create(&self, create: ResourceCreate) -> Receiver<Result<ResourceLocationRecord, Fail>> {
        unimplemented!();
    }
}

#[derive(Clone)]
pub struct ResourceManagerCore{
    pub key: ResourceKey,
    pub address: ResourceAddress,
    pub selector: ResourceHostSelector,
    pub registry: Arc<dyn ResourceRegistryBacking>,
    pub id_seq: Arc<IdSeq>
}


pub struct LocalResourceManager {
   pub core: ResourceManagerCore
}

impl LocalResourceManager {
    async fn process_create(core: ResourceManagerCore, create: ResourceCreate ) -> Result<ResourceLocationRecord,Fail>{

        if !core.key.resource_type().parent().matches(ResourceType::from(&create.parent).as_ref()) {
            return Err(Fail::WrongParentResourceType {
                expected: HashSet::from_iter(core.key.resource_type().parent().types()),
                received: Option::Some(create.parent.resource_type())
            });
        };

        create.validate()?;

        let reservation = core.registry.reserve(ResourceNamesReservationRequest{
            parent: create.parent.clone(),
            archetype: create.init.archetype.clone(),
            info: create.registry_info } ).await?;

        let key = match create.key {
            KeyCreationSrc::None => {
                ResourceKey::new(core.key.clone(), ResourceId::new(&create.init.archetype.kind.resource_type(), core.id_seq.next() ) )?
            }
            KeyCreationSrc::Key(key) => {
                if key.parent() != Option::Some(core.key.clone()){
                    return Err("parent keys do not match".into());
                }
                key
            }
        };
println!("CREATED KEY: {}",key);

        let address = match create.address{
            AddressCreationSrc::None => {
                let address = format!("{}:{}", core.address.to_string(), key.generate_address_tail()? );
                create.init.archetype.kind.resource_type().address_structure().from_str(address.as_str())?
            }
            AddressCreationSrc::Append(tail) => {
                let address = format!("{}:{}", core.address.to_string(), tail );
                create.init.archetype.kind.resource_type().address_structure().from_str(address.as_str())?
            }
            AddressCreationSrc::Space(space_name) => {
                if core.key.resource_type() != ResourceType::Nothing{
                    return Err(format!("Space creation can only be used at top level (Nothing) not by {}",core.key.resource_type().to_string()).into());
                }
                ResourceAddress::for_space(space_name.as_str())?
            }
        };

        let assign = ResourceAssign {
            key: key.clone(),
            address: address.clone(),
            source: ResourceSrc::Creation(create.init.clone())
        };

println!("selecting for resource kind: {}", create.init.archetype.kind.resource_type().to_string());
        let mut host = core.selector.select(create.init.archetype.kind.resource_type(), create.location_affinity ).await?;

        host.assign(assign).await?;

        let location_record = ResourceLocationRecord{
            key: key.clone(),
            location: ResourceLocation { host: host.star_key(), gathering: Option::None }
        };

        core.registry.set_location(location_record.clone()).await?;

        let resource = ResourceStub {
            key: key,
            address: address,
            archetype: create.init.archetype,
            owner: None
        };

        reservation.commit( resource.clone() );

        Ok(location_record)
    }
}

#[async_trait]
impl ResourceManager for LocalResourceManager {
    async fn create( &self, create: ResourceCreate ) -> oneshot::Receiver<Result<ResourceLocationRecord,Fail>> {
        let (tx,rx) = oneshot::channel();

        let core = self.core.clone();
        tokio::spawn( async move {
            tx.send(LocalResourceManager::process_create(core, create ).await).unwrap_or_default();
        });
        rx
    }
}


#[async_trait]
pub trait ResourceHost: Send+Sync {
    fn star_key( &self ) -> StarKey;
    async fn assign( &self, assign: ResourceAssign ) -> Result<(),Fail>;
}





impl fmt::Display for FileSystemKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}",
                match self{
                    FileSystemKind::App => "App".to_string(),
                    FileSystemKind::SubSpace => "SubSpace".to_string()
                })
    }
}





impl From<AppKind> for ResourceKind{
    fn from(e: AppKind) -> Self {
        ResourceKind::App(e)
    }
}


impl From<ActorKind> for ResourceKind{
    fn from(e: ActorKind) -> Self {
        ResourceKind::Actor(e)
    }
}

impl From<ArtifactKind> for ResourceKind{
    fn from(e: ArtifactKind) -> Self {
        ResourceKind::Artifact(e)
    }
}


#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceRegistryInfo
{
    pub names: Names,
    pub labels: Labels
}

pub struct ResourceNamesReservationRequest{
  pub parent: ResourceKey,
  pub archetype: ResourceArchetype,
  pub info: Option<ResourceRegistryInfo>
}

pub struct RegistryReservation{
    tx: Option<oneshot::Sender<ResourceStub>>
}

impl RegistryReservation{
    pub fn commit(self, resource: ResourceStub) -> Result<(),Fail> {
        if let Option::Some(tx) = self.tx{
            tx.send(resource).or(Err(Fail::Unexpected));
        }
        Ok(())
    }

    pub fn new(tx: oneshot::Sender<ResourceStub> ) ->Self
    {
        Self{
            tx: Option::Some(tx)
        }
    }

    pub fn empty()->Self {
        RegistryReservation{
            tx: Option::None
        }
    }
}

pub struct RegistryUniqueSrc {
  key: ResourceKey,
  tx: mpsc::Sender<ResourceRegistryAction>
}

impl RegistryUniqueSrc {
    pub fn new(key: ResourceKey, tx: mpsc::Sender<ResourceRegistryAction>) -> Self {
        RegistryUniqueSrc {
            key: key,
            tx: tx
        }
    }
}

#[async_trait]
impl UniqueSrc for RegistryUniqueSrc{
   async fn next(&self, unique: Unique) -> Result<u64,Fail>{
       let (tx,rx) = oneshot::channel();

       self.tx.send( ResourceRegistryAction{
           tx: tx,
           command: ResourceRegistryCommand::Next{key:self.key.clone(),unique:unique}
       }).await?;

       if let ResourceRegistryResult::Unique(index) = rx.await? {
           Ok(index)
       } else {
           Err(Fail::Unexpected)
       }
   }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceRegistration
{
    pub resource: ResourceStub,
    pub info: ResourceRegistryInfo
}




impl ResourceRegistration
{
    pub fn new(resource: ResourceStub, info: ResourceRegistryInfo ) ->Self
    {
        ResourceRegistration{
            resource: resource,
            info: info
        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceLocationAffinity{
    pub star: StarKey
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceLocationRecord
{
    pub key: ResourceKey,
    pub location: ResourceLocation
}

impl ResourceLocationRecord
{
    pub fn new( key: ResourceKey, host: StarKey )->Self
    {
        ResourceLocationRecord {
            key: key,
            location: ResourceLocation::new(host)
        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceLocation{
    pub host: StarKey,
    pub gathering: Option<GatheringKey>
}

impl ResourceLocation{
    pub fn new(  host: StarKey )->Self
    {
        ResourceLocation{
            host: host,
            gathering: Option::None
        }
    }
}

pub enum ResourceManagerKey
{
    Central,
    Key(ResourceKey)
}


#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct ResourceAddress
{
   resource_type: ResourceType,
   parts: Vec<ResourceAddressPart>
}

impl ResourceAddress {

    pub fn last_to_string(&self) -> Result<String,Error> {
        Ok(self.parts.last().ok_or("couldn't find last".into() )?.to_string())
    }


    pub fn nothing() -> ResourceAddress{
       ResourceAddress{
           resource_type: ResourceType::Nothing,
           parts: vec![]
       }
    }

    pub fn for_space(string : &str) -> Result<Self,Error> {
        ResourceType::Space.address_structure().from_str(string )
    }

    pub fn for_sub_space(string: &str) -> Result<Self,Error> {
        ResourceType::SubSpace.address_structure().from_str(string )
    }

    pub fn for_app(string: &str) -> Result<Self,Error> {
        ResourceType::App.address_structure().from_str(string )
    }

    pub fn for_actor(string: &str) -> Result<Self,Error> {
        ResourceType::Actor.address_structure().from_str(string )
    }

    pub fn for_filesystem(string: &str) -> Result<Self,Error> {
        ResourceType::FileSystem.address_structure().from_str(string )
    }

    pub fn for_file(string: &str) -> Result<Self,Error> {
        ResourceType::File.address_structure().from_str(string )
    }

    pub fn for_artifact(string: &str ) -> Result<Self,Error> {
        ResourceType::Artifact.address_structure().from_str(string )
    }

    pub fn for_user(string: &str) -> Result<Self,Error> {
        ResourceType::User.address_structure().from_str(string )
    }


    pub fn test_address( key: &ResourceKey )->Result<Self,Error> {
        let mut parts = vec![];

        let mut mark = Option::Some(key.clone());
        while let Option::Some(key) = mark
        {
            match &key
            {
                ResourceKey::Nothing => {
                    // do nothing
                }
                ResourceKey::Space(space) => {
                    parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("space-{}",space.id()).as_str())?));
                }
                ResourceKey::SubSpace(sub_space) => {
                    parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("sub-{}",sub_space.id).as_str())?));
                }
                ResourceKey::App(app) => {
                    parts.push(app.address_part()?);
                }
                ResourceKey::Actor(actor) => {
                    parts.push(actor.address_part()?);
                }
                ResourceKey::User(user) => {
                    parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("user-{}",user.id).as_str())?));
                }
                ResourceKey::Artifact(artifact) => {
                    parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("artifact-{}",artifact.id).as_str())?));
                }
                ResourceKey::File(file) => {
                    parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("file-{}",file.id).as_str())?));
                }
                ResourceKey::FileSystem(filesystem) => {
                    match filesystem {
                        FileSystemKey::App(app) => {
                            parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("filesystem-{}",app.id).as_str())?));
                        }
                        FileSystemKey::SubSpace(sub_space) => {
                            parts.push(ResourceAddressPart::Skewer(Skewer::new(format!("filesystem-{}",sub_space.id).as_str())?));
                            parts.push( ResourceAddressPart::Wildcard );
                        }
                    }
                }
            }

            mark = key.parent();
        }
        Ok(ResourceAddress::from_parts( &key.resource_type(), parts )?)
    }



    pub fn resource_type(&self) -> &ResourceType {
        &self.resource_type
    }

    pub fn space(&self) -> Result<ResourceAddress,Error> {
        Ok(SPACE_ADDRESS_STRUCT.from_str(self.parts.get(0).ok_or("expected space")?.to_string().as_str() )?)
    }

    pub fn sub_space(&self) -> Result<ResourceAddress,Error> {
        if self.resource_type == ResourceType::Space {
            Err("Space ResourceAddress does not have a SubSpace".into())
        }
        else {
            Ok(SPACE_ADDRESS_STRUCT.from_str(format!("{}:{}", self.parts.get(0).ok_or("expected space")?.to_string(), self.parts.get(1).ok_or("expected sub_space")?.to_string()).as_str())?)
        }
    }
    pub fn from_parent(resource_type: &ResourceType, parent: Option<&ResourceAddress>, part: ResourceAddressPart) -> Result<ResourceAddress, Error> {


        if !resource_type.parent().matches_address(parent)  {
            return Err(format!("resource type parent is wrong: expected: {}", resource_type.parent().to_string()).into() )
        }

        let mut parts = vec![];
        if let Option::Some(parent) = parent {
            parts.append(&mut parent.parts.clone());
        }
        parts.push(part);

        Self::from_parts( resource_type, parts )
    }

    pub fn from_parts(resource_type: &ResourceType, mut parts: Vec<ResourceAddressPart>) -> Result<ResourceAddress, Error> {

        for (index,part_struct) in resource_type.address_structure().parts.iter().enumerate(){
            let part = parts.get(index).ok_or("missing part")?;
            if !part_struct.kind.matches(part) {
                return Err(format!("part does not match {}",part.to_string()).into());
            }
        }

        Ok(ResourceAddress{
            parts: parts,
            resource_type: resource_type.clone()
        })
    }
}

impl ToString for ResourceAddress {
    fn to_string(&self) -> String {
        let mut rtn = String::new();

        rtn.push_str(self.resource_type.to_string().as_str() );
        rtn.push_str("::");

        for (index,part) in self.parts.iter().enumerate() {
            if index != 0{
                rtn.push_str(":")
            }
            rtn.push_str(part.to_string().as_str() );
        }
        rtn
    }
}

impl FromStr for ResourceAddress {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split("::");
        let resource_type = ResourceType::from_str(split.next().ok_or("resource type")?)?;
        Ok(resource_type.address_structure().from_str(split.next().ok_or("address structure")? )?)
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceBinding{
    pub key: ResourceKey,
    pub address: ResourceAddress
}

#[derive(Clone)]
pub struct ResourceAddressStructure
{
    parts: Vec<ResourceAddressPartStruct>,
    resource_type: ResourceType
}

impl ResourceAddressStructure
{
    pub fn format(&self)->String {
        let mut rtn = String::new();
        for (index,part) in self.parts.iter().enumerate() {
            if index != 0 {
                rtn.push_str(":");
            }
            rtn.push_str(part.name.as_str() );
        }
        rtn
    }


    pub fn new( parts: Vec<ResourceAddressPartStruct>, resource_type: ResourceType ) -> Self {
        ResourceAddressStructure{
            parts: parts,
            resource_type: resource_type
        }
    }

    pub fn with_parent( parent: Self, mut parts: Vec<ResourceAddressPartStruct>, resource_type: ResourceType ) -> Self
    {
        let mut union = parent.parts.clone();
        union.append( &mut parts );
        Self::new(union, resource_type )
    }
}




impl ResourceAddressStructure {

    pub fn from_str(&self, s: &str) -> Result<ResourceAddress, Error> {
        let mut split = s.split(":");

        if split.count()  != self.parts.len() {
            return Err(format!("part count not equal. expected format '{}'",self.format()).into());
        }

        let mut split = s.split(":");

        let mut parts = vec![];

        for part in &self.parts{
            parts.push(part.kind.from_str(split.next().ok_or(part.kind.to_string() )?.clone() )?);
        }

        Ok(ResourceAddress{
            parts: parts,
            resource_type: self.resource_type.clone()
        })
    }




    pub fn matches( &self, parts: Vec<ResourceAddressPart> ) -> bool {
        if parts.len() != self.parts.len() {
            return false;
        }
        for (index,part) in parts.iter().enumerate()
        {
            let part_struct = self.parts.get(index).unwrap();
            if !part_struct.kind.matches(part) {
                return false;
            }
        }

        return true;
    }
}

#[derive(Clone,Eq,PartialEq)]
pub struct ResourceAddressPartStruct
{
    pub name: String,
    pub kind: ResourceAddressPartKind
}

impl ResourceAddressPartStruct
{
    pub fn new( name:&str, kind: ResourceAddressPartKind ) -> Self {
        ResourceAddressPartStruct{
            name: name.to_string(),
            kind: kind
        }
    }
}

#[derive(Clone,Serialize,Deserialize,Eq,PartialEq)]
pub enum ResourceAddressPartKind
{
    Wildcard,
    Skewer,
    Version,
    WildcardOrSkewer,
    Path,
    Base64Encoded
}

impl ToString for ResourceAddressPartKind {
    fn to_string(&self) -> String {
        match self {
            ResourceAddressPartKind::Wildcard => "Wildcard".to_string(),
            ResourceAddressPartKind::Skewer => "Skewer".to_string(),
            ResourceAddressPartKind::Version => "Version".to_string(),
            ResourceAddressPartKind::WildcardOrSkewer => "WildcardOrSkewer".to_string(),
            ResourceAddressPartKind::Path => "Path".to_string(),
            ResourceAddressPartKind::Base64Encoded => "Base64Encoded".to_string()
        }
    }
}

impl ResourceAddressPartKind
{
    pub fn matches( &self, part: &ResourceAddressPart )->bool
    {
        match part
        {
            ResourceAddressPart::Wildcard => {
                *self == Self::Wildcard || *self == Self::WildcardOrSkewer
            }
            ResourceAddressPart::Skewer(_) => {
                *self == Self::Skewer || *self == Self::WildcardOrSkewer
            }
            ResourceAddressPart::Path(_) => {
                *self == Self::Path
            }
            ResourceAddressPart::Version(_) => {
                *self == Self::Version
            }
            ResourceAddressPart::Base64Encoded(_) => {
                *self == Self::Base64Encoded
            }
        }
    }

    pub fn from_str(&self, s: &str ) -> Result<ResourceAddressPart,Error> {
        match self{
            ResourceAddressPartKind::Wildcard => {
                if s == "*" {
                    Ok(ResourceAddressPart::Wildcard)
                }
                else
                {
                    Err("expected wildcard".into())
                }
            }
            ResourceAddressPartKind::Skewer => {
                Ok(ResourceAddressPart::Skewer(Skewer::from_str(s)?))
            }
            ResourceAddressPartKind::WildcardOrSkewer => {
                if s == "*" {
                    Ok(ResourceAddressPart::Wildcard)
                }
                else
                {
                    Ok(ResourceAddressPart::Skewer(Skewer::from_str(s)?))
                }
            }
            ResourceAddressPartKind::Path => {
                Ok(ResourceAddressPart::Path(Path::from_str(s)?))
            }
            ResourceAddressPartKind::Version => {
                Ok(ResourceAddressPart::Version(Version::from_str(s)?))
            }
            ResourceAddressPartKind::Base64Encoded => {
                Ok(ResourceAddressPart::Base64Encoded(Base64Encoded::encoded(s.to_string())?))
            }
        }
    }
}



#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub enum ResourceAddressPart
{
    Wildcard,
    Skewer(Skewer),
    Base64Encoded(Base64Encoded),
    Path(Path),
    Version(Version)
}

impl ToString for ResourceAddressPart {
    fn to_string(&self) -> String {
        match self {
            ResourceAddressPart::Wildcard => "*".to_string(),
            ResourceAddressPart::Skewer(skewer) => skewer.to_string(),
            ResourceAddressPart::Base64Encoded(base64) => base64.encoded.clone(),
            ResourceAddressPart::Path(path) => path.to_string(),
            ResourceAddressPart::Version(version) => version.to_string()
        }
    }
}

#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct Base64Encoded {
    encoded: String
}

impl Base64Encoded {
    pub fn decoded( decoded: String ) ->Result<Self,Error>{
        Ok(Base64Encoded {
            encoded: base64::encode(decoded.as_bytes() )
        })
    }

    pub fn encoded( encoded: String ) ->Result<Self,Error>{

        match base64::decode(encoded.clone() ) {
            Ok(decoded ) => {
                match String::from_utf8(decoded){
                    Ok(_) => {
                        Ok(Base64Encoded{
                            encoded: encoded
                        })
                    }
                    Err(err) => {
                        Err(err.to_string().into())
                    }
                }
            }
            Err(err) => {
                Err(err.to_string().into())
            }
        }
    }
}

#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct Skewer
{
    string: String
}

impl Skewer
{
    pub fn new( string: &str ) -> Result<Self,Error> {
        if string.is_empty() {
            return Err("cannot be empty".into());
        }

        for c in string.chars() {
            if !((c.is_lowercase() && c.is_alphanumeric()) || c == '-') {
                return Err("must be lowercase, use only alphanumeric characters & dashes".into());
            }
        }
        Ok(Skewer{
            string: string.to_string()
        })
    }
}

impl ToString for Skewer {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl FromStr for Skewer {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Skewer::new(s )?)
    }
}






#[cfg(test)]
mod test
{
    use std::sync::Arc;

    use futures::SinkExt;
    use tokio::runtime::Runtime;
    use tokio::sync::mpsc;
    use tokio::sync::oneshot::error::RecvError;
    use tokio::time::Duration;
    use tokio::time::timeout;

    use crate::actor::{ActorKey, ActorKind};
    use crate::app::{AppController, AppKind, AppSpecific, ConfigSrc, InitData};
    use crate::artifact::{Artifact, ArtifactKind, ArtifactLocation};
    use crate::error::Error;
    use crate::id::Id;
    use crate::keys::{AppKey, ResourceKey, SpaceKey, SubSpaceId, SubSpaceKey, UserKey};
    use crate::logger::{Flag, Flags, Log, LogAggregate, ProtoStarLog, ProtoStarLogPayload, StarFlag, StarLog, StarLogPayload};
    use crate::names::{Name, Specific};
    use crate::permissions::Authentication;
    use crate::resource::{FieldSelection, Labels, LabelSelection, Names, Registry, ResourceStub, ResourceAddress, ResourceAddressPart, ResourceArchetype, ResourceAssign, ResourceKind, ResourceRegistration, ResourceRegistryAction, ResourceRegistryCommand, ResourceRegistryInfo, ResourceRegistryResult, ResourceType, ResourceSelector, Skewer};
    use crate::resource::FieldSelection::SubSpace;
    use crate::resource::ResourceRegistryResult::Resources;
    use crate::space::CreateAppControllerFail;
    use crate::star::{StarController, StarInfo, StarKey, StarKind};
    use crate::starlane::{ConstellationCreate, StarControlRequestByName, Starlane, StarlaneCommand};
    use crate::template::{ConstellationData, ConstellationTemplate};

    fn create_save(index: usize, resource: ResourceStub) -> ResourceRegistration
    {
        if index == 0
        {
            eprintln!("don't use 0 index, it messes up the tests.  Start with 1");
            assert!(false)
        }
        let parity = match (index%2)==0 {
            true => "Even",
            false => "Odd"
        };

        let names = match index
        {
            1 => vec!["Lowest".to_string()],
            10 => vec!["Highest".to_string()],
            _ => vec![]
        };

        let mut labels = Labels::new();
        labels.insert( "parity".to_string(), parity.to_string() );
        labels.insert( "index".to_string(), index.to_string() );

        let save = ResourceRegistration{
            resource: resource,
            info: ResourceRegistryInfo {
                labels: labels,
                names: names,
            }
        };
        save
    }

    fn create_with_key(  key: ResourceKey, address: ResourceAddress, kind: ResourceKind, specific: Option<Specific>, sub_space: SubSpaceKey, owner: UserKey ) -> ResourceRegistration
    {
        let resource = ResourceStub {
            key: key,
            address: address,
            owner: Option::Some(owner),
            archetype: ResourceArchetype{
                kind: kind,
                specific: specific,
                config: Option::None
            }
        };

        let save = ResourceRegistration{
            resource: resource,
            info: ResourceRegistryInfo {
                labels: Labels::new(),
                names: Names::new(),
            }
        };

        save
    }


    fn create( index: usize, kind: ResourceKind, specific: Option<Specific>, sub_space: SubSpaceKey, owner: UserKey ) -> ResourceRegistration
    {
        if index == 0
        {
            eprintln!("don't use 0 index, it messes up the tests.  Start with 1");
            assert!(false)
        }
        let key = kind.test_key(sub_space,index);
        let address = ResourceAddress::test_address(&key).unwrap();

        let resource = ResourceStub {
            key: key,
            address: address,
            owner: Option::Some(owner),
            archetype: ResourceArchetype{
                kind: kind,
                specific: specific,
                config: Option::None
            }
        };

        create_save(index,resource)
    }

    async fn create_10(tx: mpsc::Sender<ResourceRegistryAction>, kind: ResourceKind, specific: Option<Specific>, sub_space: SubSpaceKey, owner: UserKey )
    {
        for index in 1..11
        {
            let save = create(index,kind.clone(),specific.clone(),sub_space.clone(),owner.clone());
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Commit(save));
            tx.send( request ).await;
            timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
        }
    }

    async fn create_10_spaces(tx: mpsc::Sender<ResourceRegistryAction> ) ->Vec<SpaceKey>
    {
        let mut spaces = vec!();
        for index in 1..11
        {
            let space = SpaceKey::from_index(index as _);
            let address_part = format!("some-space-{}", index);
            let resource= ResourceStub {
                key: ResourceKey::Space(space.clone()),
                address: crate::resource::SPACE_ADDRESS_STRUCT.from_str(address_part.as_str()).unwrap(),
                archetype: ResourceArchetype{
                    kind: ResourceKind::Space,
                    specific: None,
                    config: Option::None
                },
                owner: None
            };

            let save = create_save(index,resource);
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Commit(save));
            tx.send( request ).await;
            timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            spaces.push(space)
        }
        spaces
    }


    async fn create_10_actors(tx: mpsc::Sender<ResourceRegistryAction>, app: AppKey, specific: Option<Specific>, sub_space: SubSpaceKey, app_address: ResourceAddress, owner: UserKey )
    {
        for index in 1..11
        {
            let actor_key = ResourceKey::Actor(ActorKey::new(app.clone(), Id::new(0, index)));
            let address = ResourceAddress::from_parent( &ResourceType::Actor, Option::Some(&app_address), ResourceAddressPart::Skewer(Skewer::new(actor_key.encode().unwrap().as_str() ).unwrap())).unwrap();

            let save = create_with_key(actor_key,address, ResourceKind::Actor(ActorKind::Single),specific.clone(),sub_space.clone(),owner.clone());
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Commit(save));
            tx.send( request ).await;
            timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
        }
    }


    async fn create_10_sub_spaces(tx: mpsc::Sender<ResourceRegistryAction>, space_resource: ResourceStub) ->Vec<SubSpaceKey>
    {
        let mut sub_spaces = vec!();
        for index in 1..11
        {
            let space= space_resource.key.space().unwrap_or(SpaceKey::hyper_space());
            let sub_space = SubSpaceKey::new(space.clone(), index as _ );
            let address_part = ResourceAddressPart::Skewer(Skewer::new(format!("sub-space-{}", index).as_str()).unwrap());
            let address = ResourceAddress::from_parent(&ResourceType::SubSpace, Option::Some(&space_resource.address.clone()), address_part.clone()).unwrap();

            let resource= ResourceStub {
                key: ResourceKey::SubSpace(sub_space.clone()),
                address: address,
                archetype: ResourceArchetype{
                    kind: ResourceKind::SubSpace,
                    specific: None,
                    config: None
                },
                owner: None
            };

            let save = create_save(index,resource);
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Commit(save));
            tx.send( request ).await;
            timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            sub_spaces.push(sub_space)
        }
        sub_spaces
    }


    #[test]
    pub fn test10()
    {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let tx = Registry::new().await;

            create_10(tx.clone(), ResourceKind::App(AppKind::Normal),Option::None,SubSpaceKey::hyper_default(), UserKey::hyper_user() ).await;
            let mut selector = ResourceSelector::app_selector();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,10);

            let mut selector = ResourceSelector::app_selector();
            selector.add_label( LabelSelection::exact("parity", "Even") );
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,5);

            let mut selector = ResourceSelector::app_selector();
            selector.add_label( LabelSelection::exact("parity", "Odd") );
            selector.add_label( LabelSelection::exact("index", "3") );
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,1);


            let mut selector = ResourceSelector::app_selector();
            selector.name("Highest".to_string()).unwrap();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,1);

            let mut selector = ResourceSelector::actor_selector();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,0);
        });
    }

    #[test]
    pub fn test20()
    {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let tx = Registry::new().await;

            create_10(tx.clone(), ResourceKind::App(AppKind::Normal),Option::None,SubSpaceKey::hyper_default(), UserKey::hyper_user() ).await;
            create_10(tx.clone(), ResourceKind::Actor(ActorKind::Single),Option::None,SubSpaceKey::hyper_default(), UserKey::hyper_user() ).await;

            let mut selector = ResourceSelector::new();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,20);

            let mut selector = ResourceSelector::app_selector();
            selector.add_label( LabelSelection::exact("parity", "Even") );
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,5);

            let mut selector = ResourceSelector::app_selector();
            selector.add_label( LabelSelection::exact("parity", "Odd") );
            selector.add_label( LabelSelection::exact("index", "3") );
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,1);


            let mut selector = ResourceSelector::new();
            selector.name("Highest".to_string()).unwrap();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,2);
        });
    }

    #[test]
    pub fn test_spaces()
    {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let tx = Registry::new().await;

            let spaces = create_10_spaces(tx.clone() ).await;
            let mut sub_spaces = vec![];
            for space in spaces.clone() {
                let space_resource = ResourceStub {
                    key: ResourceKey::Space(space.clone()),
                    address: ResourceAddress::test_address(&ResourceKey::Space(space.clone())).unwrap(),
                    archetype: ResourceArchetype {
                        kind: ResourceKind::Space,
                        specific: None,
                        config: None
                    },
                    owner: None
                };
                sub_spaces.append( &mut create_10_sub_spaces(tx.clone(), space_resource ).await );
            }

            for sub_space in sub_spaces.clone()
            {
                create_10(tx.clone(), ResourceKind::App(AppKind::Normal),Option::None,sub_space, UserKey::hyper_user() ).await;
            }

            let mut selector = ResourceSelector::app_selector();
            selector.fields.insert(FieldSelection::Space(spaces.get(0).cloned().unwrap()));
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,100);

            let mut selector = ResourceSelector::app_selector();
            selector.fields.insert(FieldSelection::SubSpace(sub_spaces.get(0).cloned().unwrap()));
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,10);


        });
    }

    #[test]
    pub fn test_specific()
    {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let tx = Registry::new().await;


            create_10(tx.clone(), ResourceKind::App(AppKind::Normal),Option::Some(crate::names::TEST_APP_SPEC.clone()), SubSpaceKey::hyper_default(), UserKey::hyper_user() ).await;
            create_10(tx.clone(), ResourceKind::App(AppKind::Normal),Option::Some(crate::names::TEST_ACTOR_SPEC.clone()), SubSpaceKey::hyper_default(), UserKey::hyper_user() ).await;

            let mut selector = ResourceSelector::app_selector();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,20);

            let mut selector = ResourceSelector::app_selector();
            selector.fields.insert(FieldSelection::Specific(crate::names::TEST_APP_SPEC.clone()));
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,10);


        });
    }
    #[test]
    pub fn test_app()
    {
        let rt = Runtime::new().unwrap();
        rt.block_on(async {
            let tx = Registry::new().await;

            let sub_space = SubSpaceKey::hyper_default();
            let app1 = AppKey::new(sub_space.clone(), 1);
            let app_address = ResourceAddress::test_address(&ResourceKey::App(app1.clone())).unwrap();
            create_10_actors(tx.clone(), app1.clone(), Option::None, sub_space.clone(), app_address,UserKey::hyper_user() ).await;

            let app2 = AppKey::new(sub_space.clone(), 2);
            let app_address = ResourceAddress::test_address(&ResourceKey::App(app2.clone())).unwrap();
            create_10_actors(tx.clone(), app2.clone(), Option::None, sub_space.clone(), app_address, UserKey::hyper_user() ).await;

            let mut selector = ResourceSelector::actor_selector();
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,20);

            let mut selector = ResourceSelector::actor_selector();
            selector.add_field(FieldSelection::App(app1.clone()));
            let (request,rx) = ResourceRegistryAction::new(ResourceRegistryCommand::Select(selector) );
            tx.send(request).await;
            let result = timeout( Duration::from_secs(5),rx).await.unwrap().unwrap();
            assert_result_count(result,10);
        });
    }

    fn results(result: ResourceRegistryResult) ->Vec<ResourceStub>
    {
        if let ResourceRegistryResult::Resources(resources) = result
        {
            resources
        }
        else
        {
            assert!(false);
            vec!()
        }
    }


    fn assert_result_count(result: ResourceRegistryResult, count: usize )
    {
        if let ResourceRegistryResult::Resources(resources) = result
        {
            assert_eq!(resources.len(),count);
            println!("PASS");
        }
        else if let ResourceRegistryResult::Error(error) = result
        {
            eprintln!("FAIL: {}",error);
            assert!(false);
        }
        else
        {
            eprintln!("FAIL");
            assert!(false);
        }
    }
}





#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct Path
{
    string: String
}

impl Path
{
    pub fn new( string: &str ) -> Result<Self,Error> {

        if string.trim().is_empty() {
            return Err("path cannot be empty".into());
        }

        if string.contains(".."){
            return Err("path cannot contain directory traversal sequence [..]".into());
        }

        for c in string.chars() {
            if c == '*' || c == '?' || c == ':' {
                return Err("path cannot contain wildcard characters [*,?] or [:]".into());
            }
        }
        Ok(Path{
            string: string.to_string()
        })
    }
}

impl ToString for Path {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl FromStr for Path {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Path::new(s )?)
    }
}




#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct Version
{
    string: String
}

impl Version
{
    pub fn new( string: &str ) -> Result<Self,Error> {
        if string.is_empty() {
            return Err("path cannot be empty".into());
        }

        // here we are just verifying that it parses and normalizing the output
        let version = semver::Version::parse(string)?;

        Ok(Version{
            string: version.to_string()
        })
    }
}

impl Version
{
    pub fn as_semver(&self)->Result<semver::Version,Error>
    {
        Ok(semver::Version::parse(self.string.as_str() )?)
    }
}

impl ToString for Version {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl FromStr for Version {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Version::new(s )?)
    }
}


#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceCreate {
   pub parent: ResourceKey,
   pub key: KeyCreationSrc,
   pub address: AddressCreationSrc,
   pub init: ResourceInit,
   pub registry_info: Option<ResourceRegistryInfo>,
   pub owner: Option<UserKey>,
   pub location_affinity: Option<ResourceLocationAffinity>
}


impl ResourceCreate {

    pub fn new( init: ResourceInit ) ->Self {
        ResourceCreate {
            parent: ResourceKey::Nothing,
            key: KeyCreationSrc::None,
            address: AddressCreationSrc::None,
            init: init,
            registry_info: Option::None,
            owner: Option::None,
            location_affinity: Option::None
        }
    }

    pub fn validate(&self)->Result<(),Fail> {
        let resource_type = self.init.archetype.kind.resource_type();

        self.init.archetype.valid()?;

        if resource_type.requires_owner() && self.owner.is_none() {
            return Err(Fail::ResourceTypeRequiresOwner);
        };

        if let KeyCreationSrc::Key(key) = &self.key {
            if key.resource_type() != resource_type {
                return Err(Fail::ResourceTypeMismatch("ResourceCreate: key: KeyCreationSrc::Key(key) resource type != init.archetype.kind.resource_type()".into()));
            }
        }

        Ok(())
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub enum ResourceStatus {
    Unknown,
    Preparing,
    Ready
}
impl ToString for ResourceStatus{
    fn to_string(&self) -> String {
        match self {
            Self::Unknown => "Unknown".to_string(),
            Self::Preparing => "Preparing".to_string(),
            Self::Ready => "Ready".to_string()
        }
    }
}

impl FromStr for ResourceStatus{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Unknown" => Ok(Self::Unknown),
            "Preparing" => Ok(Self::Preparing),
            "Ready" => Ok(Self::Ready),
            what => Err(format!("not recognized: {}",what).into())
        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub enum AddressCreationSrc{
    None,
    Append(String),
    Space(String)
}

#[derive(Clone,Serialize,Deserialize)]
pub enum KeyCreationSrc {
    None,
    Key(ResourceKey)
}

#[derive(Clone,Serialize,Deserialize)]
pub enum KeySrc{
    None,
    Key(ResourceKey),
    Address(ResourceAddress)
}

#[derive(Clone,Serialize,Deserialize)]
pub enum ResourceSrc {
    Creation(ResourceInit),
    //Assign(ResourceState) -- this is the mechanism that a Resource would be transfered to a new Host
}

#[derive(Clone,Serialize,Deserialize,Eq,PartialEq)]
pub enum ResourceSliceStatus {
    Unknown,
    Preparing,
    Waiting,
    Ready
}

impl ToString for ResourceSliceStatus{
    fn to_string(&self) -> String {
        match self {
            ResourceSliceStatus::Unknown => "Unknown".to_string(),
            ResourceSliceStatus::Preparing => "Preparing".to_string(),
            ResourceSliceStatus::Waiting => "Waiting".to_string(),
            ResourceSliceStatus::Ready => "Ready".to_string()
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
            what => Err(format!("not recognized: {}",what).into())

        }
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceSliceAssign{
    key: ResourceKey,
    archetype: ResourceArchetype
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceSliceInit{
    key: ResourceKey,
    profile: ResourceInit
}

#[derive(Clone,Serialize,Deserialize)]
pub enum ResourceSliceCommand {
    Init(ResourceSliceInit),
    NotifyInit(ResourceKey)
}

#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceStub {
    pub key: ResourceKey,
    pub address: ResourceAddress,
    pub archetype: ResourceArchetype,
    pub owner: Option<UserKey>
}

impl ResourceStub {
    pub fn validate( &self, resource_type: ResourceType ) -> bool {
        self.key.resource_type() == resource_type && self.address.resource_type == resource_type && self.archetype.kind.resource_type() == resource_type
    }
}


#[derive(Clone,Serialize,Deserialize)]
pub struct ResourceAssign{
    pub key: ResourceKey,
    pub address: ResourceAddress,
    pub source: ResourceSrc
}

impl ResourceAssign {
    pub fn key(&self) -> ResourceKey {
        self.key.clone()
    }

    pub fn archetype(&self) -> ResourceArchetype {
        match &self.source{
            ResourceSrc::Creation(profile) => profile.archetype.clone()
        }
    }
}

pub struct LocalResourceHost{
    skel: StarSkel,
    resource: ResourceStub
}

impl LocalResourceHost{

    async fn assign(&self, assign: ResourceAssign) -> Result<Self, Fail> {
        unimplemented!()
        /*
        let archetype = match assign.source{
            ResourceSrc::Creation(init) => {
                init.archetype.clone()
            }
        };

        Ok(LocalResourceHost{
            resource: Resource{
                key: assign.key,
                address: assign.address,
                archetype: archetype,
                owner: Option::None
            }
        })
         */
    }
}



#[async_trait]
impl ResourceHost for LocalResourceHost{
    fn star_key(&self) -> StarKey {
        self.skel.info.star.clone()
    }

    async fn assign(&self, assign: ResourceAssign) -> Result<(), Fail> {
        unimplemented!()
    }
}

pub struct RemoteResourceHost {
    pub comm: StarComm,
    pub handle: StarHandle
}

#[async_trait]
impl ResourceHost for RemoteResourceHost {
    fn star_key(&self) -> StarKey {
        self.handle.key.clone()
    }

    async fn assign( &self, assign: ResourceAssign) -> Result<(), Fail> {

        if !self.handle.kind.hosts().contains(&assign.key.resource_type() ) {
            return Err(Fail::WrongResourceType{
                expected: self.handle.kind.hosts().clone(),
                received: assign.key.resource_type().clone()
            });
        }
        let mut proto = ProtoMessage::new();
        proto.to = Option::Some(self.handle.key.clone());
        proto.payload = StarMessagePayload::ResourceHost(ResourceHostAction::Assign(assign));
        let reply = proto.get_ok_result().await;
        self.comm.star_tx.send( StarCommand::SendProtoMessage(proto)).await;

        match tokio::time::timeout( Duration::from_secs(5), reply).await{
            Ok(result) => {
                if let Result::Ok( StarMessagePayload::Reply(SimpleReply::Ok(Reply::Resource(resource)))) = result{
                    Ok(())
                } else {
                    Err(Fail::Unexpected)
                }
            }
            Err(err) => {

                Err(Fail::Timeout)
            }
        }
    }
}

pub trait Resource<S:State> : Send+Sync {
    fn key(&self)->ResourceKey;
    fn address(&self)->ResourceAddress;
    fn resource_type(&self)->ResourceType;
    fn state(&self) -> StateSrc<S>;
}

pub trait State : Send+Sync+Clone {

    fn to_bytes(self) ->Result<Vec<u8>,Error>;
}

pub enum StateSrc<S> where S: State {
    Memory(S)
}

impl <S> StateSrc<S> where S: State {
    pub fn to_data(self)->Result<Vec<u8>,Error>{
        match self{
            StateSrc::Memory(data) => Ok(data)
        }
    }
}


