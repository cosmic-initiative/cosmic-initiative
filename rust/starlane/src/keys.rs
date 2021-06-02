use std::fmt;
use std::str::FromStr;

use bincode::deserialize;
use serde::{Deserialize, Serialize, Serializer};
use uuid::Uuid;

use crate::actor::{Actor, ActorKey, ActorKind};
use crate::app::AppKind;
use crate::artifact::{Artifact, ArtifactKey, ArtifactKind, ArtifactId};
use crate::error::Error;
use crate::frame::Reply;
use crate::id::Id;
use crate::message::Fail;
use crate::names::Name;
use crate::permissions::{Priviledges, User, UserKind};
use crate::resource::{Labels, ResourceStub, ResourceAddressPart, ResourceArchetype, ResourceAssign, ResourceKind, ResourceManagerKey, ResourceType, SkewerCase};
use std::collections::HashSet;
use std::iter::FromIterator;

pub type SpaceId = u32;

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum SpaceKey
{
    HyperSpace,
    Space(SpaceId)
}

impl SpaceKey
{

    pub fn hyper_space() -> Self {
        Self::from_index(0)
    }

    pub fn from_index(index: u32) -> Self
    {
        if index == 0
        {
            SpaceKey::HyperSpace
        }
        else
        {
            SpaceKey::Space(index)
        }
    }

    pub fn id(&self) ->SpaceId
    {
        match self
        {
            SpaceKey::HyperSpace => 0,
            SpaceKey::Space(index) => index.clone()
        }
    }
}

impl fmt::Display for SpaceKey{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}",
                match self{
                    SpaceKey::HyperSpace => "HyperSpace".to_string(),
                    SpaceKey::Space(index) => index.to_string()
                })
    }

}


pub type UserId=i32;

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub struct UserKey
{
  pub space: SpaceKey,
  pub id: UserId
}

impl UserKey
{
    pub fn bin(&self)->Result<Vec<u8>,Error>
    {
        let mut bin= bincode::serialize(self)?;
        Ok(bin)
    }

    pub fn from_bin(mut bin: Vec<u8> )->Result<Self,Error>
    {
        let mut key = bincode::deserialize::<Self>(bin.as_slice() )?;
        Ok(key)
    }
}



impl UserKey
{
    pub fn new(space: SpaceKey, id: UserId) -> Self
    {
        UserKey{
            space,
            id: id
        }
    }


    pub fn hyper_user() -> Self
    {
        UserKey::new(SpaceKey::HyperSpace, 0)
    }


    pub fn super_user(space: SpaceKey) -> Self
    {
        UserKey::new(space,0)
    }



    pub fn is_hyperuser(&self)->bool
    {
        match self.space{
            SpaceKey::HyperSpace => {
                match self.id
                {
                    0 => true,
                    _ => false
                }
            }
            _ => false
        }
    }

    pub fn privileges(&self) -> Priviledges
    {
        if self.is_hyperuser()
        {
            Priviledges::all()
        }
        else {
            Priviledges::new()
        }
    }
}

impl fmt::Display for UserKey{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"({},{})",self.space, self.id.to_string())
    }

}





#[derive(Clone,Serialize,Deserialize,Eq,PartialEq,Hash)]
pub struct SubSpaceKey
{
    pub space: SpaceKey,
    pub id: SubSpaceId
}

impl SubSpaceKey
{
    pub fn hyper_default( ) -> Self
    {
        SubSpaceKey::new(SpaceKey::HyperSpace, 0 )
    }

    pub fn new( space: SpaceKey, id: SubSpaceId ) -> Self
    {
        SubSpaceKey{
            space: space,
            id: id
        }
    }
}


impl fmt::Display for SubSpaceKey{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}-{}",self.space, self.id.to_string())
    }

}


pub type SubSpaceId = u32;


#[derive(Clone,Hash,Eq,PartialEq,Serialize,Deserialize)]
pub struct AppKey
{
    pub sub_space: SubSpaceKey,
    pub id: AppId
}

impl AppKey {
    pub fn address_part(&self) -> Result<ResourceAddressPart,Error>{
        Ok(ResourceAddressPart::SkewerCase(SkewerCase::new(self.id.to_string().as_str() )?))
    }
}



impl AppKey
{
    pub fn bin(&self)->Result<Vec<u8>,Error>
    {
        let mut bin= bincode::serialize(self)?;
        Ok(bin)
    }

    pub fn from_bin(mut bin: Vec<u8> )->Result<AppKey,Error>
    {
        let mut key = bincode::deserialize::<AppKey>(bin.as_slice() )?;
        Ok(key)
    }

}

pub type AppId=u64;

impl AppKey
{
    pub fn new( sub_space: SubSpaceKey, id: AppId )->Self
    {
        AppKey{
            sub_space: sub_space,
            id: id
        }
    }
}

impl fmt::Display for AppKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({},{})", self.sub_space, self.id.to_string())
    }
}





pub type MessageId = Uuid;

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum ResourceId{
    Nothing,
    Space(u32),
    SubSpace(SubSpaceId),
    App(AppId),
    Actor(Id),
    User(UserId),
    Artifact(ArtifactId),
    File(FileId),
    FileSystem(FileSystemId),
}

impl ResourceId{
    pub fn resource_type(&self)->ResourceType{
        match self{
            ResourceId::Nothing => ResourceType::Nothing,
            ResourceId::Space(_) => ResourceType::Space,
            ResourceId::SubSpace(_) => ResourceType::SubSpace,
            ResourceId::App(_) => ResourceType::App,
            ResourceId::Actor(_) => ResourceType::Actor,
            ResourceId::User(_) => ResourceType::User,
            ResourceId::Artifact(_) => ResourceType::Artifact,
            ResourceId::File(_) => ResourceType::File,
            ResourceId::FileSystem(_) => ResourceType::FileSystem,
        }
    }

    pub fn new( resource_type: &ResourceType, id: Id ) -> Self{
        match resource_type{
            ResourceType::Nothing => Self::Nothing,
            ResourceType::Space => Self::Space(id.index as _),
            ResourceType::SubSpace =>  Self::SubSpace(id.index as _),
            ResourceType::App =>  Self::App(id.index as _),
            ResourceType::Actor =>  Self::Actor(id),
            ResourceType::User =>  Self::User(id.index as _),
            ResourceType::FileSystem =>  Self::FileSystem(id.index as _),
            ResourceType::File =>  Self::File(id.index as _),
            ResourceType::Artifact =>  Self::Artifact(id.index as _),
        }
    }
}

impl ToString for ResourceId{
    fn to_string(&self) -> String {
        self.resource_type().to_string()
    }
}

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum ResourceKey
{
    Nothing,
    Space(SpaceKey),
    SubSpace(SubSpaceKey),
    App(AppKey),
    Actor(ActorKey),
    User(UserKey),
    Artifact(ArtifactKey),
    File(FileKey),
    FileSystem(FileSystemKey),
}

impl ResourceKey
{
    pub fn new(parent: ResourceKey, id: ResourceId) -> Result<Self,Error> {

        match id{
            ResourceId::Nothing => {
                Ok(Self::Nothing)
            }
            ResourceId::Space(id) => {
                Ok(Self::Space( SpaceKey::from_index(id) ))
            }
            ResourceId::SubSpace(index) => {
                if let Self::Space(parent) = parent {
                    Ok(Self::SubSpace( SubSpaceKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::App(index) => {
                if let Self::SubSpace(parent) = parent {
                    Ok(Self::App( AppKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::Actor(index) => {
                if let Self::App(parent) = parent {
                    Ok(Self::Actor( ActorKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::User(index) => {
                if let Self::Space(parent) = parent {
                    Ok(Self::User( UserKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::Artifact(index) => {
                if let Self::SubSpace(parent) = parent {
                    Ok(Self::Artifact( ArtifactKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::File(index) => {
                if let Self::FileSystem(parent) = parent {
                    Ok(Self::File( FileKey::new(parent,index)))
                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
            ResourceId::FileSystem(index) => {
                if let Self::SubSpace(parent) = parent {
                    Ok(Self::FileSystem(FileSystemKey::SubSpace(SubSpaceFilesystemKey { sub_space: parent, id: index })))
                }
                else if let Self::App(parent) = parent {
                    Ok(Self::FileSystem(FileSystemKey::App(AppFilesystemKey{ app: parent, id: index })))

                } else {
                    Err(format!("mismatched types! parent {} is not compatible with id: {}",parent,id.to_string()).into())
                }
            }
        }
    }

    pub fn generate_address_tail(&self) -> Result<String,Fail>{
        match self{
            ResourceKey::Nothing => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::Space(_) => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::SubSpace(_) => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::App(app) => {
                Ok(app.id.to_string())
            }
            ResourceKey::Actor(actor) => {
                Ok(actor.id.to_string())
            }
            ResourceKey::User(user) => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::Artifact(_) => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::File(_) => Err(Fail::ResourceCannotGenerateAddress),
            ResourceKey::FileSystem(_) => Err(Fail::ResourceCannotGenerateAddress),
        }
    }

    pub fn parent(&self)->Option<ResourceKey>{
        match self {
            ResourceKey::Nothing => Option::None,
            ResourceKey::Space(_) => Option::Some(ResourceKey::Nothing),
            ResourceKey::SubSpace(sub_space) => Option::Some(ResourceKey::Space(sub_space.space.clone())),
            ResourceKey::App(app) =>  Option::Some(ResourceKey::SubSpace(app.sub_space.clone())),
            ResourceKey::Actor(actor) =>  Option::Some(ResourceKey::App(actor.app.clone())),
            ResourceKey::User(user) => Option::Some(ResourceKey::Space(user.space.clone())),
            ResourceKey::Artifact(artifact) => Option::Some(ResourceKey::SubSpace(artifact.sub_space.clone())),
            ResourceKey::File(file) => Option::Some(ResourceKey::FileSystem(file.filesystem.clone())),
            ResourceKey::FileSystem(filesystem) => {
                match filesystem{
                    FileSystemKey::App(app) => {
                        Option::Some(ResourceKey::App(app.app.clone()))
                    }
                    FileSystemKey::SubSpace(sub_space) => {
                        Option::Some(ResourceKey::SubSpace(sub_space.sub_space.clone()))
                    }
                }
            }
        }
    }

    pub fn space(&self)->Result<SpaceKey,Fail> {
        match self{
            ResourceKey::Nothing => Err(Fail::WrongResourceType { expected: HashSet::from_iter(vec![ResourceType::Space,ResourceType::SubSpace,ResourceType::App,ResourceType::Actor,ResourceType::User,ResourceType::Artifact,ResourceType::FileSystem,ResourceType::File] ), received: ResourceType::Nothing }),
            ResourceKey::Space(space) => Ok(space.clone()),
            ResourceKey::SubSpace(sub_space) => Ok(sub_space.space.clone()),
            ResourceKey::App(app) => Ok(app.sub_space.space.clone()),
            ResourceKey::Actor(actor) => Ok(actor.app.sub_space.space.clone()),
            ResourceKey::User(user) => Ok(user.space.clone()),
            ResourceKey::Artifact(artifact) => Ok(artifact.sub_space.space.clone()),
            ResourceKey::File(file) => Ok(match &file.filesystem{
                FileSystemKey::App(app) => app.app.sub_space.space.clone(),
                FileSystemKey::SubSpace(sub_space) => sub_space.sub_space.space.clone(),
            }),
            ResourceKey::FileSystem(filesystem) => {
                Ok(match filesystem{
                    FileSystemKey::App(app) => app.app.sub_space.space.clone(),
                    FileSystemKey::SubSpace(sub_space) => sub_space.sub_space.space.clone(),
                })
            }
        }
    }

/*    pub fn sub_space(&self)->Result<SubSpaceKey,Fail> {
        match self{
            ResourceKey::SubSpace(sub_space) => Ok(sub_space.clone()),
            ResourceKey::App(app) => Ok(app.sub_space.clone()),
            ResourceKey::Actor(actor) => Ok(actor.app.sub_space.clone()),
            ResourceKey::Artifact(artifact) => Ok(artifact.sub_space.clone()),
            ResourceKey::File(file) => Ok(match &file.filesystem{
                FileSystemKey::App(app) => app.app.sub_space.clone(),
                FileSystemKey::SubSpace(sub_space) => sub_space.sub_space.clone(),
            }),
            ResourceKey::FileSystem(filesystem) => {
                Ok(match filesystem{
                    FileSystemKey::App(app) => app.app.sub_space.clone(),
                    FileSystemKey::SubSpace(sub_space) => sub_space.sub_space.clone(),
                })
            }
            received => Err(Fail::WrongResourceType { expected: HashSet::from_iter(vec![ResourceType::SubSpace,ResourceType::App,ResourceType::Artifact,ResourceType::File,ResourceType::FileSystem] ), received: received.resource_type().clone() }),
        }
    }

 */


    pub fn user(&self)->Result<UserKey,Fail> {
        match self{
            ResourceKey::User(user) => Ok(user.clone()),
            received => Err(Fail::WrongResourceType { expected: HashSet::from_iter(vec![ResourceType::User] ), received: received.resource_type().clone() }),
        }
    }

    pub fn actor(&self)->Result<ActorKey,Fail> {
        if let ResourceKey::Actor(key) = self {
            Ok(key.clone())
        } else {
            Err(Fail::WrongResourceType{expected: HashSet::from_iter(vec![ResourceType::Actor]), received: self.resource_type().clone() })
        }
    }

    pub fn app(&self)->Result<AppKey,Fail> {
        match self{
            ResourceKey::App(app) => {
                Result::Ok(app.clone())
            }
            ResourceKey::Actor(actor) => {
                ResourceKey::Actor(actor.clone()).parent().unwrap().app()
            }
            ResourceKey::FileSystem(filesystem) => {
                match filesystem{
                    FileSystemKey::App(app) => {
                        ResourceKey::FileSystem(FileSystemKey::App(app.clone())).parent().unwrap().app()
                    }
                    _ => {

                        Err(Fail::WrongResourceType{expected: HashSet::from_iter(vec![ResourceType::App,ResourceType::Actor,ResourceType::FileSystem]), received: self.resource_type().clone() })
                    }
                }
            }
            _ => {
                Err(Fail::WrongResourceType{expected: HashSet::from_iter(vec![ResourceType::App,ResourceType::Actor,ResourceType::FileSystem]), received: self.resource_type().clone() })
            }
        }
    }

    pub fn file(&self)->Result<FileKey,Fail> {
        if let ResourceKey::File(key) = self {
            Ok(key.clone())
        } else {
            Err(Fail::WrongResourceType{expected: HashSet::from_iter(vec![ResourceType::File]), received: self.resource_type().clone() })
        }
    }

    pub fn encode(&self)->Result<String,Error> {
        Ok(base64::encode(self.bin()?))
    }

    pub fn decode( string: String )->Result<Self,Error>{
        Ok(ResourceKey::from_bin(base64::decode(string)?)?)
    }



    pub fn manager(&self)->ResourceManagerKey
    {
        match self
        {
            ResourceKey::Nothing => ResourceManagerKey::Central,
            ResourceKey::Space(_) => ResourceManagerKey::Central,
            ResourceKey::SubSpace(sub_space) => {
                //ResourceManagerKey::Key(ResourceKey::Space(sub_space.space.clone()))
                ResourceManagerKey::Central
            }
            ResourceKey::App(app) => {
                //ResourceManagerKey::Key(ResourceKey::Space(app.sub_space.space.clone()))
                ResourceManagerKey::Central
            }
            ResourceKey::Actor(actor) => {
                ResourceManagerKey::Key(ResourceKey::App(actor.app.clone()))
            }
            ResourceKey::User(user) => {
                //ResourceManagerKey::Key(ResourceKey::Space(user.space.clone()))
                ResourceManagerKey::Central
            }
            ResourceKey::File(file) => {
                //ResourceManagerKey::Key(ResourceKey::App(file.app.clone()))
                ResourceManagerKey::Central
            }
            ResourceKey::Artifact(artifact) => {
                //ResourceManagerKey::Key(ResourceKey::Space(artifact.sub_space.space.clone()))
                ResourceManagerKey::Central
            }
            ResourceKey::FileSystem(key) => {
                match key
                {
                    FileSystemKey::App(app) => {
                        //ResourceManagerKey::Key(ResourceKey::Space(app.sub_space.space.clone()))
                        ResourceManagerKey::Central
                    }
                    FileSystemKey::SubSpace(sub_space) => {
                        //ResourceManagerKey::Key(ResourceKey::Space(app.sub_space.space.clone()))
                        ResourceManagerKey::Central
                    }
                }
            }
        }

    }
}

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum FileSystemKey
{
    App(AppFilesystemKey),
    SubSpace(SubSpaceFilesystemKey)
}



pub type FileSystemId = u32;

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub struct AppFilesystemKey
{
    pub app: AppKey,
    pub id: FileSystemId
}

#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub struct SubSpaceFilesystemKey
{
    pub sub_space: SubSpaceKey,
    pub id: FileSystemId
}


#[derive(Clone,Serialize,Deserialize,Hash,Eq,PartialEq)]
pub enum GatheringKey {
  Actor(ActorKey)
}

impl GatheringKey
{
    pub fn bin(&self) -> Result<Vec<u8>, Error>
    {
        let mut bin = bincode::serialize(self)?;
        Ok(bin)
    }

    pub fn from_bin(mut bin: Vec<u8>) -> Result<GatheringKey, Error>
    {
        let mut key = bincode::deserialize::<GatheringKey>(bin.as_slice())?;
        Ok(key)
    }
}

impl fmt::Display for ResourceKey{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}",
                match self{
                    ResourceKey::Space(key) => format!("SpaceKey:{}",key),
                    ResourceKey::SubSpace(key) => format!("SubSpaceKey:{}",key),
                    ResourceKey::App(key)  => format!("AppKey:{}",key),
                    ResourceKey::Actor(key) => format!("ActorKey:{}",key),
                    ResourceKey::User(key) => format!("UserKey:{}",key),
                    ResourceKey::File(key) => format!("FileKey:{}",key),
                    ResourceKey::Artifact(key) => format!("ArtifactKey:{}",key),
                    ResourceKey::FileSystem(key) => format!("FileSystemKey:{}", key),
                    ResourceKey::Nothing => "Nothing".to_string()
                })
    }
}

impl ResourceKey
{
    pub fn resource_type(&self) -> ResourceType
    {
        match self
        {
            ResourceKey::Nothing => ResourceType::Nothing,
            ResourceKey::Space(_) => ResourceType::Space,
            ResourceKey::SubSpace(_) => ResourceType::SubSpace,
            ResourceKey::App(_) => ResourceType::App,
            ResourceKey::Actor(_) => ResourceType::Actor,
            ResourceKey::User(_) => ResourceType::User,
            ResourceKey::File(_) => ResourceType::File,
            ResourceKey::Artifact(_) => ResourceType::Artifact,
            ResourceKey::FileSystem(_) => ResourceType::FileSystem,
        }
    }

    pub fn sub_space(&self)->Result<SubSpaceKey,Error>
    {
        match self
        {
            ResourceKey::Nothing => Err("nothign does not have a subspace".into()),
            ResourceKey::Space(_) => Err("space does not have a subspace".into()),
            ResourceKey::SubSpace(sub_space) => Ok(sub_space.clone()),
            ResourceKey::App(app) => Ok(app.sub_space.clone()),
            ResourceKey::Actor(actor) => Ok(actor.app.sub_space.clone()),
            ResourceKey::User(user) => Err("user does not have a sub_space".into()),
            ResourceKey::File(file) => match &file.filesystem{
                FileSystemKey::App(app) => {
                    Ok(app.app.sub_space.clone())
                }
                FileSystemKey::SubSpace(sub_space) => {
                    Ok(sub_space.sub_space.clone())
                }
            },
            ResourceKey::Artifact(artifact) => Ok(artifact.sub_space.clone()),
            ResourceKey::FileSystem(filesystem) => {
                match filesystem{
                    FileSystemKey::App(app) => {
                        Ok(app.app.sub_space.clone())
                    }
                    FileSystemKey::SubSpace(sub_space) => {
                        Ok(sub_space.sub_space.clone())
                    }
                }
            }
        }
    }


    pub fn bin(&self)->Result<Vec<u8>,Error>
    {
        let mut bin= bincode::serialize(self)?;
        bin.insert(0, self.resource_type().magic() );
        Ok(bin)
    }

    pub fn from_bin(mut bin: Vec<u8> )->Result<ResourceKey,Error>
    {
        bin.remove(0);
        let mut key = bincode::deserialize::<ResourceKey>(bin.as_slice() )?;
        Ok(key)
    }



}

impl From<Vec<ResourceStub>> for Reply
{
    fn from(resources: Vec<ResourceStub>) -> Self {
        Reply::Keys(resources.iter().map(|r|r.key.clone()).collect())
    }
}

impl fmt::Display for FileSystemKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"{}",
                match self{
                    FileSystemKey::App(_) => "App",
                    FileSystemKey::SubSpace(_) => "SubSpace"
                })
    }
}

#[derive(Clone,Eq,PartialEq,Hash,Serialize,Deserialize)]
pub struct FileKey
{
   pub filesystem: FileSystemKey,
   pub id: FileId
}

impl FileKey{
    pub fn new(filesystem: FileSystemKey, id: FileId ) -> Self {
        FileKey{
            filesystem: filesystem,
            id: id
        }
    }
}

pub type FileId = u64;


impl fmt::Display for FileKey{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!( f,"[{},{},{}]",self.filesystem,self.filesystem,self.id)
    }
}

#[derive(Clone,Serialize,Deserialize)]
pub enum Unique {
    Sequence,
    Index
}

#[async_trait]
pub trait UniqueSrc: Send+Sync{
    async fn next(&self, unique: Unique) -> Result<u64,Fail>;
}


