use std::convert::{TryFrom, TryInto};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::error::Error;
use crate::resource::{
    ResourceType,
};

#[derive(Clone)]
pub struct FileSystem {
    key: FileSystemKey,
    address: ResourceAddress,
    state_src: DataSet<BinSrc>,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct FileSystemState {}

impl FileSystemState {
    pub fn new() -> Self {
        FileSystemState {}
    }
}

impl TryInto<Vec<u8>> for FileSystemState {
    type Error = Error;

    fn try_into(self) -> Result<Vec<u8>, Self::Error> {
        Ok(bincode::serialize(&self)?)
    }
}

impl TryInto<Arc<Vec<u8>>> for FileSystemState {
    type Error = Error;

    fn try_into(self) -> Result<Arc<Vec<u8>>, Self::Error> {
        Ok(Arc::new(bincode::serialize(&self)?))
    }
}

impl TryFrom<Arc<Vec<u8>>> for FileSystemState {
    type Error = Error;

    fn try_from(value: Arc<Vec<u8>>) -> Result<Self, Self::Error> {
        Ok(bincode::deserialize::<FileSystemState>(value.as_slice())?)
    }
}

impl TryFrom<Vec<u8>> for FileSystemState {
    type Error = Error;

    fn try_from(value: Vec<u8>) -> Result<Self, Self::Error> {
        Ok(bincode::deserialize::<FileSystemState>(value.as_slice())?)
    }
}
