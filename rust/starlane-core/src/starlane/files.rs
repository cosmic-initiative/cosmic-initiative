use tokio::runtime::Handle;
use tokio::runtime::Runtime;

use starlane_resources::data::BinContext;
use crate::error::Error;
use crate::file_access::FileAccess;
use crate::star::StarKey;
use std::collections::HashSet;
use tokio::sync::RwLock;

pub struct MachineFileSystem {
    runtime: Runtime,
    local_stars: RwLock<HashSet<StarKey>>,
    data_access: FileAccess,
}

impl MachineFileSystem {
    pub fn new() -> Result<Self, Error> {
        Ok(Self {
            runtime: Runtime::new().expect("expected a new tokio Runtime"),
            local_stars: RwLock::new(HashSet::new()),
            data_access: FileAccess::new(
                std::env::var("STARLANE_DATA").unwrap_or("data".to_string()),
            )?,
        })
    }

    pub async fn add_local_star(&self, star: StarKey) {
        let mut lock = self.local_stars.write().await;
        lock.insert(star);
    }

    pub fn data_access(&self) -> FileAccess {
        self.data_access.clone()
    }
}

#[async_trait]
impl BinContext for MachineFileSystem {

}