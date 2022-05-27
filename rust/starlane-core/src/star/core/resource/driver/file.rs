use std::convert::{TryFrom, TryInto};
use std::sync::Arc;

use clap::{App, AppSettings};
use yaml_rust::Yaml;

use crate::artifact::ArtifactRef;
use crate::error::Error;
use crate::particle::{ArtifactSubKind, KindBase, ParticleAssign, AssignParticleStateSrc, Kind, FileSubKind};
use crate::star::core::resource::driver::ParticleCoreDriver;
use crate::star::core::resource::state::StateStore;
use crate::star::StarSkel;
use crate::watch::{Notification, Change, Topic, WatchSelector, Property};
use crate::message::delivery::Delivery;
use crate::frame::{StarMessagePayload, StarMessage};

use std::str::FromStr;
use mesh_portal::version::latest::command::common::{SetProperties, StateSrc};
use mesh_portal::version::latest::entity::request::create::{PointSegFactory, PointTemplate, Create, KindTemplate, Strategy, Template};
use mesh_portal::version::latest::entity::request::{Method, Rc};
use mesh_portal::version::latest::id::{Point, AddressAndKind, KindParts};
use mesh_portal::version::latest::messaging::Request;

#[derive(Debug)]
pub struct FileCoreManager {
    skel: StarSkel,
    store: StateStore,
}

impl FileCoreManager {
    pub fn new(skel: StarSkel) -> Self {
        FileCoreManager {
            skel: skel.clone(),
            store: StateStore::new(skel),
        }
    }
}

#[async_trait]
impl ParticleCoreDriver for FileCoreManager {
    async fn assign(
        &mut self,
        assign: ParticleAssign,
    ) -> Result<(), Error> {

        let kind : Kind = TryFrom::try_from(assign.details.stub.kind)?;
        if let Kind::File(file_kind) = kind
        {
            match file_kind {
                FileSubKind::Dir => {
                    // stateless
                }
                _ => {
                    let state = match &assign.state {
                        StateSrc::StatefulDirect(data) => {
                            data.clone()
                        },
                        StateSrc::Stateless => {
                            return Err("Artifact cannot be stateless".into())
                        },
                    };
                    self.store.put(assign.details.stub.point.clone(), state.clone() ).await?;

                    let selector = WatchSelector{
                        topic: Topic::Point(assign.details.stub.point),
                        property: Property::State
                    };

                    self.skel.watch_api.fire( Notification::new(selector, Change::State(state) ));

                }
            }
        } else {
            return Err("File Manager unexpected kind".into() );
        }


        Ok(())
    }


    fn kind(&self) -> KindBase {
        KindBase::File
    }

}


pub struct FileSystemManager {
    skel: StarSkel,
    store: StateStore,
}

impl FileSystemManager {
    pub async fn new( skel: StarSkel ) -> Self {

        FileSystemManager {
            skel: skel.clone(),
            store: StateStore::new(skel),
        }
    }
}

#[async_trait]
impl ParticleCoreDriver for FileSystemManager {
    fn kind(&self) -> KindBase {
        KindBase::FileSystem
    }

    async fn assign(
        &mut self,
        assign: ParticleAssign,
    ) -> Result<(), Error> {
        match assign.state {
            StateSrc::Stateless => {}
            _ => {
                return Err("must be stateless or empty create args".into());
            }
        };


        let root_point_and_kind = AddressAndKind {
            point: Point::from_str( format!("{}:/", assign.details.stub.point.to_string()).as_str())?,
            kind: KindParts { kind: "File".to_string(), sub_kind: Option::Some("Dir".to_string()), specific: None }
        };

        let skel = self.skel.clone();
        tokio::spawn( async move {
            let create = Create {
                template: Template {
                    point: PointTemplate { parent: assign.details.stub.point.clone(), child_segment_template: PointSegFactory::Exact(root_point_and_kind.point.last_segment().expect("expected final segment").to_string()) },
                    kind: KindTemplate { kind: root_point_and_kind.kind.kind.clone(), sub_kind: root_point_and_kind.kind.sub_kind.clone(), specific: None }
                },
                state: StateSrc::Stateless,
                properties: SetProperties::new(),
                strategy: Strategy::Create,
                registry: Default::default()
            };

            let action = Method::Cmd(Rc::Create(create));
            let core = action.into();
            let request = Request::new(core, assign.details.stub.point.clone(), assign.details.stub.point.clone());
            let response = skel.messaging_api.request(request).await;
        });
        Ok(())
    }



}
