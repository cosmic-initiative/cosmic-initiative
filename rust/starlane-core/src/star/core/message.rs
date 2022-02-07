use std::collections::HashMap;
use std::convert::TryInto;

use tokio::sync::oneshot::error::RecvError;
use tokio::sync::{mpsc, oneshot};
use std::str::FromStr;
use crate::error::Error;
use crate::fail::{Fail, StarlaneFailure};
use crate::frame::{
    ResourceHostAction, ResourceRegistryRequest, SimpleReply, StarMessage, StarMessagePayload,
};

use crate::message::delivery::Delivery;
use crate::message::{ProtoStarMessage, ProtoStarMessageTo, Reply, ReplyKind};
use crate::resource::{ArtifactKind, Kind, ResourceType, BaseKind, FileKind, ResourceLocation};
use crate::resource::{AssignKind, ResourceAssign, ResourceRecord};
use crate::star::core::resource::registry::{RegError, Registration};
use crate::star::shell::wrangler::{ StarFieldSelection, StarSelector};
use crate::star::{StarCommand, StarKey, StarKind, StarSkel};
use crate::util::{AsyncProcessor, AsyncRunner, Call};
use mesh_portal_serde::version::latest::fail::BadRequest;
use std::future::Future;
use std::sync::Arc;
use mesh_portal_serde::version::latest::command::common::StateSrc;
use mesh_portal_serde::version::latest::config::bind::{BindConfig, Pipeline};
use mesh_portal_serde::version::latest::config::Config;
use mesh_portal_serde::version::latest::entity::request::create::{AddressSegmentTemplate, KindTemplate, Strategy};
use mesh_portal_serde::version::latest::entity::request::{Msg, Rc, RcCommand, ReqEntity};
use mesh_portal_serde::version::latest::entity::request::get::Get;
use mesh_portal_serde::version::latest::fail;
use mesh_portal_serde::version::latest::http::{HttpRequest, HttpResponse};
use mesh_portal_serde::version::latest::id::{Address, Meta};
use mesh_portal_serde::version::latest::messaging::{Message, Request, Response};
use mesh_portal_serde::version::latest::payload::{Payload, PayloadMap, Primitive, PrimitiveList};
use mesh_portal_serde::version::latest::resource::{ResourceStub, Status};
use mesh_portal_versions::version::v0_0_1::config::bind::{PipelineSegment, PipelineStep, PipelineStop, Selector, StepKind};
use mesh_portal_versions::version::v0_0_1::entity::request::get::GetOp;
use mesh_portal_versions::version::v0_0_1::entity::response::{PayloadResponse, RespEntity};
use mesh_portal_versions::version::v0_0_1::id::Tks;
use mesh_portal_versions::version::v0_0_1::pattern::{Block, HttpPattern};
use mesh_portal_versions::version::v0_0_1::payload::CallKind;
use regex::Regex;
use serde::de::Unexpected::Str;
use crate::artifact::ArtifactRef;
use crate::cache::{ArtifactItem, CachedConfig};
use crate::star::core::resource::manager::{ResourceManagerApi, ResourceManagerComponent};

pub enum CoreMessageCall {
    Message(StarMessage),
}

impl Call for CoreMessageCall {}

pub struct MessagingEndpointComponent {
    skel: StarSkel,
    resource_manager_api: ResourceManagerApi
}

impl MessagingEndpointComponent {
    pub fn start(skel: StarSkel, rx: mpsc::Receiver<CoreMessageCall>) {
        let (resource_manager_tx, resource_manager_rx) = mpsc::channel(1024);
        let resource_manager_api= ResourceManagerApi::new(resource_manager_tx.clone());
        ResourceManagerComponent::new(skel.clone(), resource_manager_tx, resource_manager_rx );

        AsyncRunner::new(
            Box::new(Self {
                skel: skel.clone(),
                resource_manager_api
            }),
            skel.core_messaging_endpoint_tx.clone(),
            rx,
        );
    }
}

#[async_trait]
impl AsyncProcessor<CoreMessageCall> for MessagingEndpointComponent {
    async fn process(&mut self, call: CoreMessageCall) {
        match call {
            CoreMessageCall::Message(message) => match self.process_resource_message(message).await
            {
                Ok(_) => {}
                Err(err) => {
                    error!("{}", err);
                }
            },
        }
    }
}

impl MessagingEndpointComponent {

    async fn handle_request(&mut self, delivery: Delivery<Request>)
    {
        async fn get_bind_config( end: &mut MessagingEndpointComponent, address: Address ) -> Result<ArtifactItem<CachedConfig<BindConfig>>,Error> {
println!("GETTING BIND ADDRESS for '{}'", address.to_string() );
            let get = ReqEntity::Rc( Rc::new(RcCommand::Get( Get{ address:address.clone(), op: GetOp::Properties(vec!["bind".to_string()])})));
            let request = Request::new( get, address.clone(), address.parent().unwrap() );
println!("sending GET property for '{}'", address.to_string() );
            let response = end.skel.messaging_api.exchange(request).await?;
println!("got BIND property for '{}'", address.to_string() );

            if let Payload::Map(map) = response.entity.payload()? {
                if let Payload::Primitive(Primitive::Text(bind_address ))= map.get(&"bind".to_string()  ).ok_or("bind is not set" )?
                {
println!("BIND ADDRESS IS {}", bind_address.to_string() );
                    let bind_address = Address::from_str(bind_address.as_str())?;
                    let mut cache = end.skel.machine.get_proto_artifact_caches_factory().await?.create();
                    let artifact = ArtifactRef::new(bind_address, ArtifactKind::BindConfig);
                    cache.cache(vec![artifact.clone()]).await?;
                    let cache = cache.to_caches().await?;
                    return Ok(cache.bind_configs.get(&artifact.address).ok_or(format!("could not cache bind {}", artifact.address.to_string()).as_str())?);
                }
                else {
                    return Err("unexpected response".into());
                }
            } else {
                return Err("unexpected response".into());
            }
        }

        fn execute( end: &mut MessagingEndpointComponent, config: ArtifactItem<CachedConfig<BindConfig>>, delivery: Delivery<Request> ) -> Result<(),Error> {
            match &delivery.item.entity {
                ReqEntity::Rc(_) => {panic!("rc should be filtered");}
                ReqEntity::Msg(msg) => {
                   let selector = config.msg.find_match(msg)?;
                   let regex = Regex::new(selector.pattern.path_regex.as_str() )?;
                   let exec = PipelineExecutor::new( delivery, end.skel.clone(), end.resource_manager_api.clone(), selector.pipeline, regex );
                   exec.execute();
                   Ok(())
                }
                ReqEntity::Http(http) => {
println!("SELECTING HTTP...");
                    let selector = config.http.find_match(http)?;
println!("Selection made..." );
                    let regex = Regex::new(selector.pattern.path_regex.as_str() )?;
                    let exec = PipelineExecutor::new( delivery, end.skel.clone(), end.resource_manager_api.clone(), selector.pipeline, regex );
println!("Prepping to exec pipeline...");
                    exec.execute();
                    Ok(())
                }
            }
        }

        match get_bind_config(self, delivery.to.clone() ).await {
            Ok(bind_config) => {
println!("GOT BIND !");
                execute(self, bind_config, delivery );
            }
            Err(_) => {
println!("FAILED TO GET BIND");
                delivery.fail(fail::Fail::Error("resource has no bind config".into()));
            }
        }

    }

    async fn process_resource_message(&mut self, star_message: StarMessage) -> Result<(), Error> {
        match &star_message.payload {
            StarMessagePayload::Request(request) => match &request.entity {
                ReqEntity::Rc(rc) => {
                    let delivery = Delivery::new(request.clone(), star_message, self.skel.clone());
                    self.process_resource_command(delivery).await;
                }
                _ => {
                    let delivery = Delivery::new(request.clone(), star_message, self.skel.clone());
                    self.handle_request(delivery).await;
                }
            },

            StarMessagePayload::ResourceHost(action) => {
                match action {
                    ResourceHostAction::Assign(assign) => {
                        self.resource_manager_api.assign(assign.clone()).await;
                        let reply = star_message.ok(Reply::Empty);
                        self.skel.messaging_api.star_notify(reply);
                    }
                    ResourceHostAction::Init(_) => {}
                    ResourceHostAction::GetState(address) => {
                        match self.resource_manager_api.get(address.clone()).await {
                            Ok(payload) => {
                                let reply = star_message.ok(Reply::Payload(payload));
                                self.skel.messaging_api.star_notify(reply);
                            }
                            Err(err) => {
                                let reply = star_message.fail(Fail::Starlane(StarlaneFailure::Error("could not get state".to_string())));
                                self.skel.messaging_api.star_notify(reply);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }

    async fn process_resource_command(&mut self, delivery: Delivery<Request>)  {
        let skel = self.skel.clone();
        let resource_manager_api = self.resource_manager_api.clone();
        tokio::spawn(async move {
            async fn process(skel: StarSkel, resource_manager_api: ResourceManagerApi, rc: &Rc, to: Address) -> Result<Payload, Error> {
                match &rc.command {
                    RcCommand::Create(create) => {
                        let kind = match_kind(&create.template.kind)?;
                        let stub = match &create.template.address.child_segment_template {
                            AddressSegmentTemplate::Exact(child_segment) => {

                                let address = create.template.address.parent.push(child_segment.clone());
                                match &address {
                                    Ok(_) => {}
                                    Err(err) => {
                                        eprintln!("RC CREATE error: {}", err.to_string());
                                    }
                                }
                                let address = address?;
                                let registration = Registration {
                                    address: address.clone(),
                                    kind: kind.clone(),
                                    registry: create.registry.clone(),
                                    properties: create.properties.clone(),
                                };

                                let mut result = skel.registry_api.register(registration).await;

                                // if strategy is ensure then a dupe is GOOD!
                                if create.strategy == Strategy::Ensure {
                                    if let Err(RegError::Dupe) = result {
                                        result = Ok(skel.resource_locator_api.locate(address).await?.stub);
                                    }
                                }

                                result?
                            }
                            AddressSegmentTemplate::Pattern(pattern) => {
                                if !pattern.contains("%") {
                                    return Err("AddressSegmentTemplate::Pattern must have at least one '%' char for substitution".into());
                                }
                                loop {
                                    let index = skel.registry_api.sequence(create.template.address.parent.clone()).await?;
                                    let child_segment = pattern.replace( "%", index.to_string().as_str() );
                                    let address = create.template.address.parent.push(child_segment.clone())?;
                                    let registration = Registration {
                                        address: address.clone(),
                                        kind: kind.clone(),
                                        registry: create.registry.clone(),
                                        properties: create.properties.clone(),
                                    };

                                    match skel.registry_api.register(registration).await {
                                        Ok(stub) => {
                                            if let Strategy::HostedBy(key) = &create.strategy {
                                                let key = StarKey::from_str( key.as_str() )?;
//                                                let location = ResourceLocation::new(key);
                                                skel.registry_api.assign(address, key).await?;
                                                return Ok(Payload::Primitive(Primitive::Stub(stub)));
                                            } else {
                                                break stub;
                                            }
                                        },
                                        Err(RegError::Dupe) => {
                                            // continue loop
                                        }
                                        Err(RegError::Error(error)) => {
                                            return Err(error);
                                        }
                                    }
                                }
                            }
                        };



                        async fn assign(
                            skel: StarSkel,
                            stub: ResourceStub,
                            state: StateSrc,
                        ) -> Result<(), Error> {

                            let star_kind = StarKind::hosts(&ResourceType::from_str(stub.kind.resource_type().as_str())?);
                            let key = if skel.info.kind == star_kind {
                                skel.info.key.clone()
                            }
                            else {
                                let mut star_selector = StarSelector::new();
                                star_selector.add(StarFieldSelection::Kind(star_kind.clone()));
                                let wrangle = skel.star_wrangler_api.next(star_selector).await?;
                                wrangle.key
                            };
                            skel.registry_api.assign(stub.address.clone(), key.clone()).await?;

                            let mut proto = ProtoStarMessage::new();
                            proto.to(ProtoStarMessageTo::Star(key.clone()));
                            let assign = ResourceAssign::new(AssignKind::Create, stub.clone(), state);
                            proto.payload = StarMessagePayload::ResourceHost(
                                ResourceHostAction::Assign(assign),
                            );
                            skel.messaging_api
                                .star_exchange(proto, ReplyKind::Empty, "assign resource to host")
                                .await?;
                            Ok(())
                        }

                        match assign(skel.clone(), stub.clone(), create.state.clone()).await {
                            Ok(_) => {
                                Ok(Payload::Primitive(Primitive::Stub(stub)))
                            },
                            Err(fail) => {
                                eprintln!("{}",fail.to_string() );
                                skel.registry_api
                                    .set_status(
                                        to,
                                        Status::Panic(
                                            "could not assign resource to host".to_string(),
                                        ),
                                    )
                                    .await;
                                Err(fail.into())
                            }
                        }
                    }
                    RcCommand::Select(select) => {
                        let list = Payload::List( skel.registry_api.select(select.clone()).await? );
                        Ok(list)
                    },
                    RcCommand::Update(_) => {
                        unimplemented!()
                    }
                    RcCommand::Query(query) => {
                        let result = Payload::Primitive(Primitive::Text(
                        skel.registry_api
                            .query(to, query.clone())
                            .await?
                            .to_string(),
                         ));
                        Ok(result)
                    },
                    RcCommand::Get(get) => {
println!("RC GET...");
                        match &get.op {
                            GetOp::State => {
                                let mut proto = ProtoStarMessage::new();
                                proto.to(ProtoStarMessageTo::Resource(get.address.clone()));
                                proto.payload = StarMessagePayload::ResourceHost(ResourceHostAction::GetState(get.address.clone()));
                                if let Reply::Payload(payload) = skel.messaging_api
                                    .star_exchange(proto, ReplyKind::Payload, "get state from manager")
                                    .await? {
                                    Ok(payload)
                                } else {
                                    Err("could not get state".into())
                                }
                            }
                            GetOp::Properties(keys) => {
println!("GET properties");
                                let properties = skel.registry_api.get_properties(get.address.clone(), keys.clone() ).await?;
                                let mut map = PayloadMap::new();
                                for (index,property) in properties.iter().enumerate() {

println!("adding property {} value {}", property.0, property.1);

                                    map.insert( property.0.clone(), Payload::Primitive(Primitive::Text(property.1.clone())));
                                }

                                Ok(Payload::Map(map))
                            }
                        }
                    }
                    RcCommand::Set(set) => {
                        let set = set.clone();
                        skel.registry_api.set_properties(set.address, set.properties).await?;
                        Ok(Payload::Empty)
                    }

                }
            }
            let rc = match &delivery.item.entity {
                ReqEntity::Rc(rc) => {rc}
                _ => { panic!("should not get requests that are not Rc") }
            };
            let result = process(skel,resource_manager_api.clone(), rc, delivery.to().expect("expected this to work since we have already established that the item is a Request")).await.into();

            delivery.result(result);
        });
    }

    pub async fn has_resource(&self, key: &Address) -> Result<bool, Error> {
        Ok(self.resource_manager_api.has( key.clone() ).await?)
    }
}
pub fn match_kind(template: &KindTemplate) -> Result<Kind, Error> {
    let resource_type: ResourceType = ResourceType::from_str(template.resource_type.as_str())?;
    Ok(match resource_type {
        ResourceType::Root => Kind::Root,
        ResourceType::Space => Kind::Space,
        ResourceType::Base => {
            match &template.kind {
                None => {
                    return Err("kind must be set for Base".into());
                }
                Some(kind) => {
                    let kind = BaseKind::from_str(kind.as_str())?;
                    if template.specific.is_some() {
                        return Err("BaseKind cannot have a Specific".into());
                    }
                    return Ok(Kind::Base(kind));
                }
            }
        },
        ResourceType::User => Kind::User,
        ResourceType::App => Kind::App,
        ResourceType::Mechtron => Kind::Mechtron,
        ResourceType::FileSystem => Kind::FileSystem,
        ResourceType::File => {
            match &template.kind{
                None => {
                    return Err("expected kind for File".into())
                }
                Some(kind) => {
                    let file_kind = FileKind::from_str(kind.as_str())?;
                    return Ok(Kind::File(file_kind));
                }
            }
        }
        ResourceType::Database => {
            unimplemented!("need to write a SpecificPattern matcher...")
        }
        ResourceType::Authenticator => Kind::Authenticator,
        ResourceType::ArtifactBundleSeries => Kind::ArtifactBundleSeries,
        ResourceType::ArtifactBundle => Kind::ArtifactBundle,
        ResourceType::Artifact => {
            match &template.kind {
                None => {
                    return Err("expected kind for Artirtact".into());
                }
                Some(kind) => {
                    let artifact_kind = ArtifactKind::from_str(kind.as_str())?;
                    return Ok(Kind::Artifact(artifact_kind));
                }
            }
        }
        ResourceType::Proxy => Kind::Proxy,
        ResourceType::Credentials => Kind::Credentials,
        ResourceType::Control => Kind::Control
    })
}
pub struct WrappedHttpRequest {
    pub resource: Address,
    pub request: HttpRequest,
}

pub struct PipelineExecutor {
    pub delivery: Delivery<Request>,
    pub skel: StarSkel,
    pub resource_manager_api: ResourceManagerApi,
    pub pipeline: Pipeline,
    pub path_regex: Regex
}

impl  PipelineExecutor {
  pub fn new( delivery: Delivery<Request>, skel: StarSkel, resource_manager_api: ResourceManagerApi, pipeline: Pipeline, path_regex: Regex ) -> Self {
      Self {
          delivery,
          skel,
          resource_manager_api,
          pipeline,
          path_regex
      }
  }
}

impl PipelineExecutor {

    pub fn execute( mut self ) {
       tokio::spawn( async move {
           async fn process( exec: &mut PipelineExecutor) -> Result<RespEntity,Error> {
               let mut message = Message::Request( exec.delivery.item.clone() );
               while let Option::Some(segment) = exec.pipeline.consume() {
                   message = exec.execute_step(message, &segment.step )?;
                   message = exec.execute_stop(message, &segment.stop ).await?;
               }

               match message {
                   Message::Request(_) => {
                       Err("Expecting a Response entity at the end of this pipeline".into() )
                   }
                   Message::Response(response) => {
                       Ok(response.entity)
                   }
               }
           }
           match process(&mut self ).await {
               Ok(entity) => {
                   match entity.payload() {
                       Ok(payload) => {
                           self.delivery.ok(payload);
                       }
                       Err(err) => {
                           self.delivery.fail(fail::Fail::Error(err.to_string()));
                       }
                   }
               }
               Err(error) => {
                   self.delivery.fail(fail::Fail::Error(error.to_string()));
               }
           }
       });
    }

    async fn execute_stop( &self, mut message: Message, stop: &PipelineStop ) -> Result<Message,Error> {
       match stop {
           PipelineStop::Internal => {
               self.resource_manager_api.request(self.delivery.clone()).await;
           }
           PipelineStop::Call(call) => {
               match &call.kind {
                   CallKind::Rc(_) => {}
                   CallKind::Msg(msg) => {
                       let msg = Msg {
                           action: msg.action.clone(),
                           path: msg.path.clone(),
                           payload: message.payload()?
                       };
                       let entity = ReqEntity::Msg(msg);
                       let request = Request::new( entity, self.delivery.to.clone(), call.address.clone() );
                       let response = self.skel.messaging_api.exchange(request).await?;
                       message = Message::Response(response);
                   }
                   CallKind::Http(http) => {
                       let http= HttpRequest{
                           headers: Meta::new(),
                           method: http.method.clone(),
                           path: http.path.clone(),
                           body: message.payload()?
                       };
                       let entity = ReqEntity::Http(http);
                       let request = Request::new( entity, self.delivery.to.clone(), call.address.clone() );
                       let response = self.skel.messaging_api.exchange(request).await?;
                       message = Message::Response(response);
                   }
               }
           }
           PipelineStop::Return => {

           }
           PipelineStop::CaptureAddress(address) => {
               let path = self.delivery.item.entity.path();
               let captures = self.path_regex.captures( path.as_str() ).ok_or("cannot find regex captures" )?;
               let address = address.clone().to_address(captures)?;
println!("NEW ADDRESS: {}",address.to_string() );
               let get = ReqEntity::Rc( Rc::new(RcCommand::Get( Get{ address:address.clone(), op: GetOp::State})));
               let request = Request::new( get, self.delivery.item.to.clone(), address.clone() );
               let response = self.skel.messaging_api.exchange(request).await?;
               message = Message::Response(response);
           }
       }
       Ok(message)
    }


    fn execute_step( &self, mut message: Message, step: &PipelineStep ) -> Result<Message,Error> {
        match &step.kind {
            StepKind::Request => {
                for block in &step.blocks {
                    message = self.execute_block(message, block)?;
                }
            }
            StepKind::Response => {}
        }
        Ok(message)
    }

    fn execute_block( &self, mut message: Message, block: &Block ) -> Result<Message,Error> {
        match block {
            Block::Upload(_) => {
                return Err("upload block can only be used on the command line".into());
            }
            Block::RequestPattern(pattern) => {
                if let Message::Request(request) = &message{
                    let payload = request.entity.payload();
                    pattern.is_match(payload)?;
                } else {
                    return Err("expected Request".into())
                }
            }
            Block::ResponsePattern(pattern) => {
                if let Message::Response(response) = &message{
                    let payload = response.entity.payload()?;
                    pattern.is_match(&payload)?;
                } else {
                    return Err("expected Response".into());
                }
            }
            Block::CreatePayload(payload) => {
                message = match message {
                    Message::Request(mut request) => {
                        request.entity = request.entity.with_new_payload(payload.clone());
                        Message::Request(request)
                    }
                    Message::Response(mut response) => {
                        response.entity = response.entity.with_new_payload(payload.clone());
                        Message::Response(response)
                    }
                };
            }
        }
        Ok(message)
    }
}
