use core::cell::Cell;
use core::option::Option;
use core::result::Result;
use core::result::Result::{Err, Ok};
use std::collections::HashSet;
use std::iter::FromIterator;

use serde::{Deserialize, Serialize};
use tokio::sync::oneshot;
use tokio::time::Duration;

use crate::error::Error;
use crate::message::ProtoStarMessage;
use crate::resource::{ResourceRecord, ResourceType, Kind};
use crate::star::{StarCommand, StarSkel};
use crate::util;
use crate::fail::Fail;
use crate::frame::{StarMessage, StarMessagePayload, SimpleReply};
use mesh_portal::version::latest::util::unique_id;
use crate::message::Reply;
use std::ops::Deref;
use mesh_portal::version::latest::entity::response::ResponseCore;
use mesh_portal::version::latest::id::Address;
use mesh_portal::version::latest::messaging::{Request, Response};
use mesh_portal::version::latest::payload::{Payload, Primitive};

pub struct Delivery<M>
where
    M: Clone,
{
    skel: StarSkel,
    star_message: StarMessage,
    pub item: M,
}

impl<M> Delivery<M>
where
    M: Clone + Send + Sync + 'static,
{
    pub fn new(item: M, star_message: StarMessage, skel: StarSkel) -> Self {
        Delivery {
            item,
            star_message: star_message,
            skel: skel,
        }
    }

    pub fn to(&self) -> Result<Address,Error> {
        match &self.star_message.payload {
            StarMessagePayload::Request(request) => {
                Ok(request.to.clone())
            }
            _ => {
                Err("this type of Delivery does not support to() resolution".into())
            }
        }
    }
}
impl<M> Deref for Delivery<M> where
    M: Clone + Send + Sync + 'static{
    type Target = M;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

impl<M> Into<StarMessage> for Delivery<M> where
    M: Clone + Send + Sync + 'static{
    fn into(self) -> StarMessage {
        self.star_message
    }
}
impl <M> Delivery<M> where M: Clone + Send + Sync + 'static
{
  pub fn get_request(&self) -> Result<Request,Error> {
      match &self.star_message.payload {
          StarMessagePayload::Request(request) => {
              Ok(request.clone())
          }
          _ => {
              Err("not a request delivery".into())
          }
      }
  }
}

impl Delivery<Request>
{
    pub fn result( self, result: Result<Payload,Error>) {
        match result {
            Ok(payload) => {
                let request = self.item.core.clone();
                self.respond(request.ok(payload));
            }
            Err(err) => {
                self.fail(err.to_string());
            }
        }
    }

    pub fn respond(self, core: ResponseCore ) {
        let response = Response {
            id: unique_id(),
            to: self.item.from,
            from: self.item.to,
            core,
            response_to: self.item.id
        };
        let proto = self
            .star_message
            .reply(StarMessagePayload::Response(response));

        self.skel.messaging_api.star_notify(proto);
    }

   pub fn ok(self, payload: Payload) -> Result<(),Error> {
        let request = self.get_request()? ;
        let core = ResponseCore {
            headers: Default::default(),
            code: 200,
            body: payload
        };
        let response = Response {
            id: unique_id(),
            to: request.from.clone(),
            from: request.to.clone(),
            core,
            response_to: self.item.id
        };

        let proto = self
            .star_message
            .reply(StarMessagePayload::Response(response));

        self.skel.messaging_api.star_notify(proto);
        Ok(())
    }

    pub fn fail(self, fail: String ) ->Result<(),Error> {

        let request = self.get_request()?;
        let core = ResponseCore {
            headers: Default::default(),
            code: 500,
            body: Payload::Primitive(Primitive::Text(fail))
        };
        let response = Response {
            id: unique_id(),
            to: request.from,
            from: request.to,
            core,
            response_to: request.id
        };

        let proto = self
            .star_message
            .reply(StarMessagePayload::Response(response));
        self.skel.messaging_api.star_notify(proto);
        Ok(())
    }
}


/*impl<M> Delivery<M>
where
    M: Clone + Send + Sync + 'static,
{

    pub fn result(&self, result: Result<Reply, Fail>) {
        match result {
            Ok(reply) => {
                self.reply(reply);
            }
            Err(fail) => {
                self.fail(fail);
            }
        }
    }

    pub fn result_ok<T>(&self, result: Result<T, Error>) {
        match result {
            Ok(_) => {
                self.reply(Reply::Empty);
            }
            Err(fail) => {
                self.fail(fail.into());
            }
        }
    }

    pub fn result_rx<T>(self, mut rx: oneshot::Receiver<Result<T, Error>>)
    where
        T: Send + Sync + 'static,
    {
        tokio::spawn(async move {
            match tokio::time::timeout(Duration::from_secs(15), rx).await {
                Ok(Ok(Ok(_))) => {
                    self.reply(Reply::Empty);
                }
                Ok(Ok(Err(fail))) => {
                    self.fail(fail.into());
                }
                Ok(Err(_)) => {
                    self.fail(Fail::Timeout);
                }
                Err(_) => {
                    self.fail(Fail::ChannelRecvErr);
                }
            }
        });
    }

    pub fn ok(&self) {
        self.reply(Reply::Empty);
    }

    pub fn reply(&self, reply: Reply) {
        let proto = self
            .star_message
            .reply(StarMessagePayload::Reply(SimpleReply::Ok(reply)));
        self.skel.messaging_api.send(proto);
    }

    pub fn fail(&self, fail: Fail) {
        let proto = self
            .star_message
            .reply(StarMessagePayload::Reply(SimpleReply::Fail(fail)));
        self.skel.messaging_api.send(proto);
    }
}

 */

/*
#[derive(Clone, Serialize, Deserialize)]
pub enum Reply {
    Empty,
    Key(ResourceKey),
    Address(ResourceAddress),
    Records(Vec<ResourceRecord>),
    Record(ResourceRecord),
    Message(MessageReply<ResourceResponseMessage>),
    Id(ResourceId),
    Seq(u64),
}
 */

#[derive(Clone, Serialize, Deserialize)]
pub enum ActorMessage {}

pub struct DeliverySelector{
    selections: Vec<DeliverySelection>
}

pub enum DeliverySelection{
 Any
}

impl DeliverySelector {
    pub fn any() ->Self {
        Self {
            selections: vec![DeliverySelection::Any]
        }
    }
}





