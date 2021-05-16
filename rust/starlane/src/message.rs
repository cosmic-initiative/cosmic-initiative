use crate::star::{StarKey, StarSearchTransaction, Transaction, TransactionResult, StarCommand};
use crate::frame::{StarMessagePayload, Frame, StarMessage, MessageAck, SimpleReply};
use crate::error::Error;
use crate::lane::LaneMeta;
use std::cell::Cell;
use crate::id::Id;
use tokio::sync::{mpsc, oneshot, broadcast};
use tokio::sync::oneshot::Receiver;
use tokio::sync::mpsc::Sender;
use crate::keys::{MessageId, SubSpaceKey, UserKey, ResourceKey};
use tokio::sync::broadcast::error::RecvError;
use serde::{Serialize, Deserialize};
use crate::names::Specific;

pub struct ProtoMessage
{
    pub to: Option<StarKey>,
    pub payload: StarMessagePayload,
    pub tx: broadcast::Sender<MessageUpdate>,
    pub rx: broadcast::Receiver<MessageUpdate>,
    pub expect: MessageExpect,
    pub reply_to: Option<MessageId>
}

impl ProtoMessage
{

    pub fn new()->Self
    {
        let (tx,rx) = broadcast::channel(8);
        ProtoMessage::with_txrx(tx,rx)
    }

    pub fn with_txrx( tx: broadcast::Sender<MessageUpdate>, rx: broadcast::Receiver<MessageUpdate> )->Self
    {
        ProtoMessage{
            to: Option::None,
            payload: StarMessagePayload::None,
            tx: tx,
            rx: rx,
            expect: MessageExpect::None,
            reply_to: Option::None
        }
    }

    pub fn to( &mut self, to: StarKey)
    {
        self.to = Option::Some(to);
    }

    pub fn reply_to( &mut self, reply_to: MessageId )
    {
        self.reply_to = Option::Some(reply_to);
    }


    pub fn validate(&self)->Result<(),Error>
    {
        let mut errors = vec!();
        if self.to.is_none()
        {
            errors.push("must specify 'to' field");
        }
        if let StarMessagePayload::None = self.payload
        {
            errors.push("must specify a message payload");
        }

        if !errors.is_empty()
        {
            let mut rtn = String::new();
            for err in errors
            {
                rtn.push_str(err);
                rtn.push('\n');
            }
            return Err(rtn.into());
        }

        return Ok(());
    }

    pub async fn get_ok_result(&self) -> oneshot::Receiver<StarMessagePayload>
    {
        let (waiter, rx) = OkResultWaiter::new(self.tx.subscribe() );
        waiter.wait().await;
        rx
    }



}


pub struct MessageReplyTracker
{
    pub reply_to: MessageId,
    pub tx: broadcast::Sender<MessageUpdate>
}

impl MessageReplyTracker
{
    pub fn on_message(&self, message: &StarMessage ) -> TrackerJob
    {
        match &message.payload {
            StarMessagePayload::Reply(reply) => {
                match reply
                {
                    SimpleReply::Ok(reply) => {
                        self.tx.send(MessageUpdate::Result(MessageResult::Ok(message.payload.clone())));
                        TrackerJob::Done
                    }
                    SimpleReply::Fail(fail) => {
                        self.tx.send(MessageUpdate::Result(MessageResult::Err("fail".to_string())));
                        TrackerJob::Done
                    }
                    SimpleReply::Ack(ack) => {
                        self.tx.send( MessageUpdate::Ack(ack.clone()) );
                        TrackerJob::Continue
                    }
                }
            }
            _ => {
                TrackerJob::Continue
            }
       }
    }
}

pub enum TrackerJob
{
    Continue,
    Done
}

#[derive(Clone)]
pub enum MessageUpdate
{
    Ack(MessageAck),
    Result(MessageResult<StarMessagePayload>)
}

#[derive(Clone)]
pub enum MessageResult<OK>
{
    Ok(OK),
    Err(String),
    Timeout
}

pub struct StarMessageDeliveryInsurance
{
    pub message: StarMessage,
    pub expect: MessageExpect,
    pub retries: usize,
    pub tx: broadcast::Sender<MessageUpdate>,
    pub rx: broadcast::Receiver<MessageUpdate>
}

impl StarMessageDeliveryInsurance
{
    pub fn new( message: StarMessage, expect: MessageExpect) -> Self
    {
        let (tx,rx) = broadcast::channel(8);
        StarMessageDeliveryInsurance::with_txrx(message,expect,tx,rx)
    }


    pub fn with_txrx( message: StarMessage, expect: MessageExpect, tx: broadcast::Sender<MessageUpdate>, rx:broadcast::Receiver<MessageUpdate> ) -> Self
    {
        StarMessageDeliveryInsurance {
            message: message,
            retries: expect.retries(),
            expect: expect,
            tx: tx,
            rx: rx
        }
    }
}

#[derive(Clone)]
pub enum MessageExpect
{
    None,
    ReplyErrOrTimeout(MessageExpectWait),
    RetryUntilOk
}

impl MessageExpect {
    pub(crate) fn wait_seconds(&self) -> u64 {
        match self
        {
            MessageExpect::None => {
                30
            }
            MessageExpect::ReplyErrOrTimeout(wait) => {
                wait.wait_seconds()
            }
            MessageExpect::RetryUntilOk => {
                5
            }
        }
    }

    pub fn retries(&self) -> usize {
        match self
        {
            MessageExpect::None => {1}
            MessageExpect::ReplyErrOrTimeout(wait) => {
                wait.retries()
            }
            MessageExpect::RetryUntilOk => {
                10
            }
        }
    }

    pub fn retry_forever(&self) -> bool
    {
        match self
        {
            MessageExpect::RetryUntilOk => {
                true
            }
            _ => {
                false
            }
        }
    }
}

#[derive(Clone)]
pub enum MessageExpectWait
{
    Short,
    Med,
    Long
}

impl MessageExpectWait {
    pub fn wait_seconds(&self) -> u64 {
        match self
        {
            MessageExpectWait::Short => {5}
            MessageExpectWait::Med => {10}
            MessageExpectWait::Long => {30}
        }
    }

    pub fn retries(&self) -> usize {
        match self
        {
            MessageExpectWait::Short => {5}
            MessageExpectWait::Med => {10}
            MessageExpectWait::Long => {15}
        }
    }
}

pub struct OkResultWaiter
{
    rx: broadcast::Receiver<MessageUpdate>,
    tx: oneshot::Sender<StarMessagePayload>
}

impl OkResultWaiter
{
    pub fn new( rx: broadcast::Receiver<MessageUpdate> )->(Self,oneshot::Receiver<StarMessagePayload>)
    {
        let (tx,osrx) = oneshot::channel();
        (OkResultWaiter{
            rx: rx,
            tx: tx
        },osrx)
    }

    pub async fn wait( mut self )
    {
        tokio::spawn( async move {
        loop{
            if let Ok(MessageUpdate::Result(result)) = self.rx.recv().await
            {
                match result
                {
                    MessageResult::Ok(payload) => {
                        self.tx.send(payload);
                    }
                    _ => {
                        eprintln!("not expecting this results for OkResultWaiter...");
                        self.tx.send(StarMessagePayload::None);
                    }
                }
                break;
            }
        }});
    }
}

pub struct ResultWaiter
{
    rx: broadcast::Receiver<MessageUpdate>,
    tx: oneshot::Sender<MessageResult<StarMessagePayload>>
}

impl ResultWaiter
{
    pub fn new( rx: broadcast::Receiver<MessageUpdate> )->(Self,oneshot::Receiver<MessageResult<StarMessagePayload>>)
    {
        let (tx,osrx) = oneshot::channel();
        (ResultWaiter{
            rx: rx,
            tx: tx
        },osrx)
    }

    pub async fn wait( mut self )
    {
        tokio::spawn( async move {
            loop{
                if let Ok(MessageUpdate::Result(result)) = self.rx.recv().await
                {
                   self.tx.send(result);
                   break;
                }
            }});
    }
}


#[derive(Clone,Serialize,Deserialize)]
pub enum Fail
{
    Timeout,
    Error(String),
    Reject(Reject),
    Unexpected,
    DoNotKnowSpecific(Specific),
    ResourceNotFound(ResourceKey),
    WrongResourceType,
    RecvErr
}



#[derive(Clone,Serialize,Deserialize)]
pub struct Reject
{
    pub reason: String,
    pub kind: RejectKind
}

#[derive(Clone,Serialize,Deserialize)]
pub enum RejectKind
{
    Error,
    Denied,
    BadRequest
}



impl From<tokio::sync::oneshot::error::RecvError> for Fail
{
    fn from(_: tokio::sync::oneshot::error::RecvError) -> Self {
        Fail::Timeout
    }
}

impl <T> From<tokio::sync::mpsc::error::SendError<T>> for Fail
{
    fn from(_: tokio::sync::mpsc::error::SendError<T>) -> Self {
        Fail::Unexpected
    }

}

