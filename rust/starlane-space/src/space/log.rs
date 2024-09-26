use core::str::FromStr;
use std::collections::HashMap;
use std::sync::Arc;

use crate::Agent;
use regex::Regex;
use serde;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::space::err::SpaceErr;
use crate::space::loc::{Layer, ToPoint, ToSurface, Uuid};
use crate::space::parse::CamelCase;
use crate::space::point::Point;
use crate::space::selector::Selector;
use crate::space::substance::LogSubstance;
use crate::space::util::{timestamp, uuid};
use crate::space::wasm::Timestamp;
use crate::space::wave::core::cmd::CmdMethod;
use crate::space::wave::exchange::synch::{ProtoTransmitter, ProtoTransmitterBuilder};
use crate::space::wave::exchange::SetStrategy;
use crate::space::wave::{
    DirectedProto, Handling, HandlingKind, Priority, Retries, ToRecipients, WaitTime,
};

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
pub enum Level {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl Default for Level {
    fn default() -> Self {
        Level::Info
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Log {
    pub point: Point,
    pub mark: Point,
    pub action: Option<CamelCase>,
    pub source: LogSource,
    pub span: Option<Uuid>,
    pub timestamp: i64,
    pub payload: LogPayload,
    pub level: Level,
}

impl ToString for Log {
    fn to_string(&self) -> String {
        format!(
            "{} {} {}",
            self.point.to_string(),
            self.level.to_string(),
            self.payload.to_string()
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
pub enum LogSource {
    Shell,
    Core,
}
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum LogSpanEventKind {
    Entry,
    Exit,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct LogSpanEvent {
    pub point: Point,
    pub span: Uuid,
    pub kind: LogSpanEventKind,
    pub attributes: HashMap<String, String>,
    pub timestamp: Timestamp,
}

impl LogSpanEvent {
    pub fn new(
        span: &LogSpan,
        point: &Point,
        kind: LogSpanEventKind,
        attributes: HashMap<String, String>,
    ) -> LogSpanEvent {
        LogSpanEvent {
            span: span.id.clone(),
            point: point.clone(),
            kind,
            attributes,
            timestamp: timestamp(),
        }
    }
}

pub type TrailSpanId = Uuid;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct LogSpan {
    pub id: TrailSpanId,
    pub point: Point,
    pub mark: Point,
    pub action: Option<CamelCase>,
    pub parent: Option<Uuid>,
    pub attributes: HashMap<String, String>,
    pub entry_timestamp: Timestamp,
}

impl LogSpan {
    pub fn new(point: Point) -> Self {
        Self {
            id: uuid(),
            point,
            mark: Point::root(),
            action: None,
            parent: None,
            attributes: Default::default(),
            entry_timestamp: timestamp(),
        }
    }

    pub fn parent(point: Point, parent: Uuid) -> Self {
        Self {
            id: uuid(),
            point,
            mark: Point::root(),
            action: None,
            parent: Some(parent),
            attributes: Default::default(),
            entry_timestamp: timestamp(),
        }
    }

    pub fn opt(point: Point, span: Option<Self>) -> Self {
        let mut span = span.unwrap_or(Self {
            id: uuid(),
            point: point.clone(),
            mark: Point::root(),
            action: None,
            parent: None,
            attributes: Default::default(),
            entry_timestamp: timestamp(),
        });
        span.point = point;
        span
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct PointlessLog {
    timestamp: Timestamp,
    message: String,
    level: Level,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum LogPayload {
    Message(String),
    Json(Value),
    Both { message: String, json: Value },
}

impl ToString for LogPayload {
    fn to_string(&self) -> String {
        match self {
            LogPayload::Message(message) => message.clone(),
            LogPayload::Json(json) => json.to_string(),
            LogPayload::Both { json, message } => {
                format!("{} {}", json.to_string(), message.clone())
            }
        }
    }
}

pub struct RootLogBuilder {
    pub point: Option<Point>,
    pub span: Option<Uuid>,
    pub logger: RootLogger,
    pub level: Level,
    pub message: Option<String>,
    pub json: Option<Value>,
    msg_overrides: Vec<String>,
}

impl RootLogBuilder {
    pub fn new(logger: RootLogger, span: Option<Uuid>) -> Self {
        RootLogBuilder {
            logger,
            span,
            point: None,
            level: Level::default(),
            message: None,
            json: None,
            msg_overrides: vec![],
        }
    }

    pub fn level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }

    pub fn trace(mut self) -> Self {
        self.level = Level::Trace;
        self
    }
    pub fn debug(mut self) -> Self {
        self.level = Level::Debug;
        self
    }
    pub fn info(mut self) -> Self {
        self.level = Level::Info;
        self
    }
    pub fn warn(mut self) -> Self {
        self.level = Level::Warn;
        self
    }
    pub fn error(mut self) -> Self {
        self.level = Level::Error;
        self
    }

    pub fn point(mut self, p: Point) -> Self {
        self.point = Some(p);
        self
    }

    pub fn msg<M>(mut self, m: M) -> Self
    where
        M: ToString,
    {
        self.message = Some(m.to_string());
        self
    }

    pub fn json<'a, J>(mut self, json: J) -> Self
    where
        J: Into<&'a str>,
    {
        match serde_json::from_str(json.into()) {
            Ok(json) => {
                self.json = Some(json);
            }
            Err(err) => {
                self.msg_overrides
                    .push(format!("error parsing log json: {}", err.to_string()));
            }
        }
        self
    }

    pub fn json_value(mut self, json: Value) -> Self {
        self.json = Some(json);
        self
    }

    pub fn commit(mut self) {
        if self.message.is_none() && self.json.is_none() {
            self.msg_overrides
                .push("Log must have either a message or json or both".to_string())
        }

        if self.point.is_none() {
            self.msg_overrides
                .push("Particle Point must be set for a Log".to_string())
        }

        let message = if self.msg_overrides.is_empty() {
            self.message
        } else {
            let mut rtn = String::new();
            rtn.push_str("LOG ERROR OVERRIDES: this means there was an error int he logging process itself.\n");
            for over in self.msg_overrides {
                rtn.push_str(over.as_str());
            }
            match self.message {
                None => {}
                Some(message) => {
                    rtn.push_str(format!("original message: {}", message).as_str());
                }
            }
            Some(rtn)
        };

        if self.point.is_none() {
            let log = PointlessLog {
                timestamp: timestamp(),
                message: message.expect("message"),
                level: Level::Error,
            };
            self.logger.pointless(log);
            return;
        }

        let content = if message.is_some() && self.json.is_none() {
            LogPayload::Message(message.expect("message"))
        } else if message.is_none() && self.json.is_some() {
            LogPayload::Json(self.json.expect("message"))
        } else if message.is_some() && self.json.is_some() {
            LogPayload::Both {
                message: message.expect("message"),
                json: self.json.expect("json"),
            }
        } else {
            panic!("LogBuilder: must set Logger before LogBuilder.send() can be called")
        };

        let point = self.point.expect("point");
        let log = Log {
            point,
            mark: Point::root(),
            action: None,
            level: self.level,
            timestamp: timestamp().timestamp_millis(),
            payload: content,
            source: self.logger.source(),
            span: self.span,
        };
        self.logger.log(log);
    }
}

pub trait LogAppender: Send + Sync {
    fn log(&self, log: Log);

    fn audit(&self, log: AuditLog);

    fn span_event(&self, log: LogSpanEvent);

    /// PointlessLog is used for error diagnosis of the logging system itself, particularly
    /// where there is parsing error due to a bad point
    fn pointless(&self, log: PointlessLog);
}

#[derive(Clone)]
pub struct RootLogger {
    source: LogSource,
    appender: Arc<dyn LogAppender>,
}

impl Default for RootLogger {
    fn default() -> Self {
        RootLogger::new(LogSource::Core, Arc::new(StdOutAppender::new()))
    }
}

impl RootLogger {
    pub fn new(source: LogSource, appender: Arc<dyn LogAppender>) -> Self {
        Self { source, appender }
    }

    pub fn stdout(source: LogSource) -> Self {
        Self {
            source,
            ..RootLogger::default()
        }
    }

    fn source(&self) -> LogSource {
        self.source.clone()
    }

    fn log(&self, log: Log) {
        self.appender.log(log);
    }

    fn audit(&self, log: AuditLog) {
        self.appender.audit(log);
    }

    fn span_event(&self, log: LogSpanEvent) {
        self.appender.span_event(log);
    }

    /// PointlessLog is used for error diagnosis of the logging system itself, particularly
    /// where there is parsing error due to a bad point
    fn pointless(&self, log: PointlessLog) {
        self.appender.pointless(log);
    }

    pub fn point<P: ToPoint>(&self, point: P) -> PointLogger {
        PointLogger {
            logger: self.clone(),
            point: point.to_point(),
            mark: Point::root(),
            action: None,
        }
    }
}
pub struct NoAppender {}

impl NoAppender {
    pub fn new() -> Self {
        NoAppender {}
    }
}

impl LogAppender for NoAppender {
    fn log(&self, log: Log) {}

    fn audit(&self, log: AuditLog) {}

    fn span_event(&self, log: LogSpanEvent) {}

    fn pointless(&self, log: PointlessLog) {}
}

pub struct StdOutAppender();

impl StdOutAppender {
    pub fn new() -> Self {
        StdOutAppender()
    }
}

impl LogAppender for StdOutAppender {
    fn log(&self, log: Log) {
        let action = match log.action {
            None => "None".to_string(),
            Some(action) => action.to_string(),
        };
        println!("{} | {}", log.point.to_string(), log.payload.to_string())
    }

    fn audit(&self, log: AuditLog) {
        println!("audit log...")
    }

    fn span_event(&self, log: LogSpanEvent) {
        /*         println!(
                   "{} | Span({})",
                   log.point.to_string(),
                   log.span.to_string(),
               )

        */
    }

    fn pointless(&self, log: PointlessLog) {
        println!("{}", log.message);
    }
}

pub struct SynchTransmittingLogAppender {
    transmitter: ProtoTransmitter,
}

impl SynchTransmittingLogAppender {
    pub fn new(mut transmitter: ProtoTransmitterBuilder) -> Self {
        transmitter.method = SetStrategy::Override(CmdMethod::Log.into());
        transmitter.to = SetStrategy::Override(
            Point::global_logger()
                .to_surface()
                .with_layer(Layer::Core)
                .to_recipients(),
        );
        transmitter.handling = SetStrategy::Fill(Handling {
            kind: HandlingKind::Durable,
            priority: Priority::Low,
            retries: Retries::Medium,
            wait: WaitTime::High,
        });
        let transmitter = transmitter.build();
        Self { transmitter }
    }
}

impl LogAppender for SynchTransmittingLogAppender {
    fn log(&self, log: Log) {
        let mut directed = DirectedProto::signal();
        directed.from(log.point.to_surface());
        directed.agent(Agent::Point(log.point.clone()));
        directed.body(LogSubstance::Log(log).into());
        self.transmitter.signal(directed);
    }

    fn audit(&self, log: AuditLog) {
        let mut directed = DirectedProto::signal();
        directed.from(log.point.to_surface());
        directed.agent(Agent::Point(log.point.clone()));
        directed.body(LogSubstance::Audit(log).into());
        self.transmitter.signal(directed);
    }

    fn span_event(&self, log: LogSpanEvent) {
        let mut directed = DirectedProto::signal();
        directed.from(log.point.to_surface());
        directed.agent(Agent::Point(log.point.clone()));
        directed.body(LogSubstance::Event(log).into());
        self.transmitter.signal(directed);
    }

    fn pointless(&self, log: PointlessLog) {
        let mut directed = DirectedProto::signal();
        directed.from(Point::anonymous());
        directed.agent(Agent::Anonymous);
        directed.body(LogSubstance::Pointless(log).into());
        self.transmitter.signal(directed);
    }
}

#[derive(Clone)]
pub struct PointLogger {
    pub logger: RootLogger,
    pub point: Point,
    pub mark: Point,
    pub action: Option<CamelCase>,
}

impl Default for PointLogger {
    fn default() -> Self {
        Self {
            logger: Default::default(),
            point: Point::root(),
            mark: Point::root(),
            action: None,
        }
    }
}

impl PointLogger {
    pub fn source(&self) -> LogSource {
        self.logger.source()
    }

    pub fn opt_span(&self, span: Option<LogSpan>) -> SpanLogger {
        let new = span.is_none();
        let span = LogSpan::opt(self.point.clone(), span);
        let logger = SpanLogger {
            root_logger: self.logger.clone(),
            span: span.clone(),
            commit_on_drop: true,
        };

        if new {
            self.logger.span_event(LogSpanEvent::new(
                &span,
                &self.point,
                LogSpanEventKind::Entry,
                Default::default(),
            ));
        }

        logger
    }

    pub fn for_span_async(&self, span: LogSpan) -> SpanLogger {
        let mut span = self.for_span(span);
        span.commit_on_drop = false;
        span
    }

    pub fn for_span(&self, span: LogSpan) -> SpanLogger {
        let mut span = SpanLogger {
            root_logger: self.logger.clone(),
            span,
            commit_on_drop: true,
        };
        span
    }

    pub fn span(&self) -> SpanLogger {
        let span = LogSpan::new(self.point.clone());
        let logger = SpanLogger {
            root_logger: self.logger.clone(),
            span: span.clone(),
            commit_on_drop: true,
        };

        self.logger.span_event(LogSpanEvent::new(
            &span,
            &self.point,
            LogSpanEventKind::Entry,
            Default::default(),
        ));

        logger
    }

    pub fn span_async(&self) -> SpanLogger {
        let mut span = self.span();
        span.commit_on_drop = false;
        span
    }

    pub fn spanner<S>(&self, spannable: &S) -> SpanLogger
    where
        S: Spannable,
    {
        let logger = self.span();
        let mut attrs = HashMap::new();
        attrs.insert("type".to_string(), spannable.span_type().to_string());
        attrs.insert("id".to_string(), spannable.span_id().to_string());
        logger.span_attr(attrs);
        logger
    }

    pub fn point(&self, point: Point) -> PointLogger {
        PointLogger {
            logger: self.logger.clone(),
            point,
            mark: Point::root(),
            action: None,
        }
    }

    pub fn push_point<S: ToString>(&self, segs: S) -> anyhow::Result<PointLogger> {
        Ok(PointLogger {
            logger: self.logger.clone(),
            point: self.point.push(segs)?,
            mark: Point::root(),
            action: None,
        })
    }

    pub fn pop_mark(&self) -> PointLogger {
        PointLogger {
            logger: self.logger.clone(),
            point: self.point.clone(),
            mark: self.mark.pop(),
            action: self.action.clone(),
        }
    }

    pub fn push_mark<S: ToString>(&self, segs: S) -> anyhow::Result<PointLogger> {
        Ok(PointLogger {
            logger: self.logger.clone(),
            point: self.point.clone(),
            mark: self.mark.push(segs)?,
            action: None,
        })
    }

    pub fn push_action<A: ToString>(&self, action: A) -> anyhow::Result<PointLogger> {
        Ok(PointLogger {
            logger: self.logger.clone(),
            point: self.point.clone(),
            mark: self.mark.clone(),
            action: Some(CamelCase::from_str(action.to_string().as_str())?),
        })
    }

    pub fn msg<M>(&self, level: Level, message: M)
    where
        M: ToString,
    {
        self.logger.log(Log {
            point: self.point.clone(),
            mark: self.mark.clone(),
            action: self.action.clone(),
            level,
            timestamp: timestamp().timestamp_millis(),
            payload: LogPayload::Message(message.to_string()),
            span: None,
            source: self.logger.source(),
        })
    }

    pub fn handle(&self, log: LogSubstance) {
        match log {
            LogSubstance::Log(log) => {
                self.logger.log(log);
            }
            LogSubstance::Span(span) => {
                println!("start log span...");
                // not sure how to handle this
            }
            LogSubstance::Event(event) => {
                self.logger.span_event(event);
            }
            LogSubstance::Audit(audit) => {
                self.logger.audit(audit);
            }
            LogSubstance::Pointless(pointless) => {
                self.logger.pointless(pointless);
            }
        }
    }

    pub fn trace<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn debug<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn info<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn warn<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Warn, message);
    }

    pub fn error<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Error, message);
    }

    pub fn result<R, E>(&self, result: Result<R, E>) -> Result<R, E>
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                //self.error(err.to_string());
                self.msg(Level::Error, err.to_string());
            }
        }
        result
    }

    pub fn eat<R, E>(&self, result: Result<R, E>)
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                self.error(err.to_string());
            }
        }
    }

    pub fn result_ctx<R, E>(&self, ctx: &str, result: Result<R, E>) -> Result<R, E>
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                self.msg(Level::Error, err.to_string());
            }
        }
        result
    }

    pub fn eat_ctx<R, E>(&self, ctx: &str, result: Result<R, E>)
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                self.error(format!("{} {}", ctx, err.to_string()));
            }
        }
    }

    pub fn track<T, F>(&self, trackable: &T, f: F)
    where
        T: Trackable,
        F: FnOnce() -> Tracker,
    {
        if trackable.track() {
            let tracker = f();
            self.msg(tracker.level.clone(), trackable.track_fmt(&tracker));
        }
    }

    pub fn track_msg<T, F, M, S>(&self, trackable: &T, f: F, m: M)
    where
        T: Trackable,
        F: FnOnce() -> Tracker,
        M: FnOnce() -> S,
        S: ToString,
    {
        if trackable.track() {
            let tracker = f();
            let message = m().to_string();
            self.msg(
                tracker.level.clone(),
                format!("{} {}", trackable.track_fmt(&tracker), message),
            );
        }
    }
}

pub struct SpanLogBuilder {
    pub entry_timestamp: Timestamp,
    pub attributes: HashMap<String, String>,
}

impl SpanLogBuilder {
    pub fn new() -> Self {
        Self {
            entry_timestamp: timestamp(),
            attributes: HashMap::new(),
        }
    }
}

#[derive(Clone)]
pub struct SpanLogger {
    root_logger: RootLogger,
    span: LogSpan,
    commit_on_drop: bool,
}

impl SpanLogger {
    pub fn span_uuid(&self) -> Uuid {
        self.span.id.clone()
    }

    pub fn point(&self) -> &Point {
        &self.span.point
    }

    pub fn span(&self) -> SpanLogger {
        let span = LogSpan::new(self.point().clone());
        SpanLogger {
            root_logger: self.root_logger.clone(),
            span,
            commit_on_drop: true,
        }
    }

    pub fn spanner<S>(&self, spannable: &S) -> SpanLogger
    where
        S: Spannable,
    {
        let logger = self.span();
        let mut attrs = HashMap::new();
        attrs.insert("type".to_string(), spannable.span_type().to_string());
        attrs.insert("id".to_string(), spannable.span_id().to_string());
        logger.span_attr(attrs);
        logger
    }

    pub fn span_attr(&self, attr: HashMap<String, String>) -> SpanLogger {
        let mut span = LogSpan::new(self.point().clone());
        span.attributes = attr;
        SpanLogger {
            root_logger: self.root_logger.clone(),
            span,
            commit_on_drop: true,
        }
    }

    pub fn span_async(&self) -> SpanLogger {
        let mut span = self.span();
        span.commit_on_drop = false;
        span
    }

    pub fn current_span(&self) -> &LogSpan {
        &self.span
    }

    pub fn entry_timestamp(&self) -> Timestamp {
        self.span.entry_timestamp.clone()
    }

    pub fn set_span_attr<K, V>(&mut self, key: K, value: V)
    where
        K: ToString,
        V: ToString,
    {
        self.span
            .attributes
            .insert(key.to_string(), value.to_string());
    }

    pub fn get_span_attr<K>(&self, key: K) -> Option<String>
    where
        K: ToString,
    {
        self.span.attributes.get(&key.to_string()).cloned()
    }

    pub fn msg<M>(&self, level: Level, message: M)
    where
        M: ToString,
    {
        self.root_logger.log(Log {
            point: self.point().clone(),
            mark: self.span.mark.clone(),
            action: self.span.action.clone(),
            level,
            timestamp: timestamp().timestamp_millis(),
            payload: LogPayload::Message(message.to_string()),
            span: Some(self.span_uuid()),
            source: self.root_logger.source(),
        })
    }

    pub fn trace<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn debug<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn info<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Trace, message);
    }

    pub fn warn<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Warn, message);
    }

    pub fn error<M>(&self, message: M)
    where
        M: ToString,
    {
        self.msg(Level::Error, message);
    }

    pub fn audit(&self) -> AuditLogBuilder {
        AuditLogBuilder {
            logger: self.root_logger.clone(),
            point: self.point().clone(),
            span: self.span.id.clone(),
            attributes: HashMap::new(),
        }
    }

    pub fn builder(&self) -> LogBuilder {
        let builder = RootLogBuilder::new(self.root_logger.clone(), None);
        let builder = LogBuilder::new(self.root_logger.clone(), builder);
        builder
    }

    pub fn log_audit(&self, log: AuditLog) {
        self.root_logger.audit(log);
    }

    pub fn result<R, E>(&self, result: Result<R, E>) -> Result<R, E>
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                self.error(err.to_string());
            }
        }
        result
    }

    pub fn result_ctx<R, E>(&self, ctx: &str, result: Result<R, E>) -> Result<R, E>
    where
        E: ToString,
    {
        match &result {
            Ok(_) => {}
            Err(err) => {
                self.error(format!("{} {}", ctx, err.to_string()));
            }
        }
        result
    }
}

impl Drop for SpanLogger {
    fn drop(&mut self) {
        if self.commit_on_drop {
            let log = LogSpanEvent::new(
                &self.span,
                self.point(),
                LogSpanEventKind::Exit,
                self.span.attributes.clone(),
            );
            self.root_logger.span_event(log)
        }
    }
}

pub struct LogBuilder {
    logger: RootLogger,
    builder: RootLogBuilder,
}

impl LogBuilder {
    pub fn new(logger: RootLogger, builder: RootLogBuilder) -> Self {
        LogBuilder { logger, builder }
    }

    pub fn trace(mut self) -> Self {
        self.builder = self.builder.trace();
        self
    }
    pub fn debug(mut self) -> Self {
        self.builder = self.builder.debug();
        self
    }
    pub fn info(mut self) -> Self {
        self.builder = self.builder.info();
        self
    }
    pub fn warn(mut self) -> Self {
        self.builder = self.builder.warn();
        self
    }
    pub fn error(mut self) -> Self {
        self.builder = self.builder.error();
        self
    }

    pub fn msg<M>(mut self, m: M) -> Self
    where
        M: ToString,
    {
        self.builder = self.builder.msg(m);
        self
    }

    pub fn json<'a, J>(mut self, json: J) -> Self
    where
        J: Into<&'a str>,
    {
        self.builder = self.builder.json(json);
        self
    }

    pub fn json_value(mut self, json: Value) -> Self {
        self.builder = self.builder.json_value(json);
        self
    }

    pub fn commit(mut self) {
        self.builder.commit();
    }
}

pub struct AuditLogBuilder {
    logger: RootLogger,
    point: Point,
    span: Uuid,
    attributes: HashMap<String, String>,
}

impl AuditLogBuilder {
    pub fn new(logger: RootLogger, point: Point, span: Uuid) -> Self {
        AuditLogBuilder {
            logger,
            point,
            attributes: HashMap::new(),
            span,
        }
    }

    // make nice appended call:
    // logger.audit().append("hello","kitty").commit();
    pub fn append<K: ToString, V: ToString>(mut self, key: K, value: V) -> Self {
        self.attributes.insert(key.to_string(), value.to_string());
        self
    }

    pub fn add<K: ToString, V: ToString>(&mut self, key: K, value: V) {
        self.attributes.insert(key.to_string(), value.to_string());
    }

    pub fn kind<K>(mut self, kind: K) -> Self
    where
        K: ToString,
    {
        self.attributes.insert("kind".to_string(), kind.to_string());
        self
    }

    pub fn commit(mut self) {
        let log = AuditLog {
            point: self.point,
            timestamp: timestamp(),
            metrics: self.attributes,
        };
        self.logger.audit(log)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct AuditLog {
    pub point: Point,
    pub timestamp: Timestamp,
    pub metrics: HashMap<String, String>,
}

pub trait Spannable {
    fn span_id(&self) -> String;
    fn span_type(&self) -> &'static str;
}

pub trait Trackable {
    fn track_id(&self) -> String;
    fn track_method(&self) -> String;
    fn track_payload(&self) -> String;
    fn track_from(&self) -> String;
    fn track_to(&self) -> String;
    fn track(&self) -> bool;

    fn track_payload_fmt(&self) -> String {
        self.track_payload()
    }

    fn track_key_fmt(&self) -> String {
        format!(
            "{}::<{}>::[{}]",
            self.track_id(),
            self.track_method(),
            self.track_payload_fmt()
        )
    }

    fn track_fmt(&self, tracker: &Tracker) -> String {
        format!(
            "{}<{}> : {} : ({} -> {})",
            tracker.parsec,
            tracker.action,
            self.track_key_fmt(),
            self.track_from(),
            self.track_to()
        )
    }
}

pub struct Tracker {
    pub parsec: String,
    pub action: String,
    pub level: Level,
}

impl Tracker {
    pub fn new<P: ToString, A: ToString>(parsec: P, action: A) -> Self {
        Self {
            parsec: parsec.to_string(),
            action: action.to_string(),
            level: Level::Info,
        }
    }
}

pub type Track = TrackDef<String>;
pub type TrackRegex = TrackDef<Regex>;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct TrackDef<R> {
    selector: Selector,
    stop: R,
    action: R,
}

impl TrackRegex {
    pub fn new<S: ToString>(selector: S, stop: S, action: S) -> anyhow::Result<Self> {
        let selector = Selector::from_str(selector.to_string().as_str())?;
        let stop = Regex::from_str(stop.to_string().as_str())?;
        let action = Regex::from_str(action.to_string().as_str())?;

        Ok(Self {
            selector,
            stop,
            action,
        })
    }
}

impl TrackDef<String> {
    pub fn new<S: ToString>(selector: S, stop: S, action: S) -> anyhow::Result<Self> {
        let selector = Selector::from_str(selector.to_string().as_str())?;
        Regex::from_str(stop.to_string().as_str())?;
        Regex::from_str(action.to_string().as_str())?;

        let stop = stop.to_string();
        let action = action.to_string();

        Ok(Self {
            selector,
            stop,
            action,
        })
    }

    pub fn to_regex(&self) -> anyhow::Result<TrackRegex> {
        Ok(TrackRegex {
            selector: self.selector.clone(),
            stop: Regex::from_str(self.stop.as_str())?,
            action: Regex::from_str(self.action.as_str())?,
        })
    }
}
