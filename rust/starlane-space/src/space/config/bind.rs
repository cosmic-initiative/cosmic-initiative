use anyhow::anyhow;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::convert::TryInto;

use crate::space::err::Error;
use crate::space::loc::Topic;
use crate::space::parse::model::{
    BindScope, MethodScope, PipelineSegmentDef, RouteScope,
    ScopeFilters,
};
use crate::space::parse::{bind_config, Env};
use crate::space::point::{Point, PointCtx, PointVar};
use crate::space::selector::PayloadBlockDef;
use crate::space::substance::{CallDef, SubstancePattern};
use crate::space::util::{ToResolved, ValueMatcher, ValuePattern};
use crate::space::wave::core::MethodPattern;
use crate::space::wave::DirectedWave;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WaveDirection {
    Direct,
    Reflect,
}

#[derive(Clone)]
pub struct BindConfig {
    scopes: Vec<BindScope>,
}

impl BindConfig {
    pub fn new(scopes: Vec<BindScope>) -> Self {
        Self { scopes }
    }

    pub fn route_scopes(&self) -> Vec<&RouteScope> {
        let mut scopes = vec![];
        for scope in &self.scopes {
            if let BindScope::RequestScope(request_scope) = &scope {
                scopes.push(request_scope);
            }
        }
        scopes
    }

    pub fn select(&self, directed: &DirectedWave) -> err::Result<&MethodScope> {
        for route_scope in self.route_scopes() {
            if route_scope.selector.is_match(directed).is_ok() {
                for message_scope in &route_scope.block {
                    if message_scope.selector.is_match(directed).is_ok() {
                        for method_scope in &message_scope.block {
                            if method_scope.selector.is_match(directed).is_ok() {
                                return Ok(method_scope);
                            }
                        }
                    }
                }
            }
        }
        Err(err!(
                "no route matches {}<{}>{}",
                directed.kind().to_string(),
                directed.core().method.to_string(),
                directed.core().uri.path().to_string()
            ))
    }
}

impl TryFrom<Vec<u8>> for BindConfig {
    type Error = anyhow::Error;

    fn try_from(doc: Vec<u8>) -> err::Result<Self> {
        let doc = String::from_utf8(doc)?;
        bind_config(doc.as_str())
    }
}

pub struct Cursor {}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigScope<T, E> {
    pub scope_type: T,
    pub elements: Vec<E>,
}

impl<T, E> ConfigScope<T, E> {
    pub fn new(scope_type: T, elements: Vec<E>) -> Self {
        Self {
            scope_type,
            elements,
        }
    }
}

pub type Pipeline = PipelineDef<Point>;
pub type PipelineCtx = PipelineDef<PointCtx>;

#[derive(Debug, Clone)]
pub struct PipelineDef<Pnt> {
    pub segments: Vec<PipelineSegmentDef<Pnt>>,
}

impl<Pnt> PipelineDef<Pnt> {
    pub fn new() -> Self {
        Self { segments: vec![] }
    }

    pub fn consume(&mut self) -> Option<PipelineSegmentDef<Pnt>> {
        if self.segments.is_empty() {
            Option::None
        } else {
            Option::Some(self.segments.remove(0))
        }
    }
}

pub type PipelineStepVar = PipelineStepDef<PointVar>;
pub type PipelineStepCtx = PipelineStepDef<PointCtx>;
pub type PipelineStep = PipelineStepDef<Point>;

#[derive(Debug, Clone)]
pub struct PipelineStepDef<Pnt> {
    pub entry: WaveDirection,
    pub exit: WaveDirection,
    pub blocks: Vec<PayloadBlockDef<Pnt>>,
}

impl<Pnt> PipelineStepDef<Pnt> {
    pub fn direct() -> Self {
        Self {
            entry: WaveDirection::Direct,
            exit: WaveDirection::Direct,
            blocks: vec![],
        }
    }

    pub fn rtn() -> Self {
        Self {
            entry: WaveDirection::Reflect,
            exit: WaveDirection::Reflect,
            blocks: vec![],
        }
    }
}

impl ToResolved<PipelineStep> for PipelineStepCtx {
    fn to_resolved(self, env: &Env) -> err::Result<PipelineStep> {
        let mut blocks = vec![];
        for block in self.blocks {
            blocks.push(block.to_resolved(env)?);
        }

        Ok(PipelineStep {
            entry: self.entry,
            exit: self.exit,
            blocks,
        })
    }
}

impl ToResolved<PipelineStepCtx> for PipelineStepVar {
    fn to_resolved(self, env: &Env) -> err::Result<PipelineStepCtx> {
        let mut blocks = vec![];
        for block in self.blocks {
            blocks.push(block.to_resolved(env)?);
        }

        Ok(PipelineStepCtx {
            entry: self.entry,
            exit: self.exit,
            blocks,
        })
    }
}

impl PipelineStep {
    pub fn new(entry: WaveDirection, exit: WaveDirection) -> Self {
        Self {
            entry,
            exit,
            blocks: vec![],
        }
    }
}

pub type PatternBlock = ValuePattern<SubstancePattern>;

pub type PipelineStopCtx = PipelineStopDef<PointCtx>;
pub type PipelineStopVar = PipelineStopDef<PointVar>;
pub type PipelineStop = PipelineStopDef<Point>;

#[derive(Debug, Clone)]
pub enum PipelineStopDef<Pnt> {
    Core,
    Call(CallDef<Pnt>),
    Reflect,
    Point(Pnt),
    Err { status: u16, msg: String },
}

impl ToResolved<PipelineStop> for PipelineStopVar {
    fn to_resolved(self, env: &Env) -> err::Result<PipelineStop> {
        let stop: PipelineStopCtx = self.to_resolved(env)?;
        stop.to_resolved(env)
    }
}

impl ToResolved<PipelineStop> for PipelineStopCtx {
    fn to_resolved(self, env: &Env) -> err::Result<PipelineStop> {
        Ok(match self {
            PipelineStopCtx::Core => PipelineStop::Core,
            PipelineStopCtx::Call(call) => PipelineStop::Call(call.to_resolved(env)?),
            PipelineStopCtx::Reflect => PipelineStop::Reflect,
            PipelineStopCtx::Point(point) => PipelineStop::Point(point.to_resolved(env)?),
            PipelineStopCtx::Err { status, msg } => PipelineStop::Err { status, msg },
        })
    }
}

impl ToResolved<PipelineStopCtx> for PipelineStopVar {
    fn to_resolved(self, env: &Env) -> err::Result<PipelineStopCtx> {
        Ok(match self {
            PipelineStopVar::Core => PipelineStopCtx::Core,
            PipelineStopVar::Call(call) => PipelineStopCtx::Call(call.to_resolved(env)?),
            PipelineStopVar::Reflect => PipelineStopCtx::Reflect,
            PipelineStopVar::Point(point) => PipelineStopCtx::Point(point.to_resolved(env)?),
            PipelineStopVar::Err { status, msg } => PipelineStopCtx::Err { status, msg },
        })
    }
}

pub enum Whitelist {
    Any,
    None,
    Enumerated(Vec<CallPattern>),
}

pub enum CallPattern {
    Any,
    Call,
}

#[derive(Clone)]
pub struct RouteSelector {
    pub topic: Option<ValuePattern<Topic>>,
    pub method: ValuePattern<MethodPattern>,
    pub path: Regex,
    pub filters: ScopeFilters,
}

impl ToString for RouteSelector {
    fn to_string(&self) -> String {
        format!("{}", self.method.to_string())
    }
}

impl RouteSelector {
    pub fn new(
        topic: Option<ValuePattern<Topic>>,
        method: ValuePattern<MethodPattern>,
        path: Regex,
        filters: ScopeFilters,
    ) -> Self {
        Self {
            topic,
            method,
            path,
            filters,
        }
    }

    pub fn any() -> Self {
        Self {
            topic: None,
            method: ValuePattern::Always,
            path: Regex::new("/.*").unwrap(),
            filters: Default::default(),
        }
    }

    pub fn from_method(method: ValuePattern<MethodPattern>) -> Self {
        Self {
            topic: None,
            method,
            path: Regex::new("/.*").unwrap(),
            filters: Default::default(),
        }
    }

    pub fn with_topic(self, topic: Topic) -> Self {
        Self {
            topic: Some(ValuePattern::Pattern(topic)),
            method: self.method,
            path: self.path,
            filters: self.filters,
        }
    }

    pub fn is_match<'a>(&self, wave: &'a DirectedWave) -> Result<(),()> {
        self.method.is_match(&wave.core().method)?;
        Ok(())
        /*        match self.path.is_match(&wave.core().uri.path()) {
                   true => Ok(()),
                   false => Err(()),
               }

        */
    }
}
