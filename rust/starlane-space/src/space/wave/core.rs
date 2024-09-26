use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use starlane_primitive_macros::Autobox;

use crate::space::command::Command;
use crate::space::err::StatusErr;
use crate::space::loc::ToSurface;
use crate::space::substance::FormErrs;
use crate::space::util::{ValueMatcher, ValuePattern};
use crate::space::wave::core::cmd::CmdMethod;
use crate::space::wave::core::ext::ExtMethod;
use crate::space::wave::core::http2::{HttpMethod, StatusCode};
use crate::space::wave::core::hyp::HypMethod;
use crate::space::wave::{Bounce, PingCore, PongCore, ToRecipients, WaveId};
use crate::{SpaceErr, Substance, Surface, ToSubstance};
use url::Url;

pub mod cmd;
pub mod ext;
pub mod http2;
pub mod hyp;

impl From<Result<ReflectedCore, SpaceErr>> for ReflectedCore {
    fn from(result: Result<ReflectedCore, SpaceErr>) -> Self {
        match result {
            Ok(response) => response,
            Err(err) => err.into(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct ReflectedCore {
    pub headers: HeaderMap,
    pub status: StatusCode,
    pub body: Substance,
}

impl<S> ToSubstance<S> for ReflectedCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.body.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.body.to_substance_ref()
    }
}

impl ReflectedCore {
    pub fn to_err(&self) -> SpaceErr {
        if self.status.is_success() {
            "cannot convert a success into an error".into()
        } else {
            if let Substance::FormErrs(errors) = &self.body {
                errors.to_starlane_err()
            } else {
                self.status.to_string().into()
            }
        }
    }

    pub fn ok_html(html: &str) -> Self {
        let bin = html.to_string().into_bytes();
        ReflectedCore::ok_body(Substance::Bin(bin))
    }

    pub fn new() -> Self {
        ReflectedCore {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn result(result: Result<ReflectedCore, SpaceErr>) -> ReflectedCore {
        match result {
            Ok(core) => core,
            Err(err) => {
                let mut core = ReflectedCore::status(err.status());
                core.body = Substance::FormErrs(FormErrs::from(err));
                core
            }
        }
    }

    pub fn ok() -> Self {
        Self::ok_body(Substance::Empty)
    }

    pub fn ok_body(body: Substance) -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body,
        }
    }

    pub fn timeout() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(408u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn server_error() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn status(status: u16) -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(status).unwrap_or(StatusCode::from_u16(500).unwrap()),
            body: Substance::Empty,
        }
    }

    pub fn not_found() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(404u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn forbidden() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(403u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn bad_request() -> Self {
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(400u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn fail<S: ToString>(status: u16, message: S) -> Self {
        let errors = FormErrs::default(message);
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(status)
                .or_else(|_| StatusCode::from_u16(500u16))
                .unwrap(),
            body: Substance::FormErrs(errors),
        }
    }

    pub fn err(err: SpaceErr) -> Self {
        let errors = FormErrs::default(err.to_string().as_str());
        Self {
            headers: HeaderMap::new(),
            status: StatusCode::from_u16(err.status())
                .unwrap_or(StatusCode::from_u16(500u16).unwrap()),
            body: Substance::FormErrs(errors),
        }
    }

    pub fn with_new_substance(self, substance: Substance) -> Self {
        Self {
            headers: self.headers,
            status: self.status,
            body: substance,
        }
    }

    pub fn is_ok(&self) -> bool {
        self.status.is_success()
    }

    pub fn into_reflection<P>(self, intended: Surface, to: P, reflection_of: WaveId) -> PongCore
    where
        P: ToSurface,
    {
        PongCore {
            to: to.to_surface(),
            intended: intended.to_recipients(),
            core: self,
            reflection_of: reflection_of,
        }
    }
}

impl ReflectedCore {
    pub fn as_result<E: From<&'static str>, P: TryFrom<Substance>>(self) -> Result<P, E> {
        if self.status.is_success() {
            match P::try_from(self.body) {
                Ok(substance) => Ok(substance),
                Err(err) => Err(E::from("error")),
            }
        } else {
            Err(E::from("error"))
        }
    }

    pub fn ok_or(&self) -> Result<(), SpaceErr> {
        if self.is_ok() {
            Ok(())
        } else if let Substance::Err(err) = &self.body {
            Err(err.clone())
        } else {
            Err(SpaceErr::new(self.status.as_u16(), "error"))
        }
    }
}

/*
impl TryInto<http::response::Builder> for ReflectedCore {
    type Error = UniErr;

    fn try_into(self) -> Result<http::response::Builder, Self::Error> {
        let mut builder = http::response::Builder::new();

        for (name, value) in self.headers {
            match name {
                Some(name) => {
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                }
                None => {}
            }
        }

        Ok(builder.status(self.status))
    }
}

impl TryInto<http::Response<Bin>> for ReflectedCore {
    type Error = UniErr;

    fn try_into(self) -> Result<http::Response<Bin>, Self::Error> {
        let mut builder = http::response::Builder::new();

        for (name, value) in self.headers {
            match name {
                Some(name) => {
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                }
                None => {}
            }
        }

        let response = builder.status(self.status).body(self.body.to_bin()?)?;
        Ok(response)
    }
}

 */

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Autobox)]
pub enum Method {
    Hyp(HypMethod),
    Cmd(CmdMethod),
    Http(HttpMethod),
    Ext(ExtMethod),
}

impl Method {
    pub fn to_deep_string(&self) -> String {
        match self {
            Method::Hyp(x) => format!("Hyp<{}>", x.to_string()),
            Method::Cmd(x) => format!("Cmd<{}>", x.to_string()),
            Method::Http(x) => format!("Http<{}>", x.to_string()),
            Method::Ext(x) => format!("Ext<{}>", x.to_string()),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MethodPattern {
    Hyp(ValuePattern<HypMethod>),
    Cmd(ValuePattern<CmdMethod>),
    Http(ValuePattern<HttpMethod>),
    Ext(ValuePattern<ExtMethod>),
}

impl ToString for MethodPattern {
    fn to_string(&self) -> String {
        match self {
            MethodPattern::Cmd(c) => {
                format!("Cmd<{}>", c.to_string())
            }
            MethodPattern::Http(c) => {
                format!("Http<{}>", c.to_string())
            }
            MethodPattern::Ext(c) => {
                format!("Ext<{}>", c.to_string())
            }
            MethodPattern::Hyp(c) => {
                format!("Hyp<{}>", c.to_string())
            }
        }
    }
}

impl ValueMatcher<Method> for MethodPattern {
    fn is_match(&self, x: &Method) -> Result<(), ()> {
        match self {
            MethodPattern::Hyp(pattern) => {
                if let Method::Hyp(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
            MethodPattern::Cmd(pattern) => {
                if let Method::Cmd(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
            MethodPattern::Http(pattern) => {
                if let Method::Http(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
            MethodPattern::Ext(pattern) => {
                if let Method::Ext(v) = x {
                    pattern.is_match(v)
                } else {
                    Err(())
                }
            }
        }
    }
}

impl ValueMatcher<Method> for Method {
    fn is_match(&self, x: &Method) -> Result<(), ()> {
        if x == self {
            Ok(())
        } else {
            Err(())
        }
    }
}

impl Method {
    pub fn kind(&self) -> MethodKind {
        match self {
            Method::Cmd(_) => MethodKind::Cmd,
            Method::Http(_) => MethodKind::Http,
            Method::Ext(_) => MethodKind::Ext,
            Method::Hyp(_) => MethodKind::Hyp,
        }
    }
}

impl ToString for Method {
    fn to_string(&self) -> String {
        match self {
            Method::Cmd(cmd) => format!("Cmd<{}>", cmd.to_string()),
            Method::Http(method) => format!("Http<{}>", method.to_string()),
            Method::Ext(msg) => format!("Ext<{}>", msg.to_string()),
            Method::Hyp(sys) => format!("Hyp<{}>", sys.to_string()),
        }
    }
}

impl Into<DirectedCore> for Method {
    fn into(self) -> DirectedCore {
        DirectedCore {
            headers: Default::default(),
            method: self,
            uri: Url::parse("http://localhost/").unwrap(),
            body: Substance::Empty,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct DirectedCore {
    pub headers: HeaderMap,
    pub method: Method,
    pub uri: Url,
    pub body: Substance,
}

impl<S> ToSubstance<S> for DirectedCore
where
    Substance: ToSubstance<S>,
{
    fn to_substance(self) -> Result<S, SpaceErr> {
        self.body.to_substance()
    }

    fn to_substance_ref(&self) -> Result<&S, SpaceErr> {
        self.body.to_substance_ref()
    }
}

impl DirectedCore {
    pub fn new(method: Method) -> Self {
        Self {
            method,
            headers: HeaderMap::new(),
            uri: Url::parse("http://localhost/").unwrap(),
            body: Default::default(),
        }
    }

    pub fn to_selection_str(&self) -> String {
        format!(
            "{}{} -[{}]->",
            self.method.to_deep_string(),
            self.uri.path(),
            self.body.kind().to_string()
        )
    }

    pub fn ext<M: Into<ExtMethod>>(method: M) -> Self {
        let method: ExtMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn http<M: Into<HttpMethod>>(method: M) -> Self {
        let method: HttpMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }

    pub fn cmd<M: Into<CmdMethod>>(method: M) -> Self {
        let method: CmdMethod = method.into();
        let method: Method = method.into();
        Self::new(method)
    }
}

impl TryFrom<PingCore> for DirectedCore {
    type Error = SpaceErr;

    fn try_from(request: PingCore) -> Result<Self, Self::Error> {
        Ok(request.core)
    }
}

impl DirectedCore {
    pub fn kind(&self) -> MethodKind {
        self.method.kind()
    }
}

impl Into<DirectedCore> for Command {
    fn into(self) -> DirectedCore {
        DirectedCore {
            body: Substance::Command(Box::new(self)),
            method: Method::Cmd(CmdMethod::Command),
            ..Default::default()
        }
    }
}

/*
impl TryFrom<http::Request<Bin>> for DirectedCore {
    type Error = UniErr;

    fn try_from(request: http::Request<Bin>) -> Result<Self, Self::Error> {
        Ok(Self {
            headers: request.headers().clone(),
            method: Method::Http(request.method().clone().try_into()?),
            uri: request.uri().clone(),
            body: Substance::Bin(request.body().clone()),
        })
    }
}

impl TryInto<http::Request<Bin>> for DirectedCore {
    type Error = UniErr;

    fn try_into(self) -> Result<http::Request<Bin>, UniErr> {
        let mut builder = http::Request::builder();
        for (name, value) in self.headers {
            match name {
                Some(name) => {
                    builder = builder.header(name.as_str(), value.to_str()?.to_string().as_str());
                }
                None => {}
            }
        }
        match self.method {
            Method::Http(method) => {
                builder = builder.method(method).uri(self.uri);
                Ok(builder.body(self.body.to_bin()?)?)
            }
            _ => Err("cannot convert to http response".into()),
        }
    }
}

 */

impl Default for DirectedCore {
    fn default() -> Self {
        Self {
            headers: Default::default(),
            method: Method::Http(HttpMethod::Get),
            uri: Url::parse("http://localhost/").unwrap(),
            body: Substance::Empty,
        }
    }
}

impl DirectedCore {
    pub fn with_body(self, body: Substance) -> Self {
        Self {
            headers: self.headers,
            uri: self.uri,
            method: self.method,
            body,
        }
    }

    pub fn server_error(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn timeout(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(408u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn not_found(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(404u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn forbidden(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(403u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn bad_request(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(400u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn substance(method: Method, body: Substance) -> DirectedCore {
        DirectedCore {
            method,
            body,
            ..Default::default()
        }
    }

    pub fn ok(&self) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: Substance::Empty,
        }
    }

    pub fn ok_body(&self, body: Substance) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body,
        }
    }

    pub fn fail<M: ToString>(&self, status: u16, message: M) -> ReflectedCore {
        let errors = FormErrs::default(message.to_string().as_str());
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(status)
                .or_else(|_| StatusCode::from_u16(500u16))
                .unwrap(),
            body: Substance::FormErrs(errors),
        }
    }

    pub fn err<E: StatusErr>(&self, error: E) -> ReflectedCore {
        let errors = FormErrs::default(error.message().as_str());
        let status = match StatusCode::from_u16(error.status()) {
            Ok(status) => status,
            Err(_) => StatusCode::from_u16(500u16).unwrap(),
        };
        println!("----->   returning STATUS of {}", status.to_string());
        ReflectedCore {
            headers: Default::default(),
            status,
            body: Substance::FormErrs(errors),
        }
    }
}

impl From<()> for ReflectedCore {
    fn from(_: ()) -> Self {
        ReflectedCore::ok()
    }
}

impl Into<ReflectedCore> for Surface {
    fn into(self) -> ReflectedCore {
        ReflectedCore::ok_body(Substance::Surface(self))
    }
}

impl TryFrom<ReflectedCore> for Surface {
    type Error = SpaceErr;

    fn try_from(core: ReflectedCore) -> Result<Self, Self::Error> {
        if !core.status.is_success() {
            Err(SpaceErr::new(core.status.as_u16(), "error"))
        } else {
            match core.body {
                Substance::Surface(surface) => Ok(surface),
                substance => Err(format!(
                    "expecting Surface received {}",
                    substance.kind().to_string()
                )
                .into()),
            }
        }
    }
}

pub type CoreBounce = Bounce<ReflectedCore>;

#[derive(
    Debug,
    Clone,
    Serialize,
    Deserialize,
    strum_macros::Display,
    strum_macros::EnumString,
    Eq,
    PartialEq,
)]
pub enum MethodKind {
    Hyp,
    Cmd,
    Ext,
    Http,
}

impl ValueMatcher<MethodKind> for MethodKind {
    fn is_match(&self, x: &MethodKind) -> Result<(), ()> {
        if self == x {
            Ok(())
        } else {
            Err(())
        }
    }
}

pub type HeaderMap = HashMap<String, String>;

pub trait CoreReflector {
    fn as_reflected_core(self) -> ReflectedCore;
}