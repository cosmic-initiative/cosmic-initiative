use std::ops::Deref;

use nom::combinator::all_consuming;
use regex::Regex;
use serde::{Deserialize, Serialize};

use cosmic_nom::new_span;

use crate::err::SpaceErr;
use crate::point::Meta;
use crate::parse::camel_case_chars;
use crate::parse::error::result;
use crate::parse::model::MethodScopeSelector;
use crate::substance::{FormErrs, Substance};
use crate::util::{ValueMatcher, ValuePattern};
use crate::wave::core::http2::StatusCode;
use crate::wave::core::{DirectedCore, HeaderMap, Method, ReflectedCore};
use url::Url;

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct ExtMethod {
    string: String,
}

impl ExtMethod {
    pub fn new<S: ToString>(string: S) -> Result<Self, SpaceErr> {
        let tmp = string.to_string();
        let string = result(all_consuming(camel_case_chars)(new_span(tmp.as_str())))?.to_string();
        Ok(Self { string })
    }
}

impl ToString for ExtMethod {
    fn to_string(&self) -> String {
        self.string.clone()
    }
}

impl ValueMatcher<ExtMethod> for ExtMethod {
    fn is_match(&self, x: &ExtMethod) -> Result<(), ()> {
        if *self == *x {
            Ok(())
        } else {
            Err(())
        }
    }
}

impl Into<MethodScopeSelector> for ExtMethod {
    fn into(self) -> MethodScopeSelector {
        MethodScopeSelector::new(
            ValuePattern::Pattern(Method::Ext(self)),
            Regex::new(".*").unwrap(),
        )
    }
}

impl TryFrom<String> for ExtMethod {
    type Error = SpaceErr;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl TryFrom<&str> for ExtMethod {
    type Error = SpaceErr;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl Deref for ExtMethod {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl Default for ExtMethod {
    fn default() -> Self {
        Self {
            string: "Def".to_string(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtDirected {
    pub method: ExtMethod,

    pub headers: HeaderMap,

    pub uri: Url,
    pub body: Substance,
}

impl Default for ExtDirected {
    fn default() -> Self {
        Self {
            method: Default::default(),
            headers: Default::default(),
            uri: Url::parse("http:://localhost/").unwrap(),
            body: Default::default(),
        }
    }
}

impl ExtDirected {
    pub fn new<M>(method: M) -> Result<Self, SpaceErr>
    where
        M: TryInto<ExtMethod, Error = SpaceErr>,
    {
        Ok(ExtDirected {
            method: method.try_into()?,
            ..Default::default()
        })
    }

    pub fn with_body(mut self, body: Substance) -> Self {
        self.body = body;
        self
    }

    pub fn ok(&self, payload: Substance) -> ReflectedCore {
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(200u16).unwrap(),
            body: payload,
        }
    }

    pub fn fail(&self, error: &str) -> ReflectedCore {
        let errors = FormErrs::default(error);
        ReflectedCore {
            headers: Default::default(),
            status: StatusCode::from_u16(500u16).unwrap(),
            body: Substance::FormErrs(errors),
        }
    }
}

impl TryFrom<DirectedCore> for ExtDirected {
    type Error = SpaceErr;

    fn try_from(core: DirectedCore) -> Result<Self, Self::Error> {
        if let Method::Ext(action) = core.method {
            Ok(Self {
                method: action,
                headers: core.headers,
                uri: core.uri,
                body: core.body,
            })
        } else {
            Err("expected Ext".into())
        }
    }
}
