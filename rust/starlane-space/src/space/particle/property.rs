use core::str::FromStr;
use std::collections::HashMap;
use std::ops::Deref;
use validator::validate_email;

use crate::space::command::common::PropertyMod;
use crate::space::err;
use crate::space::err::err;
use crate::space::parse::SkewerCase;
use crate::space::point::Point;
use crate::{Kind, SetProperties};

pub struct PropertyDef {
    pub pattern: Box<dyn PropertyPattern>,
    pub required: bool,
    pub mutable: bool,
    pub source: PropertySource,
    pub default: Option<String>,
    pub constant: bool,
    pub permits: Vec<PropertyPermit>,
}

impl PropertyDef {
    pub fn new(
        pattern: Box<dyn PropertyPattern>,
        required: bool,
        mutable: bool,
        source: PropertySource,
        default: Option<String>,
        constant: bool,
        permits: Vec<PropertyPermit>,
    ) -> err::Result<Self> {
        if constant {
            default
                .as_ref()
                .ok_or(err!("if PropertyDef is a constant then 'default' value must be set"))?;
        }

        if let Some(value) = default.as_ref() {
            match pattern.is_match(value) {
                Ok(_) => {}
                Err(err) => {
                    return Err(err!(
                        "default value does not match pattern: "
                    )
                    );
                }
            }
        }

        Ok(Self {
            pattern,
            required,
            mutable,
            source,
            default,
            constant,
            permits,
        })
    }
}

pub trait PropertyPattern: Send + Sync + 'static {
    fn is_match(&self, value: &String) -> Result<(),()>;
}

#[derive(Clone)]
pub struct AnythingPattern {}

impl PropertyPattern for AnythingPattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        Ok(())
    }
}

#[derive(Clone)]
pub struct PointPattern {}

impl PropertyPattern for PointPattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        use std::str::FromStr;
        Point::from_str(value.as_str()).map_err(|_| ());
        Ok(())
    }
}

#[derive(Clone)]
pub struct U64Pattern {}

impl PropertyPattern for U64Pattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        use std::str::FromStr;
        match u64::from_str(value.as_str()) {
            Ok(_) => Ok(()),
            Err(err) => Err(()),
        }
    }
}

#[derive(Clone)]
pub struct BoolPattern {}

impl PropertyPattern for BoolPattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        use std::str::FromStr;
        match bool::from_str(value.as_str()) {
            Ok(_) => Ok(()),
            Err(err) => Err(()),
        }
    }
}

#[derive(Clone)]
pub struct UsernamePattern {}

impl PropertyPattern for UsernamePattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        SkewerCase::from_str(value.as_str()).map_err(|_| ())?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct EmailPattern {}

impl PropertyPattern for EmailPattern {
    fn is_match(&self, value: &String) -> Result<(),()> {
        if !validate_email(value) {
            Err(())
        } else {
            Ok(())
        }
    }
}

#[derive(Clone)]
pub enum PropertySource {
    Shell,
    Core,
    CoreReadOnly,
    CoreSecret,
}

pub struct PropertiesConfig {
    pub properties: HashMap<String, PropertyDef>,
    pub kind: Kind,
}

impl Deref for PropertiesConfig {
    type Target = HashMap<String, PropertyDef>;

    fn deref(&self) -> &Self::Target {
        &self.properties
    }
}

impl PropertiesConfig {
    pub fn new(kind: Kind) -> PropertiesConfig {
        Self {
            properties: HashMap::new(),
            kind,
        }
    }

    pub fn builder() -> PropertiesConfigBuilder {
        PropertiesConfigBuilder {
            kind: None,
            properties: HashMap::new(),
        }
    }

    pub fn required(&self) -> Vec<String> {
        let mut rtn = vec![];
        for (key, def) in &self.properties {
            if def.required {
                rtn.push(key.clone());
            }
        }
        rtn
    }

    pub fn defaults(&self) -> Vec<String> {
        let mut rtn = vec![];
        for (key, def) in &self.properties {
            if def.default.is_some() {
                rtn.push(key.clone());
            }
        }
        rtn
    }

    pub fn check_create(&self, set: &SetProperties) -> err::Result<()> {
        for req in self.required() {
            if !set.contains_key(&req) {
                return Err(err!(
                    "{} missing required property: '{}'",
                    self.kind.to_string(),
                    req
                )
                );
            }
        }

        for (key, propmod) in &set.map {
            let def = self.get(key).ok_or(err!(
                "{} illegal property: '{}'",
                self.kind.to_string(),
                key
            ))?;
            match propmod {
                PropertyMod::Set { key, value, lock } => {
                    if def.constant && def.default.as_ref().unwrap().clone() != value.clone() {
                        return Err(err!(
                            "{} property: '{}' is constant and cannot be set",
                            self.kind.to_string(),
                            key
                        )
                        .into());
                    }
                    def.pattern.is_match(value).map_err(|_|err!("could not match"))?;
                    match def.source {
                        PropertySource::CoreReadOnly => {
                            return Err(err!("{} property '{}' is flagged CoreReadOnly and cannot be set within the Mesh",self.kind.to_string(), key).into());
                        }
                        _ => {}
                    }
                }
                PropertyMod::UnSet(_) => {
                    return Err(err!("cannot unset: '{}' during particle create", key));
                }
            }
        }
        Ok(())
    }

    pub fn check_update(&self, set: &SetProperties) -> err::Result<()> {
        for (key, propmod) in &set.map {
            let def = self
                .get(key)
                .ok_or(err!("illegal property: '{}'", key))?;
            match propmod {
                PropertyMod::Set { key, value, lock } => {
                    if def.constant {
                        return Err(
                            err!("property: '{}' is constant and cannot be set", key).into()
                        );
                    }
                    def.pattern.is_match(value).map_err(|_|err!("could not match"))?;
                    match def.source {
                        PropertySource::CoreReadOnly => {
                            return Err(err!("property '{}' is flagged CoreReadOnly and cannot be set within the Mesh",key).into());
                        }
                        _ => {}
                    }
                }
                PropertyMod::UnSet(_) => {
                    if !def.mutable {
                        return Err(err!("property '{}' is immutable and cannot be changed after particle creation",key).into());
                    }
                    if def.required {
                        return Err(
                            err!("property '{}' is required and cannot be unset", key).into(),
                        );
                    }
                }
            }
        }
        Ok(())
    }

    pub fn check_read(&self, keys: &Vec<String>) -> err::Result<()> {
        for key in keys {
            let def = self
                .get(key)
                .ok_or(err!("illegal property: '{}'", key))?;
            match def.source {
                PropertySource::CoreSecret => {
                    return Err(err!(
                        "property '{}' is flagged CoreSecret and cannot be read within the Mesh",
                        key
                    )
                    .into());
                }
                _ => {}
            }
        }
        Ok(())
    }

    pub fn fill_create_defaults(&self, set: &SetProperties) -> err::Result<SetProperties> {
        let mut rtn = set.clone();
        let defaults = self.defaults();
        for d in defaults {
            if !rtn.contains_key(&d) {
                let def = self
                    .get(&d)
                    .ok_or(err!("expected default property def: {}", &d))?;
                let value = def
                    .default
                    .as_ref()
                    .ok_or(err!("expected default property def: {}", &d))?
                    .clone();
                rtn.push(PropertyMod::Set {
                    key: d,
                    value,
                    lock: false,
                });
            }
        }
        Ok(rtn)
    }
}

pub enum PropertyPermit {
    Read,
    Write,
}

pub struct PropertiesConfigBuilder {
    kind: Option<Kind>,
    properties: HashMap<String, PropertyDef>,
}

impl PropertiesConfigBuilder {
    pub fn new() -> Self {
        let mut rtn = Self {
            kind: None,
            properties: HashMap::new(),
        };
        rtn.add_point("bind", false, true).unwrap();
        rtn
    }

    pub fn build(self) -> err::Result<PropertiesConfig> {
        Ok(PropertiesConfig {
            kind: self.kind.ok_or(err!("kind must be set before PropertiesConfig can be built") )?,
            properties: self.properties,
        })
    }

    pub fn kind(&mut self, kind: Kind) {
        self.kind.replace(kind);
    }

    pub fn add(
        &mut self,
        name: &str,
        pattern: Box<dyn PropertyPattern>,
        required: bool,
        mutable: bool,
        source: PropertySource,
        default: Option<String>,
        constant: bool,
        permits: Vec<PropertyPermit>,
    ) -> err::Result<()> {
        let def = PropertyDef::new(
            pattern, required, mutable, source, default, constant, permits,
        )?;
        self.properties.insert(name.to_string(), def);
        Ok(())
    }

    pub fn add_string(&mut self, name: &str) -> err::Result<()> {
        let def = PropertyDef::new(
            Box::new(AnythingPattern {}),
            false,
            true,
            PropertySource::Shell,
            None,
            false,
            vec![],
        )?;
        self.properties.insert(name.to_string(), def);
        Ok(())
    }

    pub fn add_point(&mut self, name: &str, required: bool, mutable: bool) -> err::Result<()> {
        let def = PropertyDef::new(
            Box::new(PointPattern {}),
            required,
            mutable,
            PropertySource::Shell,
            None,
            false,
            vec![],
        )?;
        self.properties.insert(name.to_string(), def);
        Ok(())
    }
}
