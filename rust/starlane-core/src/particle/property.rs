use std::collections::HashMap;
use std::ops::Deref;
use mesh_portal::version::latest::command::common::{PropertyMod, SetProperties};
use mesh_portal::version::latest::id::Point;
use mesh_portal::version::latest::payload::{Payload, PayloadPattern};
use mesh_portal_api_client::ResourceCommand::Add;
use mesh_portal_versions::version::v0_0_1::util::ValueMatcher;
use validator::validate_email;
use crate::error::Error;

pub struct PropertyDef{
    pub pattern: Box<dyn PropertyPattern>,
    pub required: bool,
    pub mutable: bool,
    pub source: PropertySource,
    pub default: Option<String>,
    pub constant: bool,
    pub permits: Vec<PropertyPermit>
}

impl PropertyDef {
    pub fn new( pattern: Box<dyn PropertyPattern>, required: bool, mutable: bool, source: PropertySource, default: Option<String>, constant: bool, permits: Vec<PropertyPermit>) -> Result<Self,Error> {

        if constant {
            default.as_ref().ok_or("if PropertyDef is a constant then 'default' value must be set")?;
        }

        if let Some(value) = default.as_ref() {
            match pattern.is_match(value) {
                Ok(_) => {}
                Err(err) => {
                    return Err(format!("default value does not match pattern: {}",err.to_string()).into());
                }
            }
        }

        Ok(Self{
            pattern,
            required,
            mutable,
            source,
            default,
            constant,
            permits
        })

    }
}

pub trait PropertyPattern: Send+Sync+'static {
    fn is_match( &self, value: &String ) -> Result<(),Error>;
}

#[derive(Clone)]
pub struct AnythingPattern {}

impl PropertyPattern for AnythingPattern {
    fn is_match(&self, value: &String) -> Result<(), Error> {
        Ok(())
    }
}

#[derive(Clone)]
pub struct PointPattern {}

impl PropertyPattern for PointPattern {
    fn is_match(&self, value: &String) -> Result<(), Error> {
        use std::str::FromStr;
        Point::from_str(value.as_str())?;
        Ok(())
    }
}

#[derive(Clone)]
pub struct U64Pattern{}

impl PropertyPattern for U64Pattern{
    fn is_match(&self, value: &String) -> Result<(), Error> {
        use std::str::FromStr;
        match u64::from_str(value.as_str()) {
            Ok(_) => {
                Ok(())
            }
            Err(err) => {
                Err(err.to_string().into())
            }

        }
    }
}

#[derive(Clone)]
pub struct BoolPattern{}

impl PropertyPattern for BoolPattern{
    fn is_match(&self, value: &String) -> Result<(), Error> {
        use std::str::FromStr;
        match bool::from_str(value.as_str()) {
            Ok(_) => {
                Ok(())
            }
            Err(err) => {
                Err(err.to_string().into())
            }

        }
    }
}

#[derive(Clone)]
pub struct EmailPattern{}

impl PropertyPattern for EmailPattern{
    fn is_match(&self, value: &String) -> Result<(), Error> {
        if !validate_email(value) {
            Err(format!("not a valid email: '{}'",value).into())
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
    CoreSecret
}

pub struct PropertiesConfig {
    pub properties: HashMap<String,PropertyDef>
}

impl Deref for PropertiesConfig {
    type Target = HashMap<String,PropertyDef>;

    fn deref(&self) -> &Self::Target {
        &self.properties
    }
}

impl PropertiesConfig {

    pub fn new() -> PropertiesConfig{
        Self{
            properties: HashMap::new()
        }
    }

    pub fn builder() -> PropertiesConfigBuilder {
        PropertiesConfigBuilder {
            config: Self::new()
        }
    }

    pub fn required(&self) -> Vec<String> {
        let mut rtn = vec![];
        for (key,def) in &self.properties {
            if def.required {
                rtn.push(key.clone());
            }
        }
        rtn
    }

    pub fn defaults(&self) -> Vec<String> {
        let mut rtn = vec![];
        for (key,def) in &self.properties {
            if def.default.is_some() {
                rtn.push(key.clone());
            }
        }
        rtn
    }


    pub fn check_create(&self, set: &SetProperties ) -> Result<(),Error> {
        for req in self.required() {
            if !set.contains_key(&req) {
                return Err(format!("missing required property: '{}'", req).into());
            }
        }

        for (key,propmod) in &set.map {
            let def = self.get(key).ok_or(format!("illegal property: '{}'",key))?;
            match propmod {
                PropertyMod::Set { key, value, lock } => {
                    if def.constant && def.default.as_ref().unwrap().clone() != value.clone() {
                        return Err(format!("property: '{}' is constant and cannot be set",key).into());
                    }
                    def.pattern.is_match(value)?;
                    match def.source {
                        PropertySource::CoreReadOnly => {
                            return Err(format!("property '{}' is flagged CoreReadOnly and cannot be set within the Mesh",key).into());
                        }
                        _ => {}
                    }
                }
                PropertyMod::UnSet(_) => {
                    return Err(format!("cannot unset: '{}' during particle create",key).into());
                }
            }

        }
        Ok(())
    }

    pub fn check_update(&self, set: &SetProperties ) -> Result<(),Error> {
        for (key,propmod) in &set.map {
            let def = self.get(key).ok_or(format!("illegal property: '{}'",key))?;
            match propmod {
                PropertyMod::Set { key, value, lock } => {
                    if def.constant {
                        return Err(format!("property: '{}' is constant and cannot be set",key).into());
                    }
                    def.pattern.is_match(value)?;
                    match def.source {
                        PropertySource::CoreReadOnly => {
                            return Err(format!("property '{}' is flagged CoreReadOnly and cannot be set within the Mesh",key).into());
                        }
                        _ => {}
                    }
                }
                PropertyMod::UnSet(_) => {
                    if !def.mutable  {
                        return Err(format!("property '{}' is immutable and cannot be changed after particle creation",key).into())
                    }
                    if def.required{
                        return Err(format!("property '{}' is required and cannot be unset",key).into())
                    }
                }
            }

        }
        Ok(())
    }

    pub fn check_read( &self, keys: &Vec<String> ) -> Result<(),Error> {
       for key in keys {
           let def = self.get(key).ok_or(format!("illegal property: '{}'",key))?;
           match def.source {
               PropertySource::CoreSecret=> {
                   return Err(format!("property '{}' is flagged CoreSecret and cannot be read within the Mesh",key).into());
               }
               _ => {}
           }
       }
       Ok(())
    }

    pub fn fill_create_defaults( &self, set: &SetProperties ) -> Result<SetProperties,Error> {
        let mut rtn = set.clone();
        let defaults = self.defaults();
        for d in defaults {
            if !rtn.contains_key(&d) {
                let def = self.get(&d).ok_or(format!("expected default property def: {}",&d))?;
                let value = def.default.as_ref().ok_or(format!("expected default property def: {}",&d))?.clone();
                rtn.push( PropertyMod::Set { key: d, value, lock: false } );
            }
        }
        Ok(rtn)
    }
}

pub enum PropertyPermit {
    Read,
    Write
}

pub struct PropertiesConfigBuilder {
    config: PropertiesConfig
}

impl PropertiesConfigBuilder {
    pub fn build(self) -> PropertiesConfig {
        self.config
    }

    pub fn add( &mut self, name: &str, pattern: Box<dyn PropertyPattern>, required: bool, mutable: bool, source: PropertySource, default: Option<String>, constant: bool, permits: Vec<PropertyPermit>) -> Result<(),Error> {
        let def = PropertyDef::new( pattern,required,mutable,source,default,constant,permits)?;
        self.config.properties.insert(name.to_string(), def );
        Ok(())
    }

    pub fn add_string( &mut self, name: &str ) -> Result<(),Error> {
        let def = PropertyDef::new( Box::new(AnythingPattern{} ),false,true,PropertySource::Shell,None,false,vec![])?;
        self.config.properties.insert(name.to_string(), def );
        Ok(())
    }

    pub fn add_address( &mut self, name: &str, required: bool, mutable: bool ) -> Result<(),Error> {
        let def = PropertyDef::new(Box::new(PointPattern {} ), required, mutable, PropertySource::Shell, None, false, vec![])?;
        self.config.properties.insert(name.to_string(), def );
        Ok(())
    }
}

