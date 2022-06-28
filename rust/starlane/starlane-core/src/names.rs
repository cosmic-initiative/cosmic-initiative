use std::fmt;
use std::str::{FromStr, Split};

use serde::{Deserialize, Serialize};

use crate::artifact::SubSpaceName;
use crate::error::Error;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct Name {
    pub sub_space: SubSpaceName,
    pub path: String,
}

impl Name {
    pub fn more(string: &str) -> Result<(Self, Split<&str>), Error> {
        let (sub_space, mut parts) = SubSpaceName::more(string)?;

        Ok((
            Name {
                sub_space: sub_space,
                path: parts.next().ok_or("path")?.to_string(),
            },
            parts,
        ))
    }

    pub fn from(string: &str) -> Result<Self, Error> {
        let (name, _) = Name::more(string)?;
        Ok(name)
    }

    pub fn to(&self) -> String {
        let mut rtn = String::new();
        rtn.push_str(self.sub_space.to().as_str());
        rtn.push_str(":");
        rtn.push_str(self.path.as_str());
        return rtn;
    }

    pub fn as_name(&self) -> Self {
        self.clone()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to())
    }
}

impl FromStr for Name {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (name, _) = Name::more(s)?;
        Ok(name)
    }
}
