use core::str::FromStr;

use nom::AsBytes;
use semver::Version;
use serde::{Deserialize, Serialize};

use crate::hyper::Knock;
use crate::wave::{Ping, Pong, UltraWave};
use crate::SpaceErr;

pub struct PrimitiveFrame {
    pub data: Vec<u8>,
}

impl PrimitiveFrame {
    pub fn size(&self) -> u32 {
        self.data.len() as u32
    }
}

impl From<Vec<u8>> for PrimitiveFrame {
    fn from(value: Vec<u8>) -> Self {
        Self { data: value }
    }
}

impl From<String> for PrimitiveFrame {
    fn from(value: String) -> Self {
        let bytes = value.as_bytes();
        Self {
            data: bytes.to_vec(),
        }
    }
}

impl TryInto<String> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<String, Self::Error> {
        Ok(String::from_utf8(self.data)?)
    }
}

impl TryInto<semver::Version> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<semver::Version, Self::Error> {
        let data = String::from_utf8(self.data)?;
        Ok(semver::Version::from_str(data.as_str())?)
    }
}

impl From<semver::Version> for PrimitiveFrame {
    fn from(version: Version) -> Self {
        let data = version.to_string();
        PrimitiveFrame::from(data)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, strum_macros::Display)]
pub enum CloseReason {
    Done,
    Error(String),
}

impl TryInto<PrimitiveFrame> for Ping {
    type Error = SpaceErr;

    fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
        let data = bincode::serialize(&self)?;
        Ok(PrimitiveFrame::from(data))
    }
}

impl TryInto<Ping> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<Ping, Self::Error> {
        Ok(bincode::deserialize(self.data.as_bytes())?)
    }
}

impl TryInto<PrimitiveFrame> for Pong {
    type Error = SpaceErr;

    fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
        let data = bincode::serialize(&self)?;
        Ok(PrimitiveFrame::from(data))
    }
}

impl TryInto<Pong> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<Pong, Self::Error> {
        Ok(bincode::deserialize(self.data.as_bytes())?)
    }
}

impl TryInto<PrimitiveFrame> for UltraWave {
    type Error = SpaceErr;

    fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
        let data = bincode::serialize(&self)?;
        Ok(PrimitiveFrame::from(data))
    }
}

impl TryInto<UltraWave> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<UltraWave, Self::Error> {
        Ok(bincode::deserialize(self.data.as_bytes())?)
    }
}

impl TryInto<PrimitiveFrame> for Knock {
    type Error = SpaceErr;

    fn try_into(self) -> Result<PrimitiveFrame, Self::Error> {
        let data = bincode::serialize(&self)?;
        Ok(PrimitiveFrame::from(data))
    }
}

impl TryInto<Knock> for PrimitiveFrame {
    type Error = SpaceErr;

    fn try_into(self) -> Result<Knock, Self::Error> {
        Ok(bincode::deserialize(self.data.as_bytes())?)
    }
}
