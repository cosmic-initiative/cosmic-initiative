use crate::point;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Timestamp {
    pub millis: i64,
}

impl Timestamp {
    pub fn timestamp_millis(&self) -> i64 {
        self.millis
    }

    pub fn new(millis: i64) -> Self {
        Self { millis }
    }
}

#[no_mangle]
extern "C" {
    pub fn cosmic_timestamp() -> Timestamp;
    pub fn cosmic_uuid() -> point::Uuid;
}
