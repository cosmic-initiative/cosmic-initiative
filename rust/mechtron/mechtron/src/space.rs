use crate::membrane::mechtron_consume_string;
use crate::{mechtron_timestamp, mechtron_uuid};
use cosmic_space::point;
use cosmic_space::wasm::Timestamp;

#[no_mangle]
extern "C" fn cosmic_uuid() -> point::Uuid {
    point::Uuid::from_unwrap(mechtron_consume_string(unsafe { mechtron_uuid() }).unwrap())
}

#[no_mangle]
extern "C" fn cosmic_timestamp() -> Timestamp {
    Timestamp::new(unsafe { mechtron_timestamp() })
}
