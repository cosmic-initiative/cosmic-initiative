use std::env;
use std::process::{Child, Command};

use mesh_portal::version::latest::id::Point;

use crate::error::Error;
use crate::starlane::STARLANE_MECHTRON_PORT;

pub fn launch_mechtron_process(wasm_src: Point) -> Result<Child, Error> {
    let host = format!("localhost:{}", STARLANE_MECHTRON_PORT.to_string());
    let program = env::args().next().expect("expected first argument");
    let child = Command::new(program.as_str())
        .arg("mechtron")
        .arg(host.as_str())
        .arg(wasm_src.to_string().as_str())
        .spawn()?;
    Ok(child)
}
