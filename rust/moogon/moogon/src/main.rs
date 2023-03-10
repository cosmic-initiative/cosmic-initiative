mod server;
mod parse;

use axum::response::Html;
use axum::Router;
use axum::routing::get;
use crate::server::MoogonServer;

#[tokio::main]
async fn main() {

    let mut singularity = MoogonServer::new();
    singularity.serve("/*path".to_string(), "../me-axum/target/wasm32-wasi/debug/me-axum.wasm".to_string() ).await.unwrap();
    singularity.start().await.unwrap();
}



async fn handler() -> Html<&'static str> {
    Html("<h1>Hello, World!</h1>")
}
