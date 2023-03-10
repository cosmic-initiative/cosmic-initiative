use std::future::Future;
use std::sync::atomic::{AtomicU16, Ordering};
use anyhow::anyhow;
use regex::Regex;
use std::net::TcpListener;
use std::sync::Arc;
use std::thread;
use axum::body::Body;
use axum::extract::State;
use axum::http::{Request, Response, Uri};
use axum::response::Html;
use axum::Router;
use axum::routing::{any, get};
use hyper::client::HttpConnector;
use tokio::sync::RwLock;
use wasmtime::{Engine, Linker, Module, Store};
use wasmtime_wasi::WasiCtxBuilder;

type Client = hyper::client::Client<HttpConnector, Body>;

pub struct MoogonServer {
    pub state: Arc<RwLock<AppState>>
}

impl MoogonServer {

    pub fn new() -> Self {
        let rtn = Self {
            state: Arc::new(RwLock::new(AppState::new()))
        };



        rtn
    }

    pub async fn start(&self) -> Result<(),anyhow::Error>{

        let app = Router::new().route("/*path", get(handler)).with_state(self.state.clone());

        let std_listener = std::net::TcpListener::bind("127.0.0.1:8080").unwrap();
        std_listener.set_nonblocking(true).unwrap();
            axum::Server::from_tcp(std_listener)
                .unwrap()
                .serve(app.into_make_service()).await.unwrap();
        Ok(())
    }

    pub async fn serve(&mut self, path: String, wasm: String ) -> Result<(),anyhow::Error> {
        let path = Regex::new(path.as_str() )?;

        loop {
            let port = {
                let state = self.state.write().await;
                state.port_index.fetch_add(1, Ordering::Relaxed)
            };
            match TcpListener::bind(format!("127.0.0.1:{}", port )) {
                Ok(listener) => {
                    listener.set_nonblocking(true);
                    let route = Route {
                        path,
                        port
                    };
                    let mut state = self.state.write().await;
                    state.routes.push(route);


                    let listener = wasmtime_wasi::TcpListener::from_std(listener);
                    thread::spawn( move || {
                        // Define the WASI functions globally on the `Config`.
                        let engine = Engine::default();
                        let mut linker = Linker::new(&engine);
                        wasmtime_wasi::add_to_linker(&mut linker, |s| s);

                        // Create a WASI context and put it in a Store; all instances in the store
                        // share this context. `WasiCtxBuilder` provides a number of ways to
                        // configure what the target program will have access to.
                        let wasi = WasiCtxBuilder::new()
                            .inherit_stdio().preopened_socket(3, listener).unwrap()
                            .inherit_args().unwrap()
                            .build();
                        let mut store = Store::new(&engine, wasi);

                        // Instantiate our module with the imports we've created, and run it.
                        let module = Module::from_file(&engine, wasm).unwrap();
                        linker.module(&mut store, "", &module).unwrap();
                        linker
                            .get_default(&mut store, "").unwrap()
                            .typed::<(), ()>(&store).unwrap()
                            .call(&mut store, ()).unwrap();
                    });



                    break;
                }
                Err(_) => {
                    if port > 10000 {
                        return Err(anyhow!("out of ports"))
                    }
                }
            }
        }

        Ok(())
    }


}

pub struct AppState {
    pub routes: Vec<Route>,
    pub port_index: AtomicU16,
    pub client: Client
}

impl AppState {
    pub fn new() -> Self {
        Self {
            routes: vec![],
            port_index: AtomicU16::new(9000),
            client: Client::new()
        }
    }
}

pub struct Route {
    pub path: Regex,
    pub port: u16
}


async fn handler(State(state): State<Arc<RwLock<AppState>>>, mut req: Request<Body>) -> Response<Body> {

    let (client, port) = {
        let state = state.read().await;
        (state.client.clone(), state.routes.get(0).unwrap().port)
    };

    let path = req.uri().path();
    let path_query = req
        .uri()
        .path_and_query()
        .map(|v| v.as_str())
        .unwrap_or(path);

    let uri = format!("http://127.0.0.1:{}{}", port, path_query);

    *req.uri_mut() = Uri::try_from(uri).unwrap();

    client.request(req).await.unwrap()
}