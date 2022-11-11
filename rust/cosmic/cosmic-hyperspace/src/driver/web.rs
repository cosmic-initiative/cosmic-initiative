use crate::driver::{
    Driver, DriverCtx, DriverHandler, DriverSkel, DriverStatus, HyperDriverFactory, HyperItemSkel,
    HyperSkel, ItemHandler, ItemRouter, ItemSkel, ItemSphere,
};
use crate::err::HyperErr;
use crate::reg::Registration;
use crate::star::{HyperStarSkel, LayerInjectionRouter};
use crate::Cosmos;
use ascii::IntoAsciiString;
use cosmic_space::artifact::ArtRef;
use cosmic_space::command::common::StateSrc;
use cosmic_space::command::direct::create::{
    Create, KindTemplate, PointSegTemplate, PointTemplate, Strategy, Template,
};
use cosmic_space::config::bind::BindConfig;
use cosmic_space::err::SpaceErr;
use cosmic_space::fail::http;
use cosmic_space::hyper::{HyperSubstance, ParticleLocation};
use cosmic_space::kind::{BaseKind, Kind, NativeSub};
use cosmic_space::loc::{Layer, Point, ToSurface};
use cosmic_space::parse::{bind_config, CamelCase};
use cosmic_space::particle::traversal::{Traversal, TraversalDirection};
use cosmic_space::particle::Status;
use cosmic_space::selector::{ProtoKindSelector, Pattern, SubKindSelector};
use cosmic_space::substance::{Bin, Substance};
use cosmic_space::util::{log, ValuePattern};
use cosmic_space::wave::core::http2::{HttpMethod, HttpRequest};
use cosmic_space::wave::core::{DirectedCore, HeaderMap, ReflectedCore};
use cosmic_space::wave::exchange::asynch::{
    InCtx, ProtoTransmitter, ProtoTransmitterBuilder, TraversalRouter,
};
use cosmic_space::wave::exchange::SetStrategy;
use cosmic_space::wave::{
    Agent, DirectedProto, Handling, HandlingKind, Ping, ToRecipients, UltraWave, WaitTime, Wave,
};
use cosmic_space::HYPERUSER;
use inflector::Inflector;
use std::str::FromStr;
use std::sync::Arc;
use std::thread;
use tiny_http::Server;
use tokio::runtime::Runtime;
use url::Url;

lazy_static! {
    static ref WEB_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(web_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/web.bind").unwrap()
    );
}

fn web_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
        Route<Http<*>> -> localhost => &;
    }
    "#,
    ))
    .unwrap()
}

pub struct WebDriverFactory;

impl WebDriverFactory {
    pub fn new() -> Self {
        Self
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for WebDriverFactory
where
    P: Cosmos,
{
    fn kind(&self) -> ProtoKindSelector {
        ProtoKindSelector {
            base: Pattern::Exact(BaseKind::Native),
            sub: SubKindSelector::Exact(Some(CamelCase::from_str("Web").unwrap())),
            specific: ValuePattern::Any,
        }
    }

    async fn create(
        &self,
        _: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        _: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(WebDriver::new(driver_skel)))
    }
}

pub struct WebDriver<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
}

impl<P> WebDriver<P>
where
    P: Cosmos,
{
    pub fn new(skel: DriverSkel<P>) -> Self {
        Self { skel }
    }
}

#[async_trait]
impl<P> Driver<P> for WebDriver<P>
where
    P: Cosmos,
{
    fn kind(&self) -> Kind {
        Kind::Native(NativeSub::Web)
    }

    async fn init(&mut self, skel: DriverSkel<P>, ctx: DriverCtx) -> Result<(), P::Err> {
        let point = self.skel.point.push("http-server")?;
        let registration = Registration {
            point: point.clone(),
            kind: Kind::Native(NativeSub::Web),
            registry: Default::default(),
            properties: Default::default(),
            owner: HYPERUSER.clone(),
            strategy: Strategy::Ensure,
            status: Status::Ready,
        };

        self.skel.skel.api.create_states(point.clone()).await?;
        self.skel.skel.registry.register(&registration).await?;
        self.skel
            .skel
            .registry
            .assign_star(&point, &self.skel.skel.point)
            .await?;

        let item_skel = ItemSkel::new(point, Kind::Native(NativeSub::Web), self.skel.clone());
        let mut runner = WebRunner::new(item_skel);
        runner.start();

        skel.status_tx
            .send(DriverStatus::Ready)
            .await
            .unwrap_or_default();
        Ok(())
    }
    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        let skel = ItemSkel::new(
            point.clone(),
            Kind::Native(NativeSub::Web),
            self.skel.clone(),
        );
        Ok(ItemSphere::Router(Box::new(Web::new(skel))))
    }

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        Box::new(WebDriverHandler::restore(self.skel.clone()))
    }
}

pub struct WebDriverHandler<P>
where
    P: Cosmos,
{
    skel: DriverSkel<P>,
}

impl<P> WebDriverHandler<P>
where
    P: Cosmos,
{
    fn restore(skel: DriverSkel<P>) -> Self {
        WebDriverHandler { skel }
    }
}

impl<P> DriverHandler<P> for WebDriverHandler<P> where P: Cosmos {}

#[handler]
impl<P> WebDriverHandler<P>
where
    P: Cosmos,
{
    /*
    #[route("Hyp<Assign>")]
    async fn assign(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Assign(assign) = ctx.input {
            let skel = ItemSkel::new(
                assign.details.stub.point.clone(),
                Kind::Native(NativeSub::Web),
                self.skel.clone(),
            );
            let mut runner = WebRunner::new(skel);
            runner.start();
        }
        Ok(())
    }

     */
}

pub struct Web<P>
where
    P: Cosmos,
{
    skel: ItemSkel<P>,
}

impl<P> Web<P>
where
    P: Cosmos,
{
    pub fn new(skel: ItemSkel<P>) -> Self {
        Self { skel }
    }
}

#[async_trait]
impl<P> TraversalRouter for Web<P>
where
    P: Cosmos,
{
    async fn traverse(&self, traversal: Traversal<UltraWave>) -> Result<(), SpaceErr> {
        if traversal.is_directed() {
        } else {
            let wave = traversal.payload;
            let reflected = wave.to_reflected().unwrap();

            self.skel
                .skel
                .skel
                .exchanger
                .reflected(reflected)
                .await
                .unwrap_or_default();
        }
        Ok(())
    }
}

#[async_trait]
impl<P> ItemRouter<P> for Web<P>
where
    P: Cosmos,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(WEB_BIND_CONFIG.clone())
    }
}

pub struct WebRunner<P>
where
    P: Cosmos,
{
    pub skel: ItemSkel<P>,
    pub transmitter: ProtoTransmitter,
}

impl<P> WebRunner<P>
where
    P: Cosmos,
{
    pub fn new(skel: ItemSkel<P>) -> Self {
        let mut router = LayerInjectionRouter::new(
            skel.skel.skel.clone(),
            skel.point.clone().to_surface().with_layer(Layer::Core),
        );

        router.direction = Some(TraversalDirection::Fabric);
        let router = Arc::new(router);

        let mut transmitter =
            ProtoTransmitterBuilder::new(router, skel.skel.skel.exchanger.clone());
        transmitter.from =
            SetStrategy::Override(skel.point.clone().to_surface().with_layer(Layer::Gravity));
        transmitter.to = SetStrategy::Override(
            skel.point
                .clone()
                .to_surface()
                .with_layer(Layer::Core)
                .to_recipients(),
        );
        transmitter.handling = SetStrategy::Fill(Handling {
            kind: HandlingKind::Immediate,
            priority: Default::default(),
            retries: Default::default(),
            wait: WaitTime::Low,
        });
        transmitter.agent = SetStrategy::Fill(Agent::Anonymous);

        let transmitter = transmitter.build();

        Self { skel, transmitter }
    }

    pub fn start(mut self) {
        let runtime = tokio::runtime::Handle::current();
        thread::spawn(move || {
            let port = self.skel.skel.skel.machine.cosmos.web_port().unwrap();
            let server = Server::http(format!("0.0.0.0:{}", port)).unwrap();
            for req in server.incoming_requests() {
                let runtime = runtime.clone();
                let transmitter = self.transmitter.clone();
                runtime.spawn(async move {
                    match Self::handle::<P>(transmitter, req).await {
                        Ok(_) => {}
                        Err(err) => {
                            println!("http handle ERR: {}", err.to_string());
                        }
                    }
                });
            }
        });
    }

    async fn handle<C>(
        transmitter: ProtoTransmitter,
        mut req: tiny_http::Request,
    ) -> Result<(), C::Err>
    where
        C: Cosmos,
    {
        let method = req
            .method()
            .to_string()
            .to_lowercase()
            .as_str()
            .to_title_case();

        let method = HttpMethod::from_str(method.as_str())?;
        let mut headers = HeaderMap::new();
        for header in req.headers() {
            headers.insert(header.field.to_string(), header.value.to_string());
        }
        let url = format!("http://localhost{}", req.url());
        let uri: Url = Url::from_str(url.as_str())?;
        let body = match req.body_length().as_ref() {
            None => Substance::Empty,
            Some(len) => {
                let mut buf: Vec<u8> = Vec::with_capacity(*len);
                let reader = req.as_reader();
                reader.read_to_end(&mut buf);
                let buf = Arc::new(buf);
                Substance::Bin(buf)
            }
        };

        let request = HttpRequest {
            method,
            headers,
            uri,
            body,
        };

        let core: DirectedCore = request.into();

        let mut wave = DirectedProto::ping();
        wave.core(core);
        //        wave.track = true;
        let pong = transmitter.ping(wave).await?;

        let body = pong.core.body.clone().to_bin()?;
        let mut headers = vec![];
        for (name, value) in pong.core.headers.clone() {
            let header = tiny_http::Header {
                field: tiny_http::HeaderField::from_str(name.as_str())?,
                value: value.into_ascii_string()?,
            };
            headers.push(header);
        }
        let data_length = Some(body.len());

        rayon::spawn(move || {
            let response = tiny_http::Response::new(
                tiny_http::StatusCode(pong.core.status.as_u16()),
                headers,
                body.as_slice(),
                data_length,
                None,
            );

            req.respond(response);
        });

        Ok(())
    }
}
