use crate::driver::{Driver, DriverCtx, DriverHandler, DriverSkel, HyperDriverFactory, HyperSkel, Item, ItemHandler, ItemSkel, ItemSphere};
use crate::hyper::space::err::HyperErr;
use crate::hyper::space::platform::Platform;
use crate::hyper::space::star::HyperStarSkel;
use acid_store::repo::Commit;
use acid_store::repo::OpenOptions;
use once_cell::sync::Lazy;
use starlane_space::artifact::ArtRef;
use starlane_space::command::common::{SetProperties, StateSrc};
use starlane_space::command::direct::create::{
    Create, KindTemplate, PointSegTemplate, PointTemplate, Strategy, Template,
};
use starlane_space::config::bind::BindConfig;
use starlane_space::err::SpaceErr;
use starlane_space::hyper::HyperSubstance;
use starlane_space::kind::{ArtifactSubKind, BaseKind, Kind};
use starlane_space::loc::ToBaseKind;
use starlane_space::parse::bind_config;
use starlane_space::particle::PointKind;
use starlane_space::point::Point;
use starlane_space::selector::KindSelector;
use starlane_space::substance::Substance;
use starlane_space::util::log;
use starlane_space::wave::exchange::asynch::InCtx;
use starlane_space::wave::{DirectedProto, Pong, Wave};
use std::str::FromStr;
use std::sync::Arc;
use tempdir::TempDir;

static REPO_BIND_CONFIG: Lazy<ArtRef<BindConfig>> = Lazy::new( ||{ ArtRef::new(
        Arc::new(repo_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/repo.bind").unwrap()
    )});
    static SERIES_BIND_CONFIG: Lazy<ArtRef<BindConfig>> = Lazy::new( ||{ArtRef::new(
        Arc::new(series_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/bundle_series.bind").unwrap()
    )});
    static BUNDLE_BIND_CONFIG: Lazy<ArtRef<BindConfig>> = Lazy::new( ||{ArtRef::new(
        Arc::new(bundle_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/bundle.bind").unwrap()
    )});
    static ARTIFACT_BIND_CONFIG: Lazy<ArtRef<BindConfig>> = Lazy::new( ||{ArtRef::new(
        Arc::new(artifact_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/artifact.bind").unwrap()
    )});

fn artifact_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
        Route -> {
           Http<Get> -> (()) => &;
        }
    }
    "#,
    ))
    .unwrap()
}

fn repo_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
    }
    "#,
    ))
    .unwrap()
}

fn series_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
    }
    "#,
    ))
    .unwrap()
}

fn bundle_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
    }
    "#,
    ))
    .unwrap()
}

pub struct RepoDriverFactory<P> where P: Platform{
}

impl <P> RepoDriverFactory<P> where P: Platform {
    pub fn new() -> Self {
        Self {
        }
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for RepoDriverFactory<P> where P: Platform
{
    fn kind(&self) -> KindSelector {
        KindSelector::from_base(BaseKind::Repo)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(RepoDriver::new(skel,ctx)))
    }
}

pub struct RepoDriver<P>
where
    P: Platform,
{
    skel: HyperStarSkel<P>,
}

impl<P> RepoDriver<P>
where
    P: Platform
{
    pub fn new(skel: HyperStarSkel<P>) -> Self {
        Self { skel}
    }
}


#[handler]
impl<P> RepoDriver<P>
where
    P: Platform
{

}

#[async_trait]
impl<P> Driver<P> for RepoDriver<P>
where
    P: Platform
{
    fn kind(&self) -> Kind {
        Kind::Repo
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        Ok(ItemSphere::Handler(Box::new(Repo)))
    }
}



/*
fn store() -> Result<ValueRepo<String>, UniErr> {
    let config = acid_store::store::DirectoryConfig {
        path: PathBuf::from("./data/artifacts"),
    };

    match OpenOptions::new()
        .mode(acid_store::repo::OpenMode::Create)
        .open(&config)
    {
        Ok(repo) => Ok(repo),
        Err(err) => return Err(UniErr::new(500u16, err.to_string())),
    }
}
 */

pub struct Repo;

#[handler]
impl Repo {}

#[async_trait]
impl<P> ItemHandler<P> for Repo
where
    P: Platform,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(REPO_BIND_CONFIG.clone())
    }
}

pub struct BundleSeriesDriverFactory;

impl BundleSeriesDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P,S> HyperDriverFactory<P> for BundleSeriesDriverFactory
where
    P: Platform
{
    fn kind(&self) -> KindSelector {
        KindSelector::from_base(BaseKind::BundleSeries)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(BundleSeriesDriver::new(ctx)))
    }
}

pub struct BundleSeriesDriver<P> where P: Platform {
    ctx: DriverCtx,
}

#[handler]
impl <P> BundleSeriesDriver<P> where P: Platform{
    pub fn new(ctx: DriverCtx) -> Self {
        Self {
            ctx
        }
    }
}

#[async_trait]
impl<P> Driver<P> for BundleSeriesDriver<P>
where
    P: Platform
{
    fn kind(&self) -> Kind {
        Kind::BundleSeries
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        Ok(ItemSphere::Handler(Box::new(BundleSeries::new(self.ctx.clone()))))
    }
}

pub struct BundleSeries<P> where P: Platform {
    ctx: DriverCtx
}

impl <P> BundleSeries<P> {
   pub fn new( ctx: DriverCtx) -> BundleSeries<P>{
       Self {
           ctx
       }
   }
}

#[handler]
impl <P> BundleSeries<P> where P: Platform {

}

#[async_trait]
impl<P> ItemHandler<P> for BundleSeries<P>
where
    P: Platform,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(SERIES_BIND_CONFIG.clone())
    }
}

pub struct BundleDriverFactory;

impl BundleDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for BundleDriverFactory
where
    P: Platform,
{
    fn kind(&self) -> KindSelector {
        KindSelector::from_base(BaseKind::Bundle)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        let skel = HyperSkel::new(skel, driver_skel);
        Ok(Box::new(BundleDriver::new(skel, ctx)))
    }
}

pub struct BundleDriver<P>
where
    P: Platform,
{
    skel: HyperSkel<P>,
    ctx: DriverCtx,
}

#[handler]
impl<P> BundleDriver<P>
where
    P: Platform,
{
    pub fn new(skel: HyperSkel<P>, ctx: DriverCtx) -> Self {
        Self { skel, ctx }
    }
}

#[async_trait]
impl<P> Driver<P> for BundleDriver<P>
where
    P: Platform,
{
    fn kind(&self) -> Kind {
        Kind::Bundle
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        Ok(ItemSphere::Handler(Box::new(Bundle)))
    }

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        Box::new(BundleDriverHandler::restore(
            self.skel.clone(),
            self.ctx.clone(),
        ))
    }
}

pub struct BundleDriverHandler<P>
where
    P: Platform
{
    skel: HyperSkel<P>,
    ctx: DriverCtx,
}

impl<P> BundleDriverHandler<P>
where
    P: Platform
{
    fn restore(skel: HyperSkel<P>, ctx: DriverCtx) -> Self {
        Self { skel, ctx }
    }
}

impl<P> DriverHandler<P> for BundleDriverHandler<P> where P: Platform{}

#[handler]
impl<P> BundleDriverHandler<P>
where
    P: Platform,
{
    fn store(&self) -> Result<ValueRepo<String>, SpaceErr> {
        let config = acid_store::store::DirectoryConfig {
            path: PathBuf::from(format!("{}artifacts", self.skel.star.data_dir())),
        };

        match OpenOptions::new()
            .mode(acid_store::repo::OpenMode::Create)
            .open(&config)
        {
            Ok(repo) => Ok(repo),
            Err(err) => return Err(SpaceErr::new(500u16, err.to_string())),
        }
    }
    #[route("Hyp<Assign>")]
    async fn assign(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Assign(assign) = ctx.input {
            let state = match &assign.state {
                StateSrc::Substance(data) => data.clone(),
                StateSrc::None => {
                    return self
                        .skel
                        .driver
                        .logger
                        .result(Err("ArtifactBundle cannot be stateless".into()));
                }
            };
            if let Substance::Bin(zip) = (*state).clone() {
                let temp_dir = TempDir::new("zipcheck")?;
                let temp_path = temp_dir.path().clone();
                let file_path = temp_path.with_file_name("file.zip");
                let mut file = File::create(file_path.as_path())?;
                file.write_all(zip.as_slice())?;

                let file = File::open(file_path.as_path())?;
                let mut archive = zip::ZipArchive::new(file)?;
                let mut artifacts = vec![];
                for i in 0..archive.len() {
                    let file = archive.by_index(i).unwrap();
                    if !file.name().ends_with("/") {
                        artifacts.push(file.name().to_string())
                    }
                }

                {
                    let mut store = self.store()?;
                    let state = *state;
                    store.insert(assign.details.stub.point.to_string(), &state)?;
                    store.commit()?;
                }

                self.skel
                    .star
                    .registry
                    .assign_star(&assign.details.stub.point, &self.skel.star.point)
                    .await?;

                let mut point_and_kind_set = HashSet::new();
                for artifact in artifacts {
                    let mut path = String::new();
                    let segments = artifact.split("/");
                    let segments: Vec<&str> = segments.collect();
                    for (index, segment) in segments.iter().enumerate() {
                        path.push_str(segment);
                        if index < segments.len() - 1 {
                            path.push_str("/");
                        }
                        let point = Point::from_str(
                            format!(
                                "{}:/{}",
                                assign.details.stub.point.to_string(),
                                path.as_str()
                            )
                            .as_str(),
                        )?;
                        let kind = if index < segments.len() - 1 {
                            Kind::Artifact(ArtifactSubKind::Dir)
                        } else {
                            Kind::Artifact(ArtifactSubKind::Raw)
                        };
                        let point_and_kind = PointKind { point, kind };
                        point_and_kind_set.insert(point_and_kind);
                    }
                }

                let root_point_and_kind = PointKind {
                    point: Point::from_str(
                        format!("{}:/", assign.details.stub.point.to_string()).as_str(),
                    )?,
                    kind: Kind::Artifact(ArtifactSubKind::Dir),
                };

                point_and_kind_set.insert(root_point_and_kind);

                let mut point_and_kind_set: Vec<PointKind> =
                    point_and_kind_set.into_iter().collect();

                // shortest first will ensure that dirs are created before files
                point_and_kind_set.sort_by(|a, b| {
                    if a.point.to_string().len() > b.point.to_string().len() {
                        Ordering::Greater
                    } else if a.point.to_string().len() < b.point.to_string().len() {
                        Ordering::Less
                    } else {
                        Ordering::Equal
                    }
                });

                {
                    let ctx = self.ctx.clone();
                    //                    tokio::spawn(async move {
                    for point_and_kind in point_and_kind_set {
                        let parent = point_and_kind.point.parent().expect("expected parent");

                        let state = match point_and_kind.kind {
                            Kind::Artifact(ArtifactSubKind::Dir) => StateSrc::None,
                            Kind::Artifact(_) => {
                                let mut path = point_and_kind
                                    .point
                                    .filepath()
                                    .expect("expecting non Dir artifact to have a filepath");
                                // convert to relative path
                                path.remove(0);
                                match archive.by_name(path.as_str()) {
                                    Ok(mut file) => {
                                        let mut buf = vec![];
                                        file.read_to_end(&mut buf);
                                        let bin = Arc::new(buf);
                                        let payload = Substance::Bin(bin);
                                        StateSrc::Substance(Box::new(payload))
                                    }
                                    Err(err) => StateSrc::None,
                                }
                            }
                            _ => {
                                panic!("unexpected knd");
                            }
                        };

                        let create = Create {
                            template: Template {
                                point: PointTemplate {
                                    parent: parent.clone(),
                                    child_segment_template: PointSegTemplate::Exact(
                                        point_and_kind
                                            .point
                                            .last_segment()
                                            .expect("expected final segment")
                                            .to_string(),
                                    ),
                                },
                                kind: KindTemplate {
                                    base: point_and_kind.kind.to_base(),
                                    sub: point_and_kind.kind.sub().into(),
                                    specific: None,
                                },
                            },
                            state,
                            properties: SetProperties::new(),
                            strategy: Strategy::Commit,
                        };

                        let wave: DirectedProto = create.into();
                        let pong: Wave<Pong> = ctx.transmitter.direct(wave).await.unwrap();
                    }
                    //   });
                }
            } else {
                return Err("ArtifactBundle Manager expected Bin payload".into());
            }

            Ok(())
        } else {
            Err(P::Err::new("Bad Reqeust: expected Assign"))
        }
    }
}

pub struct Bundle;

#[handler]
impl Bundle {}

#[async_trait]
impl<P> ItemHandler<P> for Bundle
where
    P: Platform,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(BUNDLE_BIND_CONFIG.clone())
    }
}

pub struct ArtifactDriverFactory;

impl ArtifactDriverFactory {
    pub fn new() -> Self {
        Self {}
    }
}

#[async_trait]
impl<P> HyperDriverFactory<P> for ArtifactDriverFactory
where
    P: Platform,
{
    fn kind(&self) -> KindSelector {
        KindSelector::from_base(BaseKind::Artifact)
    }

    async fn create(
        &self,
        skel: HyperStarSkel<P>,
        driver_skel: DriverSkel<P>,
        ctx: DriverCtx,
    ) -> Result<Box<dyn Driver<P>>, P::Err> {
        Ok(Box::new(ArtifactDriver::new(driver_skel, ctx)))
    }
}

pub struct ArtifactDriver<P>
where
    P: Platform,
{
    skel: DriverSkel<P>,
    ctx: DriverCtx,
}


impl<P> ArtifactDriver<P>
where
    P: Platform,
{
    pub fn new(skel: DriverSkel<P>, ctx: DriverCtx) -> Self {
        Self { skel, ctx }
    }
}

#[handler]
impl<P> ArtifactDriver<P>
where
    P: Platform,
{
}

#[async_trait]
impl<P> Driver<P> for ArtifactDriver<P>
where
    P: Platform,
{
    fn kind(&self) -> Kind {
        Kind::Artifact(ArtifactSubKind::Raw)
    }

    async fn item(&self, point: &Point) -> Result<ItemSphere<P>, P::Err> {
        let record = self.skel.locate(point).await?;

        let skel = ItemSkel::new(point.clone(), record.details.stub.kind, self.skel.clone());
        Ok(ItemSphere::Handler(Box::new(Artifact::restore(
            skel,
            (),
            (),
        ))))
    }

    async fn handler(&self) -> Box<dyn DriverHandler<P>> {
        let skel = HyperSkel::new(self.skel.skel.clone(), self.skel.clone());
        Box::new(ArtifactDriverHandler::restore(skel))
    }
}

pub struct ArtifactDriverHandler<P>
where
    P: Platform,
{
    skel: HyperSkel<P>,
}

impl<P> ArtifactDriverHandler<P>
where
    P: Platform,
{
    fn restore(skel: HyperSkel<P>) -> Self {
        Self { skel }
    }
}

impl<P> DriverHandler<P> for ArtifactDriverHandler<P> where P: Platform {}

#[handler]
impl<P> ArtifactDriverHandler<P>
where
    P: Platform,
{
    fn store(&self) -> Result<ValueRepo<String>, SpaceErr> {
        let config = acid_store::store::DirectoryConfig {
            path: PathBuf::from(format!("{}artifacts", self.skel.star.data_dir())),
        };

        match OpenOptions::new()
            .mode(acid_store::repo::OpenMode::Create)
            .open(&config)
        {
            Ok(repo) => Ok(repo),
            Err(err) => return Err(SpaceErr::new(500u16, err.to_string())),
        }
    }

    #[route("Hyp<Assign>")]
    async fn assign(&self, ctx: InCtx<'_, HyperSubstance>) -> Result<(), P::Err> {
        if let HyperSubstance::Assign(assign) = ctx.input {
            if let Kind::Artifact(sub) = &assign.details.stub.kind {
                match sub {
                    ArtifactSubKind::Dir => {}
                    _ => {
                        let substance = assign.state.get_substance()?;
                        let mut store = self.skel.driver.logger.result(self.store())?;
                        store
                            .insert(assign.details.stub.point.to_string(), &substance)
                            .map_err(|e| SpaceErr::server_error(e.to_string()))?;
                        self.skel.driver.logger.result(
                            store
                                .commit()
                                .map_err(|e| SpaceErr::server_error(e.to_string())),
                        )?;
                    }
                }
                self.skel
                    .star
                    .registry
                    .assign_star(&assign.details.stub.point, &self.skel.star.point)
                    .await?;
            }
            Ok(())
        } else {
            Err(P::Err::new("ArtifactDriver expected Assign"))
        }
    }
}

pub struct Artifact<P>
where
    P: Platform,
{
    skel: ItemSkel<P>,
}

#[handler]
impl<P> Artifact<P>
where
    P: Platform,
{
    fn store(&self) -> Result<ValueRepo<String>, SpaceErr> {
        let config = acid_store::store::DirectoryConfig {
            path: PathBuf::from(format!("{}artifacts", self.skel.data_dir())),
        };

        match OpenOptions::new()
            .mode(acid_store::repo::OpenMode::Create)
            .open(&config)
        {
            Ok(repo) => Ok(repo),
            Err(err) => return Err(SpaceErr::new(500u16, err.to_string())),
        }
    }

    #[route("Cmd<Read>")]
    pub async fn read(&self, _ctx: InCtx<'_, ()>) -> Result<Substance, P::Err> {
        if let Kind::Artifact(ArtifactSubKind::Dir) = self.skel.kind {
            return Ok(Substance::Empty);
        }
        let store = self.store()?;

        let substance: Substance = store.get(&self.skel.point.to_string()).unwrap();
        Ok(substance)
    }

    #[route("Http<Get>")]
    pub async fn get(&self, _: InCtx<'_, ()>) -> Result<Substance, P::Err> {
        if let Kind::Artifact(ArtifactSubKind::Dir) = self.skel.kind {
            return Ok(Substance::Empty);
        }
        let store = self.store()?;

        let substance: Substance = store.get(&self.skel.point.to_string()).unwrap();
        Ok(substance)
    }
}

impl<P> Item<P> for Artifact<P>
where
    P: Platform,
{
    type Skel = ItemSkel<P>;
    type Ctx = ();
    type State = ();

    fn restore(skel: Self::Skel, ctx: Self::Ctx, state: Self::State) -> Self {
        Self { skel }
    }
}

/*
#[async_trait]
impl<P> DirectedHandler for Artifact<P> where P: Cosmos {
    async fn handle(&self, ctx: RootInCtx) -> CoreBounce {
        println!("ARTIFACT HANDLE REQUEST : {}",ctx.wave.clone().to_ultra().desc());

        let core = ReflectedCore {
            headers: Default::default(),
            status: Default::default(),
            body: Default::default()
        };
        CoreBounce::Reflected(core)
    }
}

 */

#[async_trait]
impl<P> ItemHandler<P> for Artifact<P>
where
    P: Platform,
{
    async fn bind(&self) -> Result<ArtRef<BindConfig>, P::Err> {
        Ok(ARTIFACT_BIND_CONFIG.clone())
    }
}
