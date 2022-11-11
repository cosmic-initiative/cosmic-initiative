use crate::Cosmos;
use cosmic_space::command::common::{SetProperties, SetRegistry};
use cosmic_space::command::direct::create::Strategy;
use cosmic_space::command::direct::delete::Delete;
use cosmic_space::command::direct::query::{Query, QueryResult};
use cosmic_space::command::direct::select::{Select, SubSelect};
use cosmic_space::hyper::{ParticleLocation, ParticleRecord};
use cosmic_space::loc::Point;
use cosmic_space::particle::{Details, Properties, Status, Stub};
use cosmic_space::security::{Access, AccessGrant, IndexedAccessGrant};
use cosmic_space::selector::Selector;
use cosmic_space::substance::SubstanceList;
use std::sync::Arc;

pub type Registry<P> = Arc<dyn RegistryApi<P>>;

#[async_trait]
pub trait RegistryApi<P>: Send + Sync
where
    P: Cosmos,
{
    async fn nuke<'a>(&'a self) -> Result<(), P::Err>;

    async fn register<'a>(&'a self, registration: &'a Registration) -> Result<(), P::Err>;

    async fn assign_star<'a>(&'a self, point: &'a Point, star: &'a Point) -> Result<(), P::Err>;

    async fn assign_host<'a>(&'a self, point: &'a Point, host: &'a Point) -> Result<(), P::Err>;

    async fn set_status<'a>(&'a self, point: &'a Point, status: &'a Status) -> Result<(), P::Err>;

    async fn set_properties<'a>(
        &'a self,
        point: &'a Point,
        properties: &'a SetProperties,
    ) -> Result<(), P::Err>;

    async fn sequence<'a>(&'a self, point: &'a Point) -> Result<u64, P::Err>;

    async fn get_properties<'a>(&'a self, point: &'a Point) -> Result<Properties, P::Err>;

    async fn record<'a>(&'a self, point: &'a Point) -> Result<ParticleRecord, P::Err>;

    async fn query<'a>(&'a self, point: &'a Point, query: &'a Query)
        -> Result<QueryResult, P::Err>;

    async fn delete<'a>(&'a self, delete: &'a Delete) -> Result<SubstanceList, P::Err>;

    async fn select<'a>(&'a self, select: &'a mut Select) -> Result<SubstanceList, P::Err>;

    async fn sub_select<'a>(&'a self, sub_select: &'a SubSelect) -> Result<Vec<Stub>, P::Err>;

    async fn grant<'a>(&'a self, access_grant: &'a AccessGrant) -> Result<(), P::Err>;

    async fn access<'a>(&'a self, to: &'a Point, on: &'a Point) -> Result<Access, P::Err>;

    async fn chown<'a>(
        &'a self,
        on: &'a Selector,
        owner: &'a Point,
        by: &'a Point,
    ) -> Result<(), P::Err>;

    async fn list_access<'a>(
        &'a self,
        to: &'a Option<&'a Point>,
        on: &'a Selector,
    ) -> Result<Vec<IndexedAccessGrant>, P::Err>;

    async fn remove_access<'a>(&'a self, id: i32, to: &'a Point) -> Result<(), P::Err>;
}

#[derive(Clone)]
pub struct Registration {
    pub point: Point,
    pub kind: Kind,
    pub registry: SetRegistry,
    pub properties: SetProperties,
    pub owner: Point,
    pub strategy: Strategy,
    pub status: Status,
}
