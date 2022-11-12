use crate::err::CosmicErr;
use crate::mem::cosmos::MemCosmos;
use crate::reg::{Registration, RegistryApi};
use crate::Cosmos;
use cosmic_space::command::common::{PropertyMod, SetProperties};
use cosmic_space::command::direct::delete::Delete;
use cosmic_space::command::direct::query::{Query, QueryResult};
use cosmic_space::command::direct::select::{Select, SubSelect};
use cosmic_space::hyper::{ParticleLocation, ParticleRecord};
use cosmic_space::point::Point;
use cosmic_space::parse::get_properties;
use cosmic_space::particle::{Details, Properties, Property, Status, Stub};
use cosmic_space::security::{Access, AccessGrant, IndexedAccessGrant};
use cosmic_space::selector::Selector;
use cosmic_space::substance::SubstanceList;
use dashmap::mapref::one::Ref;
use dashmap::DashMap;
use std::marker::PhantomData;
use std::sync::atomic::AtomicU64;
use std::sync::{atomic, Arc};
use tokio::sync::oneshot;

impl MemRegCtx {
    pub fn new() -> Self {
        Self {
            sequence: Arc::new(AtomicU64::new(0u64)),
            particles: Arc::new(DashMap::new()),
            properties: Arc::new(DashMap::new()),
        }
    }
}

#[derive(Clone)]
pub struct MemRegCtx {
    pub sequence: Arc<AtomicU64>,
    pub particles: Arc<DashMap<Point, ParticleRecord>>,
    pub properties: Arc<DashMap<Point, Properties>>,
}

pub struct MemRegApi<C>
where
    C: Cosmos,
{
    ctx: MemRegCtx,
    phantom: PhantomData<C>,
}

impl<C> MemRegApi<C>
where
    C: Cosmos,
{
    pub fn new(ctx: MemRegCtx) -> Self {
        let phantom = Default::default();
        Self { phantom, ctx }
    }

    fn ctx(&self) -> &MemRegCtx {
        &self.ctx
    }
}

#[async_trait]
impl<C> RegistryApi<C> for MemRegApi<C>
where
    C: Cosmos,
{
    async fn nuke<'a>(&'a self) -> Result<(), C::Err> {
        todo!()
    }

    async fn register<'a>(&'a self, registration: &'a Registration) -> Result<(), C::Err> {
        self.set_properties(&registration.point, &registration.properties)
            .await?;

        let details = Details {
            stub: Stub {
                point: registration.point.clone(),
                kind: registration.kind.clone(),
                status: Status::Pending,
            },
            properties: self.get_properties(&registration.point).await?,
        };
        let record = ParticleRecord {
            details: details.clone(),
            location: ParticleLocation::default(),
        };
        self.ctx
            .particles
            .insert(registration.point.clone(), record);
        Ok(())
    }

    async fn assign_star<'a>(&'a self, point: &'a Point, star: &'a Point) -> Result<(), C::Err> {
        let mut record = self.ctx.particles.get_mut(&point).unwrap();
        record.value_mut().location.star = Some(star.clone());
        Ok(())
    }

    async fn assign_host<'a>(&'a self, point: &'a Point, host: &'a Point) -> Result<(), C::Err> {
        let mut record = self.ctx.particles.get_mut(&point).unwrap();
        record.value_mut().location.host = Some(host.clone());
        Ok(())
    }

    async fn set_status<'a>(&'a self, point: &'a Point, status: &'a Status) -> Result<(), C::Err> {
        let mut record = self
            .ctx
            .particles
            .get_mut(point)
            .ok_or(format!("not found: {}", point.to_string()))?;
        record.value_mut().details.stub.status = status.clone();
        Ok(())
    }

    async fn set_properties<'a>(
        &'a self,
        point: &'a Point,
        properties: &'a SetProperties,
    ) -> Result<(), C::Err> {
        let mut rtn = Properties::new();
        for (id, property) in properties.iter() {
            match property {
                PropertyMod::Set { key, value, lock } => {
                    let property = Property {
                        key: key.clone(),
                        value: value.clone(),
                        locked: lock.clone(),
                    };
                    rtn.insert(id.clone(), property);
                }
                PropertyMod::UnSet(_) => {}
            }
        }
        self.ctx.properties.insert(point.clone(), rtn);
        Ok(())
    }

    async fn sequence<'a>(&'a self, point: &'a Point) -> Result<u64, C::Err> {
        Ok(self.ctx.sequence.fetch_add(1, atomic::Ordering::Relaxed))
    }

    async fn get_properties<'a>(&'a self, point: &'a Point) -> Result<Properties, C::Err> {
        match self.ctx.properties.get(point) {
            None => Ok(Default::default()),
            Some(mul) => Ok(mul.value().clone()),
        }
    }

    async fn record<'a>(&'a self, point: &'a Point) -> Result<ParticleRecord, C::Err> {
        let properties = self.get_properties(point).await?;
        let mut record = self
            .ctx
            .particles
            .get(&point)
            .ok_or("not found")?
            .value()
            .clone();
        record.details.properties = properties;
        Ok(record)
    }

    async fn query<'a>(
        &'a self,
        point: &'a Point,
        query: &'a Query,
    ) -> Result<QueryResult, C::Err> {
        todo!()
    }

    async fn delete<'a>(&'a self, delete: &'a Delete) -> Result<SubstanceList, C::Err> {
        todo!()
    }

    async fn select<'a>(&'a self, select: &'a mut Select) -> Result<SubstanceList, C::Err> {
        todo!()
    }

    async fn sub_select<'a>(&'a self, sub_select: &'a SubSelect) -> Result<Vec<Stub>, C::Err> {
        todo!()
    }

    async fn grant<'a>(&'a self, access_grant: &'a AccessGrant) -> Result<(), C::Err> {
        todo!()
    }

    async fn access<'a>(&'a self, to: &'a Point, on: &'a Point) -> Result<Access, C::Err> {
        Ok(Access::Super)
    }

    async fn chown<'a>(
        &'a self,
        on: &'a Selector,
        owner: &'a Point,
        by: &'a Point,
    ) -> Result<(), C::Err> {
        todo!()
    }

    async fn list_access<'a>(
        &'a self,
        to: &'a Option<&'a Point>,
        on: &'a Selector,
    ) -> Result<Vec<IndexedAccessGrant>, C::Err> {
        todo!()
    }

    async fn remove_access<'a>(&'a self, id: i32, to: &'a Point) -> Result<(), C::Err> {
        todo!()
    }
}
