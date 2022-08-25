use std::marker::PhantomData;
use std::str::FromStr;
use crate::{DriverFactory, PlatErr, Platform, Registry};
use cosmic_api::cli::RawCommand;
use cosmic_api::command::command::common::StateSrc;
use cosmic_api::command::request::create::{Create, PointSegTemplate, Strategy};
use cosmic_api::command::Command;
use cosmic_api::error::MsgErr;
use cosmic_api::id::id::{Kind, Layer, Point, Port, ToPort, GLOBAL_EXEC, ToPoint};
use cosmic_api::log::{PointLogger, RootLogger};
use cosmic_api::parse::{bind_config, command_line};
use cosmic_api::parse::error::result;
use cosmic_api::particle::particle::{Details, Status};
use cosmic_api::util::{log, ToResolved};
use cosmic_api::wave::{
    Agent, DirectedHandlerShell, DirectedProto, Exchanger, Handling, InCtx, Pong, ProtoTransmitter,
    ProtoTransmitterBuilder, ReflectedCore, Router, Scope, SetStrategy, SysMethod, Wave,
};
use cosmic_api::{Registration, HYPERUSER, ArtRef};
use cosmic_nom::new_span;
use std::sync::Arc;

/*
#[derive(DirectedHandler,Clone)]
pub struct Global<P> where P: Platform {
    pub logger: PointLogger,
    pub registry: Registry<P>,
}

 */

use crate::driver::{Driver, DriverCtx, DriverSkel, DriverStatus, HyperDriverFactory, Item, ItemHandler, ItemSphere};
use crate::star::StarSkel;
use cosmic_api::config::config::bind::{BindConfig, RouteSelector};
use cosmic_api::parse::route_attribute;
use cosmic_api::substance::substance::Substance;
use cosmic_api::sys::{Assign, AssignmentKind, Sys};
use cosmic_api::wave::CoreBounce;
use cosmic_api::wave::DirectedHandler;
use cosmic_api::wave::DirectedHandlerSelector;
use cosmic_api::wave::RecipientSelector;
use cosmic_api::wave::RootInCtx;

lazy_static! {
    static ref GLOBAL_BIND_CONFIG: ArtRef<BindConfig> = ArtRef::new(
        Arc::new(global_bind()),
        Point::from_str("GLOBAL::repo:1.0.0:/bind/global.bind").unwrap()
    );
}


fn global_bind() -> BindConfig {
    log(bind_config(
        r#"
    Bind(version=1.0.0)
    {
       Route<Cmd<RawCommand>> -> (());
       Route<Cmd<Command>> -> (()) => &;
    }
    "#,
    ))
        .unwrap()

}

#[derive(Clone,DirectedHandler)]
pub struct GlobalCommandExecutionHandler<P>
where
    P: Platform,
{
    skel: StarSkel<P>,
}

impl <P> GlobalCommandExecutionHandler<P> where P: Platform {
    pub fn new(skel: StarSkel<P>) -> Self {
        Self {
            skel
        }
    }
}

#[routes]
impl<P> GlobalCommandExecutionHandler<P>
where
    P: Platform,
{
    #[route("Cmd<RawCommand>")]
    pub async fn raw(&self, ctx: InCtx<'_, RawCommand>) -> Result<ReflectedCore, P::Err> {
        let line = ctx.input.line.clone();
        let span = new_span(line.as_str());
        let command = log(result(command_line(span)))?;
        let command = command.collapse()?;
        let ctx = ctx.push_input_ref(&command);
        self.command(ctx).await
    }

    #[route("Cmd<Command>")]
    pub async fn command(&self, ctx: InCtx<'_, Command>) -> Result<ReflectedCore, P::Err> {
        let global = GlobalExecutionChamber::new(self.skel.clone() );
        let agent = ctx.wave().agent().clone();
        match ctx.input {
            Command::Create(create) => {
                Ok(ctx.ok_body(self.skel.logger.result(global.create(create,&agent).await)?.into()))
            },
            _ => Err(P::Err::new("not implemented")),
        }
    }
}

pub struct GlobalExecutionChamber<P>
where
    P: Platform,
{
    pub skel: StarSkel<P>,
    pub logger: PointLogger,
}

impl<P> GlobalExecutionChamber<P>
where
    P: Platform,
{
    pub fn new( skel: StarSkel<P>) -> Self {
        let logger = skel.logger.push_point("global").unwrap();
        Self {
            skel,
            logger
        }
    }

    #[track_caller]
    pub async fn create(&self, create: &Create, agent: &Agent) -> Result<Details, P::Err> {
        let child_kind = self
            .skel
            .machine
            .platform
            .select_kind(&create.template.kind).map_err(|err|P::Err::new(format!("Kind {} is not available on this Platform", create.template.kind.to_string())))?;
        let details = match &create.template.point.child_segment_template {
            PointSegTemplate::Exact(child_segment) => {
                let point = create.template.point.parent.push(child_segment.clone());
                match &point {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("RC CREATE error: {}", err.to_string());
                    }
                }
                let point = point?;

                let properties = self
                    .skel
                    .machine
                    .platform
                    .properties_config(&child_kind)
                    .fill_create_defaults(&create.properties)?;
                self.skel
                    .machine
                    .platform
                    .properties_config(&child_kind)
                    .check_create(&properties)?;

                let registration = Registration {
                    point: point.clone(),
                    kind: child_kind.clone(),
                    registry: Default::default(),
                    properties,
                    owner: agent.clone().to_point(),
                    strategy: create.strategy.clone(),
                    status: Status::Ready
                };
                let mut result = self.skel.registry.register(&registration).await;
                result?
            }
            PointSegTemplate::Pattern(pattern) => {
                if !pattern.contains("%") {
                    return Err(P::Err::status_msg(500u16, "AddressSegmentTemplate::Pattern must have at least one '%' char for substitution"));
                }
                    let index = self
                        .skel
                        .registry
                        .sequence(&create.template.point.parent)
                        .await?;
                    let child_segment = pattern.replace("%", index.to_string().as_str());
                    let point = create.template.point.parent.push(child_segment.clone())?;
                    let registration = Registration {
                        point: point.clone(),
                        kind: child_kind.clone(),
                        registry: Default::default(),
                        properties: create.properties.clone(),
                        owner: Point::root(),
                        strategy: create.strategy.clone(),
                        status: Status::Ready
                    };

                    self.skel.registry.register(&registration).await?
            }
        };

        Ok(details)
    }
}
