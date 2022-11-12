use core::str::FromStr;

use nom::combinator::all_consuming;
use serde::{Deserialize, Serialize};

use cosmic_macros_primitive::Autobox;
use cosmic_nom::{new_span, Trace};
use direct::create::{Create, CreateCtx, CreateVar};
use direct::delete::{DeleteCtx, DeleteVar};
use direct::get::{Get, GetCtx, GetVar};
use direct::read::{Read, ReadCtx, ReadVar};
use direct::select::{SelectCtx, SelectVar};
use direct::set::{Set, SetCtx, SetVar};
use direct::write::{Write, WriteCtx, WriteVar};

use crate::parse::error::result;
use crate::parse::command_line;
use crate::substance::{Bin, ChildSubstance};
use crate::util::ToResolved;
use crate::wave::core::cmd::CmdMethod;
use crate::{Delete, Select, SpaceErr};
use crate::model::Env;

pub mod common {
    use std::collections::hash_map::Iter;
    use std::collections::HashMap;
    use std::convert::{TryFrom, TryInto};
    use std::ops::{Deref, DerefMut};

    use serde::{Deserialize, Serialize};

    use crate::err::SpaceErr;
    use crate::point::Variable;
    use crate::parse::model::Var;
    use crate::substance::{Substance, SubstanceMap};

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum StateSrcVar {
        None,
        FileRef(String),
        Var(Variable),
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum StateSrc {
        None,
        Substance(Box<Substance>),
    }

    impl StateSrc {
        pub fn has_substance(&self) -> bool {
            match self {
                StateSrc::None => false,
                StateSrc::Substance(_) => true,
            }
        }

        pub fn get_substance(&self) -> Result<Substance, SpaceErr> {
            match self {
                StateSrc::None => Err(SpaceErr::server_error("state has no substance")),
                StateSrc::Substance(substance) => Ok(*substance.clone()),
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub enum PropertyMod {
        Set {
            key: String,
            value: String,
            lock: bool,
        },
        UnSet(String),
    }

    impl PropertyMod {
        pub fn set_or<E>(&self, err: E) -> Result<String, E> {
            match self {
                Self::Set { key, value, lock } => Ok(value.clone()),
                Self::UnSet(_) => Err(err),
            }
        }

        pub fn opt(&self) -> Option<String> {
            match self {
                Self::Set { key, value, lock } => Some(value.clone()),
                Self::UnSet(_) => None,
            }
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct SetProperties {
        pub map: HashMap<String, PropertyMod>,
    }

    impl Default for SetProperties {
        fn default() -> Self {
            Self {
                map: Default::default(),
            }
        }
    }

    impl SetProperties {
        pub fn new() -> Self {
            Self {
                map: HashMap::new(),
            }
        }

        pub fn append(&mut self, properties: SetProperties) {
            for (_, property) in properties.map.into_iter() {
                self.push(property);
            }
        }

        pub fn push(&mut self, property: PropertyMod) {
            match &property {
                PropertyMod::Set { key, value, lock } => {
                    self.map.insert(key.clone(), property);
                }
                PropertyMod::UnSet(key) => {
                    self.map.insert(key.clone(), property);
                }
            }
        }

        pub fn iter(&self) -> Iter<'_, String, PropertyMod> {
            self.map.iter()
        }
    }

    impl Deref for SetProperties {
        type Target = HashMap<String, PropertyMod>;

        fn deref(&self) -> &Self::Target {
            &self.map
        }
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
    pub enum SetLabel {
        Set(String),
        SetValue { key: String, value: String },
        Unset(String),
    }

    #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
    pub struct SetRegistry {
        pub labels: Vec<SetLabel>,
    }

    impl Deref for SetRegistry {
        type Target = Vec<SetLabel>;

        fn deref(&self) -> &Self::Target {
            &self.labels
        }
    }

    impl DerefMut for SetRegistry {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.labels
        }
    }

    impl Default for SetRegistry {
        fn default() -> Self {
            Self {
                labels: Default::default(),
            }
        }
    }
}

pub mod direct {
    //    use http::status::InvalidStatusCode;
    //    use http::{HeaderMap, Request, StatusCode, Uri};
    use serde::{Deserialize, Serialize};

    use crate::command::direct::create::Create;
    use crate::command::direct::get::Get;
    use crate::command::direct::select::Select;
    use crate::command::direct::set::Set;
    use crate::command::direct::write::Write;
    use crate::err::SpaceErr;
    use crate::fail;
    use crate::fail::{BadRequest, Fail, NotFound};
    use crate::point::{Meta, Point};
    use crate::substance::Bin;
    use crate::substance::{FormErrs, Substance};
    use crate::util::{ValueMatcher, ValuePattern};
    use crate::wave::core::ext::ExtMethod;
    use crate::wave::core::http2::HttpMethod;
    use crate::wave::core::MethodKind;
    use crate::wave::core::ReflectedCore;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub enum Cmd {
        Create(Create),
        Select(Select),
        Update(Write),
        Get(Get),
        Set(Set),
    }

    impl PartialEq<Self> for Cmd {
        fn eq(&self, other: &Self) -> bool {
            self.get_type() == other.get_type()
        }
    }

    impl Eq for Cmd {}

    impl Cmd {
        pub fn get_type(&self) -> CmdKind {
            match self {
                Cmd::Create(_) => CmdKind::Create,
                Cmd::Select(_) => CmdKind::Select,
                Cmd::Update(_) => CmdKind::Update,
                Cmd::Get(_) => CmdKind::Get,
                Cmd::Set(_) => CmdKind::Set,
            }
        }
    }

    #[derive(
        Debug,
        Clone,
        Eq,
        PartialEq,
        strum_macros::Display,
        strum_macros::EnumString,
        Serialize,
        Deserialize,
    )]
    pub enum CmdKind {
        Create,
        Select,
        Update,
        Query,
        Get,
        Set,
    }

    impl ValueMatcher<Cmd> for Cmd {
        fn is_match(&self, x: &Cmd) -> Result<(), ()> {
            if self.get_type() == x.get_type() {
                Ok(())
            } else {
                Err(())
            }
        }
    }

    impl ToString for Cmd {
        fn to_string(&self) -> String {
            format!("Rc<{}>", self.get_type().to_string())
        }
    }

    pub mod set {
        use serde::{Deserialize, Serialize};

        use crate::command::common::SetProperties;
        use crate::err::SpaceErr;
        use crate::point::{Point, PointCtx, PointVar};
        use crate::model::Env;
        use crate::util::ToResolved;

        pub type Set = SetDef<Point>;
        pub type SetCtx = SetDef<PointCtx>;
        pub type SetVar = SetDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct SetDef<Pnt> {
            pub point: Pnt,
            pub properties: SetProperties,
        }

        impl ToResolved<Set> for SetVar {
            fn to_resolved(self, env: &Env) -> Result<Set, SpaceErr> {
                let set: SetCtx = self.to_resolved(env)?;
                set.to_resolved(env)
            }
        }

        impl ToResolved<SetCtx> for SetVar {
            fn to_resolved(self, env: &Env) -> Result<SetCtx, SpaceErr> {
                Ok(SetCtx {
                    point: self.point.to_resolved(env)?,
                    properties: self.properties,
                })
            }
        }

        impl ToResolved<Set> for SetCtx {
            fn to_resolved(self, env: &Env) -> Result<Set, SpaceErr> {
                Ok(Set {
                    point: self.point.to_resolved(env)?,
                    properties: self.properties,
                })
            }
        }
    }

    pub mod get {
        use serde::{Deserialize, Serialize};

        use crate::command::common::SetProperties;
        use crate::err::SpaceErr;
        use crate::point::{Point, PointCtx, PointVar};
        use crate::model::Env;
        use crate::util::ToResolved;

        pub type Get = GetDef<Point>;
        pub type GetCtx = GetDef<PointCtx>;
        pub type GetVar = GetDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct GetDef<Pnt> {
            pub point: Pnt,
            pub op: GetOp,
        }

        impl ToResolved<Get> for GetVar {
            fn to_resolved(self, env: &Env) -> Result<Get, SpaceErr> {
                let set: GetCtx = self.to_resolved(env)?;
                set.to_resolved(env)
            }
        }

        impl ToResolved<GetCtx> for GetVar {
            fn to_resolved(self, env: &Env) -> Result<GetCtx, SpaceErr> {
                Ok(GetCtx {
                    point: self.point.to_resolved(env)?,
                    op: self.op,
                })
            }
        }

        impl ToResolved<Get> for GetCtx {
            fn to_resolved(self, env: &Env) -> Result<Get, SpaceErr> {
                Ok(Get {
                    point: self.point.to_resolved(env)?,
                    op: self.op,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum GetOp {
            State,
            Properties(Vec<String>),
        }
    }

    pub mod create {
        use std::convert::TryInto;
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::sync::Arc;

        use serde::{Deserialize, Serialize};
        use tokio::sync::Mutex;

        use crate::command::common::{SetProperties, SetRegistry, StateSrc, StateSrcVar};
        use crate::command::Command;
        use crate::err::SpaceErr;
        use crate::kind::ProtoKindSelector;
        use crate::point::{HostKey, Point, PointCtx, PointFactory, PointSeg, PointVar, ToSurface};
        use crate::model::{CamelCase, Env};
        use crate::parse::model::Subst;
        use crate::parse::ResolverErr;
        use crate::selector::SpecificSelector;
        use crate::substance::Bin;
        use crate::substance::Substance;
        use crate::util::{ConvertFrom, ToResolved};
        use crate::wave::core::cmd::CmdMethod;
        use crate::wave::core::ext::ExtMethod;
        use crate::wave::core::hyp::HypMethod;
        use crate::wave::core::DirectedCore;
        use crate::wave::{DirectedProto, Ping, Wave};

        pub enum PointTemplateSeg {
            ExactSeg(PointSeg),
            Wildcard(String),
        }

        impl PointTemplateSeg {
            pub fn is_wildcard(&self) -> bool {
                match self {
                    PointTemplateSeg::ExactSeg(_) => false,
                    PointTemplateSeg::Wildcard(_) => true,
                }
            }
        }

        pub type Template = TemplateDef<PointTemplate>;
        pub type TemplateCtx = TemplateDef<PointTemplateCtx>;
        pub type TemplateVar = TemplateDef<PointTemplateVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct TemplateDef<Pnt> {
            pub point: Pnt,
            pub kind: ProtoKindSelector,
        }

        impl ToResolved<Template> for TemplateVar {
            fn to_resolved(self, env: &Env) -> Result<Template, SpaceErr> {
                let template: TemplateCtx = self.to_resolved(env)?;
                template.to_resolved(env)
            }
        }

        impl ToResolved<TemplateCtx> for TemplateVar {
            fn to_resolved(self, env: &Env) -> Result<TemplateCtx, SpaceErr> {
                let point: PointTemplateCtx = self.point.to_resolved(env)?;

                let template = TemplateCtx {
                    point,
                    kind: self.kind,
                };
                Ok(template)
            }
        }
        impl ToResolved<Template> for TemplateCtx {
            fn to_resolved(self, env: &Env) -> Result<Template, SpaceErr> {
                let point = self.point.to_resolved(env)?;

                let template = Template {
                    point,
                    kind: self.kind,
                };
                Ok(template)
            }
        }

        impl Template {
            pub fn new(point: PointTemplate, kind: ProtoKindSelector) -> Self {
                Self { point, kind }
            }
        }


        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Require {
            File(String),
            Auth(String),
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum Fulfillment {
            File { name: String, content: Bin },
            Complete,
        }

        pub type Create = CreateDef<Point, StateSrc>;
        pub type CreateVar = CreateDef<PointVar, StateSrcVar>;
        pub type CreateCtx = CreateDef<PointCtx, StateSrc>;

        impl ToResolved<Create> for CreateVar {
            fn to_resolved(self, env: &Env) -> Result<Create, SpaceErr> {
                let create: CreateCtx = self.to_resolved(env)?;
                create.to_resolved(env)
            }
        }

        impl ToResolved<CreateCtx> for CreateVar {
            fn to_resolved(self, env: &Env) -> Result<CreateCtx, SpaceErr> {
                let template = self.template.to_resolved(env)?;
                let state = match &self.state {
                    StateSrcVar::None => StateSrc::None,
                    StateSrcVar::FileRef(name) => StateSrc::Substance(Box::new(Substance::Bin(
                        env.file(name)
                            .map_err(|e| match e {
                                ResolverErr::NotAvailable => SpaceErr::server_error(
                                    "files are not available in this context",
                                ),
                                ResolverErr::NotFound => {
                                    SpaceErr::server_error(format!("cannot find file '{}'", name))
                                }
                            })?
                            .content,
                    ))),
                    StateSrcVar::Var(var) => {
                        let val = env.val(var.name.as_str()).map_err(|e| match e {
                            ResolverErr::NotAvailable => {
                                SpaceErr::server_error("variable are not available in this context")
                            }
                            ResolverErr::NotFound => SpaceErr::server_error(format!(
                                "cannot find variable '{}'",
                                var.name
                            )),
                        })?;
                        StateSrc::Substance(Box::new(Substance::Bin(
                            env.file(val.clone())
                                .map_err(|e| match e {
                                    ResolverErr::NotAvailable => SpaceErr::server_error(
                                        "files are not available in this context",
                                    ),
                                    ResolverErr::NotFound => SpaceErr::server_error(format!(
                                        "cannot find file '{}'",
                                        val.to_text().unwrap_or("err".to_string())
                                    )),
                                })?
                                .content,
                        )))
                    }
                };
                Ok(CreateCtx {
                    template,
                    properties: self.properties,
                    strategy: self.strategy,
                    state,
                })
            }
        }

        impl ToResolved<Create> for CreateCtx {
            fn to_resolved(self, env: &Env) -> Result<Create, SpaceErr> {
                let template = self.template.to_resolved(env)?;
                Ok(Create {
                    template,
                    properties: self.properties,
                    strategy: self.strategy,
                    state: self.state,
                })
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct CreateDef<Pnt, StateSrc> {
            pub template: TemplateDef<PointTemplateDef<Pnt>>,
            pub properties: SetProperties,
            pub strategy: Strategy,
            pub state: StateSrc,
        }

        impl Create {
            pub fn fulfillment(mut self, bin: Bin) -> Create {
                Create {
                    template: self.template,
                    state: StateSrc::Substance(Box::new(Substance::Bin(bin))),
                    properties: self.properties,
                    strategy: self.strategy,
                }
            }

            pub fn to_wave_proto(self) -> DirectedProto {
                let mut wave = DirectedProto::ping();
                wave.method(CmdMethod::Command);
                wave.body(Substance::Command(Box::new(Command::Create(self))));
                wave.to(Point::global_executor());
                wave
            }
        }

        impl Into<DirectedCore> for Create {
            fn into(self) -> DirectedCore {
                let mut directed = DirectedCore::ext(ExtMethod::new("Command").unwrap());
                directed.body = Substance::Command(Box::new(Command::Create(self)));
                directed
            }
        }

        impl Into<DirectedProto> for Create {
            fn into(self) -> DirectedProto {
                let mut request =
                    DirectedProto::cmd(Point::global_executor().to_surface(), CmdMethod::Command);
                request.body(Substance::Command(Box::new(Command::Create(self))));
                request
            }
        }

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, strum_macros::Display)]
        pub enum Strategy {
            Commit,
            Ensure,
            Override,
        }

        pub struct PointFactoryU64 {
            parent: Point,
            prefix: String,
            atomic: Arc<AtomicU64>,
        }

        impl PointFactoryU64 {
            pub fn new(parent: Point, prefix: String) -> Self {
                Self {
                    parent,
                    prefix,
                    atomic: Arc::new(AtomicU64::new(0)),
                }
            }
        }

        #[async_trait]
        impl PointFactory for PointFactoryU64 {
            async fn create(&self) -> Result<Point, SpaceErr> {
                let index = self.atomic.fetch_add(1u64, Ordering::Relaxed);
                self.parent.push(format!("{}{}", self.prefix, index))
            }
        }

        pub type PointTemplate = PointTemplateDef<Point>;
        pub type PointTemplateCtx = PointTemplateDef<PointCtx>;
        pub type PointTemplateVar = PointTemplateDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct PointTemplateDef<Pnt> {
            pub parent: Pnt,
            pub child_segment_template: PointSegTemplate,
        }

        impl ToResolved<PointTemplateCtx> for PointTemplateVar {
            fn to_resolved(self, env: &Env) -> Result<PointTemplateCtx, SpaceErr> {
                let parent = self.parent.to_resolved(env)?;
                Ok(PointTemplateCtx {
                    parent,
                    child_segment_template: self.child_segment_template,
                })
            }
        }

        impl ToResolved<PointTemplate> for PointTemplateCtx {
            fn to_resolved(self, env: &Env) -> Result<PointTemplate, SpaceErr> {
                let parent = self.parent.to_resolved(env)?;
                Ok(PointTemplate {
                    parent,
                    child_segment_template: self.child_segment_template,
                })
            }
        }

        impl ToResolved<PointTemplate> for PointTemplateVar {
            fn to_resolved(self, env: &Env) -> Result<PointTemplate, SpaceErr> {
                let ctx: PointTemplateCtx = self.to_resolved(env)?;
                ctx.to_resolved(env)
            }
        }

        #[derive(Debug, Clone, strum_macros::Display, Serialize, Deserialize, Eq, PartialEq)]
        pub enum PointSegTemplate {
            Exact(String),
            Pattern(String), // must have a '%'
            Root,
        }
    }

    pub mod select {
        use std::collections::{HashMap, HashSet};
        use std::convert::{TryFrom, TryInto};
        use std::marker::PhantomData;

        use serde::{Deserialize, Serialize};

        use crate::err::SpaceErr;
        use crate::fail::{BadCoercion, Fail};
        use crate::point::Point;
        use crate::model::Env;
        use crate::particle::Stub;
        use crate::selector::{PointHierarchy, PointSelector, PointSelectorCtx, PointSelectorVar, SelectorDef};
        use crate::substance::{MapPattern, Substance, SubstanceList};
        use crate::util::{ConvertFrom, ToResolved};

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum SelectIntoSubstance {
            Stubs,
            Points,
        }

        impl SelectIntoSubstance {
            pub fn to_primitive(&self, stubs: Vec<Stub>) -> Result<SubstanceList, SpaceErr> {
                match self {
                    SelectIntoSubstance::Stubs => {
                        let stubs: Vec<Box<Substance>> = stubs
                            .into_iter()
                            .map(|stub| Box::new(Substance::Stub(stub)))
                            .collect();
                        let stubs = SubstanceList { list: stubs };
                        Ok(stubs)
                    }
                    SelectIntoSubstance::Points => {
                        let pointes: Vec<Box<Substance>> = stubs
                            .into_iter()
                            .map(|stub| Box::new(Substance::Point(stub.point)))
                            .collect();
                        let stubs = SubstanceList { list: pointes };
                        Ok(stubs)
                    }
                }
            }
        }

        pub type Select = SelectDef<PointSelector>;
        pub type SelectCtx = SelectDef<PointSelectorCtx>;
        pub type SelectVar = SelectDef<PointSelectorVar>;

        impl ToResolved<Select> for Select {
            fn to_resolved(self, env: &Env) -> Result<Select, SpaceErr> {
                Ok(self)
            }
        }

         impl ToResolved<Select> for SelectCtx {
            fn to_resolved(self, env: &Env) -> Result<Select, SpaceErr> {
                todo!()
            }
        }

        impl ToResolved<SelectCtx> for SelectVar {
            fn to_resolved(self, env: &Env) -> Result<SelectCtx, SpaceErr> {
                todo!()
            }
        }


        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct SelectDef<S> {
            pub pattern: S,
            pub properties: PropertiesPattern,
            pub into_substance: SelectIntoSubstance,
        }





        impl Select {
            pub fn new(pattern: PointSelector) -> Self {
                Self {
                    pattern,
                    properties: Default::default(),
                    into_substance: SelectIntoSubstance::Stubs,
                }
            }
        }

        pub type PropertiesPattern = MapPattern;
    }

    pub mod delete {
        use serde::{Deserialize, Serialize};

        use crate::command::direct::select::{PropertiesPattern, Select, SelectIntoSubstance};
        use crate::err::SpaceErr;
        use crate::model::Env;
        use crate::selector::{PointSelector, PointSelectorCtx, PointSelectorVar, SelectorDef};
        use crate::util::ToResolved;

        pub type Delete = DeleteDef<PointSelector>;
        pub type DeleteCtx = DeleteDef<PointSelectorCtx>;
        pub type DeleteVar = DeleteDef<PointSelectorVar>;

        impl ToResolved<Delete> for Delete {
            fn to_resolved(self, env: &Env) -> Result<Delete, SpaceErr> {
                Ok(self)
            }
        }

        impl ToResolved<Delete> for DeleteCtx {
            fn to_resolved(self, env: &Env) -> Result<Delete, SpaceErr> {
                todo!()
            }
        }


        impl ToResolved<DeleteCtx> for DeleteVar {
            fn to_resolved(self, env: &Env) -> Result<DeleteCtx, SpaceErr> {
                todo!()
            }
        }


        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct DeleteDef<Selector> {
            pub selector: Selector
        }

        impl Into<Select> for Delete {
            fn into(self) -> Select {
                let mut select = Select::new(self.selector);
                select.into_substance = SelectIntoSubstance::Points;
                select
            }
        }
    }

    pub mod write {
        use std::convert::TryInto;

        use serde::{Deserialize, Serialize};

        use crate::command::common::SetProperties;
        use crate::err::SpaceErr;
        use crate::point::{Point, PointCtx, PointVar};
        use crate::model::Env;
        use crate::substance::Substance;
        use crate::util::ToResolved;

        pub type Write = WriteDef<Point>;
        pub type WriteCtx = WriteDef<PointCtx>;
        pub type WriteVar = WriteDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct WriteDef<Pnt> {
            pub point: Pnt,
            pub payload: Substance,
        }

        impl ToResolved<WriteCtx> for WriteVar {
            fn to_resolved(self, env: &Env) -> Result<WriteCtx, SpaceErr> {
                Ok(WriteCtx {
                    point: self.point.to_resolved(env)?,
                    payload: self.payload,
                })
            }
        }

        impl ToResolved<Write> for WriteCtx {
            fn to_resolved(self, env: &Env) -> Result<Write, SpaceErr> {
                Ok(Write {
                    point: self.point.to_resolved(env)?,
                    payload: self.payload,
                })
            }
        }
    }

    pub mod read {
        use serde::{Deserialize, Serialize};

        use crate::err::SpaceErr;
        use crate::point::{Point, PointCtx, PointVar};
        use crate::model::Env;
        use crate::substance::Substance;
        use crate::util::ToResolved;

        pub type Read = ReadDef<Point>;
        pub type ReadCtx = ReadDef<PointCtx>;
        pub type ReadVar = ReadDef<PointVar>;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub struct ReadDef<Pnt> {
            pub point: Pnt,
            pub payload: Substance,
        }

        impl ToResolved<ReadCtx> for ReadVar {
            fn to_resolved(self, env: &Env) -> Result<ReadCtx, SpaceErr> {
                Ok(ReadCtx {
                    point: self.point.to_resolved(env)?,
                    payload: self.payload,
                })
            }
        }

        impl ToResolved<Read> for ReadCtx {
            fn to_resolved(self, env: &Env) -> Result<Read, SpaceErr> {
                Ok(Read {
                    point: self.point.to_resolved(env)?,
                    payload: self.payload,
                })
            }
        }
    }

    pub mod query {
        use std::convert::TryInto;

        use serde::{Deserialize, Serialize};

        use crate::command::direct::Cmd;
        use crate::err::SpaceErr;
        use crate::selector::PointHierarchy;

        #[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
        pub enum Query {
            PointHierarchy,
        }

        #[derive(Debug, Clone, Serialize, Deserialize)]
        pub enum QueryResult {
            PointHierarchy(PointHierarchy),
        }

        impl TryInto<PointHierarchy> for QueryResult {
            type Error = SpaceErr;

            fn try_into(self) -> Result<PointHierarchy, SpaceErr> {
                match self {
                    QueryResult::PointHierarchy(hierarchy) => Ok(hierarchy),
                }
            }
        }

        impl ToString for QueryResult {
            fn to_string(&self) -> String {
                match self {
                    QueryResult::PointHierarchy(hierarchy) => hierarchy.to_string(),
                }
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Autobox)]
pub enum Command {
    Create(Create),
    Delete(Delete),
    Select(Select),
    Set(Set),
    Get(Get),
    Write(Write),
    Read(Read),
}

impl ChildSubstance for Command {}

impl Command {
    pub fn matches(&self, method: &CmdMethod) -> Result<(), ()> {
        if match self {
            Command::Write(_) => *method == CmdMethod::Update,
            Command::Read(_) => *method == CmdMethod::Read,
            _ => false,
        } {
            Ok(())
        } else {
            Err(())
        }
    }
}

pub enum CommandCtx {
    Create(CreateCtx),
    Delete(DeleteCtx),
    Select(SelectCtx),
    Set(SetCtx),
    Get(GetCtx),
    Update(WriteCtx),
    Read(ReadCtx),
}

pub enum CommandVar {
    Create(CreateVar),
    Delete(DeleteVar),
    Select(SelectVar),
    Set(SetVar),
    Get(GetVar),
    Update(WriteVar),
    Read(ReadVar),
}

impl FromStr for CommandVar {
    type Err = SpaceErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = new_span(s);
        result(all_consuming(command_line)(s))
    }
}

impl ToResolved<Command> for CommandVar {
    fn to_resolved(self, env: &Env) -> Result<Command, SpaceErr> {
        let command: CommandCtx = self.to_resolved(env)?;
        command.to_resolved(env)
    }
}

impl ToResolved<CommandCtx> for CommandVar {
    fn to_resolved(self, env: &Env) -> Result<CommandCtx, SpaceErr> {
        Ok(match self {
            CommandVar::Create(i) => CommandCtx::Create(i.to_resolved(env)?),
            CommandVar::Select(i) => CommandCtx::Select(i.to_resolved(env)?),
            CommandVar::Set(i) => CommandCtx::Set(i.to_resolved(env)?),
            CommandVar::Get(i) => CommandCtx::Get(i.to_resolved(env)?),
            CommandVar::Delete(i) => CommandCtx::Delete(i.to_resolved(env)?),
            CommandVar::Update(update) => CommandCtx::Update(update.to_resolved(env)?),
            CommandVar::Read(read) => CommandCtx::Read(read.to_resolved(env)?),
        })
    }
}

impl ToResolved<Command> for CommandCtx {
    fn to_resolved(self, env: &Env) -> Result<Command, SpaceErr> {
        Ok(match self {
            CommandCtx::Create(i) => Command::Create(i.to_resolved(env)?),
            CommandCtx::Select(i) => Command::Select(i.to_resolved(env)?),
            CommandCtx::Set(i) => Command::Set(i.to_resolved(env)?),
            CommandCtx::Get(i) => Command::Get(i.to_resolved(env)?),
            CommandCtx::Delete(i) => Command::Delete(i.to_resolved(env)?),
            CommandCtx::Update(update) => Command::Write(update.to_resolved(env)?),
            CommandCtx::Read(read) => Command::Read(read.to_resolved(env)?),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandTemplate {
    pub line: String,
    pub transfers: Vec<Trace>,
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct RawCommand {
    pub line: String,
    pub transfers: Vec<CmdTransfer>,
}

impl RawCommand {
    pub fn new<S>(line: S) -> Self
    where
        S: ToString,
    {
        Self {
            line: line.to_string(),
            transfers: vec![],
        }
    }
}

impl ToString for RawCommand {
    fn to_string(&self) -> String {
        self.line.clone()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct CmdTransfer {
    pub id: String,
    pub content: Bin,
}

impl CmdTransfer {
    pub fn new<N: ToString>(id: N, content: Bin) -> Self {
        Self {
            id: id.to_string(),
            content,
        }
    }
}
