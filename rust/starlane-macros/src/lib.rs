use std::collections::{HashMap, HashSet};

use nom::bytes::complete::take_till1;
use nom::character::complete::anychar;
use nom::error::{context, VerboseError};
use nom::multi::many1;
use nom::sequence::tuple;
use nom::IResult;
use quote::__private::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{
    parse_macro_input, Expr, Ident, Item, ItemEnum, Lit, Meta, MetaList, MetaNameValue, NestedMeta,
    Path, PathArguments, Token, Type, Visibility,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

struct ResourceParser {
    pub resources: Vec<Resource>,
    pub kinds: Vec<ItemEnum>,
    pub ident_to_resource: HashMap<String, Resource>,
}

impl ResourceParser {
    pub fn children_of(&self, parent: Resource) -> Vec<Resource> {
        let mut rtn = vec![];
        for child in &self.resources {
            for parent_ident in child.parents.clone() {
                if parent.get_ident().to_string() == parent_ident.to_string() {
                    rtn.push(child.clone())
                }
            }
        }
        rtn
    }

    pub fn parents_of(&self, resource: Resource) -> Vec<Resource> {
        let mut rtn = vec![];
        for parent in &self.resources {
            for parent_ident in resource.parents.clone() {
                if parent.get_ident().to_string() == parent_ident.to_string() {
                    rtn.push(parent.clone())
                }
            }
        }
        rtn
    }

    pub fn build_paths(&self, resource: Resource) -> HashMap<String, Vec<String>> {
        let mut parts = vec![];
        parts.push(resource.path_part.as_ref().unwrap().to_string());
        let mut rtn = HashMap::new();

        for parent in self.parents_of(resource.clone()) {
            for path in self.paths(parent.clone(), parts.clone()) {
                rtn.insert(parent.get_ident().to_string(), path.clone());
            }
        }

        rtn
    }

    pub fn paths(&self, resource: Resource, mut parts: Vec<String>) -> HashSet<Vec<String>> {
        let mut rtn = HashSet::new();
        parts.push(resource.path_part.as_ref().unwrap().to_string());
        for parent in self.parents_of(resource.clone()) {
            for path in self.paths(parent, parts.clone()) {
                rtn.insert(path);
            }
        }

        if self.parents_of(resource).is_empty() {
            parts.reverse();
            rtn.insert(parts);
        }

        rtn
    }

    pub fn kind_for(&self, resource: &Resource) -> Option<ItemEnum> {
        for kind in &self.kinds {
            if kind.ident.to_string() == format!("{}Kind", resource.get_ident().to_string()) {
                println!("FOUND KIND MATCH");
                return Option::Some(kind.clone());
            }
        }
        Option::None
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
struct PathIdent {
    pub resource: String,
    pub parts: Vec<String>,
}

#[derive(Clone)]
struct Resource {
    item: Item,
    parents: Vec<Ident>,
    key_prefix: Option<String>,
    path_part: Option<Ident>,
    state_persistence: Option<Path>,
    state_aspects: HashMap<String, String>,
}

impl Resource {
    pub fn new(item: Item) -> Self {
        Self {
            item: item,
            parents: vec![],
            key_prefix: Option::None,
            path_part: Option::None,
            state_persistence: Option::None,
            state_aspects: HashMap::new(),
        }
    }

    pub fn get_ident(&self) -> Ident {
        match &self.item {
            Item::Struct(el_struct) => el_struct.ident.clone(),
            _ => {
                panic!("expected struct");
            }
        }
    }

    pub fn strip_resource_attributes(&mut self) {
        if let Item::Struct(e) = &mut self.item {
            e.attrs.retain(|attr| {
                if let Option::Some(seg) = attr.path.segments.last() {
                    if seg.ident.to_string() == "particle".to_string() {
                        false
                    } else {
                        true
                    }
                } else {
                    true
                }
            });
        }
    }
}

impl Parse for ResourceParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut kinds: Vec<ItemEnum> = vec![];
        let mut resources: Vec<Resource> = vec![];

        while !input.is_empty() {
            let item = input.parse::<Item>()?;
            if let Item::Struct(e) = &item {
                let mut resource = Resource::new(item.clone());
                for attr in &e.attrs {
                    if let Option::Some(seg) = attr.path.segments.last() {
                        if seg.ident.to_string() == "particle".to_string() {
                            let content: Meta = attr.parse_args()?;
                            match content {
                                Meta::Path(path) => {
                                    if path.segments.first().is_some()
                                        && path.segments.first().unwrap().ident.to_string().as_str()
                                            == "ResourcePathSegmentKind"
                                    {
                                        resource.path_part = Option::Some(
                                            path.segments.last().unwrap().ident.clone(),
                                        );
                                    } else if path.segments.first().is_some()
                                        && path.segments.first().unwrap().ident.to_string().as_str()
                                            == "ResourceStatePersistenceManager"
                                    {
                                        resource.state_persistence = Option::Some(path);
                                    }
                                }
                                Meta::List(list) => {
                                    match list
                                        .path
                                        .segments
                                        .last()
                                        .unwrap()
                                        .ident
                                        .to_string()
                                        .as_str()
                                    {
                                        "parents" => {
                                            resource.parents = to_idents(&list);
                                        }
                                        "state" => {
                                            for aspect in list.nested {
                                                if let NestedMeta::Meta(aspect) = aspect {
                                                    let name = aspect
                                                        .path()
                                                        .segments
                                                        .first()
                                                        .expect("expected a first")
                                                        .ident
                                                        .to_string();
                                                    let kind = aspect
                                                        .path()
                                                        .segments
                                                        .last()
                                                        .expect("expected a last")
                                                        .ident
                                                        .to_string();
                                                    println!("name & kind : {}: {} ", name, kind);
                                                    resource.state_aspects.insert(name, kind);
                                                }
                                            }
                                            //                                            particle.parents = to_idents(&list);
                                        }

                                        what => {
                                            panic!("unrecognized particle attribute '{}'", what);
                                        }
                                    }
                                }
                                Meta::NameValue(name_value) => {
                                    if name_value.path.is_ident("prefix") {
                                        if let Lit::Str(str) = name_value.lit {
                                            resource.key_prefix = Option::Some(str.value());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                resource.strip_resource_attributes();
                resources.push(resource);
            } else if let Item::Enum(e) = &item {
                if !e.ident.to_string().ends_with("Kind") {
                    panic!("only ResourceKinds can be defined here");
                }
                println!("ADDING KIND: {}", e.ident.to_string());
                kinds.push(e.clone());
            } else {
            }
        }

        let mut ident_to_resource = HashMap::new();
        for resource in &resources {
            ident_to_resource.insert(resource.get_ident().to_string(), resource.clone());
        }

        Ok(Self {
            kinds: kinds,
            resources: resources,
            ident_to_resource,
        })
    }
}

fn to_idents(list: &MetaList) -> Vec<Ident> {
    let mut idents = vec![];
    for parent in &list.nested {
        if let NestedMeta::Meta(parent) = parent {
            idents.push(parent.path().segments.last().unwrap().ident.clone());
        }
    }
    idents
}

#[proc_macro]
pub fn resources(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let parsed = parse_macro_input!(input as ResourceParser);

    let rts: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|resource| resource.get_ident())
        .collect();

    let requires_kind: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|resource| {
            Ident::new(
                format!("{}", parsed.kind_for(resource).is_some()).as_str(),
                resource.get_ident().span(),
            )
        })
        .collect();

    let resource_type_enum_def = quote! {
        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub enum ResourceType {
          Root,
          #(#rts),*
         }

        impl ResourceType {
            pub fn requires_kind(&self) -> bool {
                match self {
                    Self::Root => false,
                    #(Self::#rts => #requires_kind),*
                }
            }

            pub fn to_resource_id( &self, id: u64 ) -> ResourceId {
                match self {
                    Self::Root => ResourceId::Root,
                    #(Self::#rts => ResourceId::#rts(id as _)),*
                }
            }
        }

        impl ToString for ResourceType {
            fn to_string(&self) -> String {
                match self {
                    Self::Root => "Root".to_string(),
                    #(Self::#rts => stringify!(#rts).to_string() ),*
                }
            }
        }

        impl FromStr for ResourceType {
            type Err=Error;
            fn from_str( s: &str ) -> Result<Self,Error> {
                match s {
                    "Root" => Ok(Self::Root),
                    #(stringify!(#rts) => Ok(Self::#rts),)*
                    what => Err(format!("could not match ResourceType '{}'", what ).into())
                }
            }
        }

        impl TryFrom<ResourceKindParts> for ResourceType {
            type Error = Error;
            fn try_from(parts:ResourceKindParts) -> Result<Self,Self::Error> {
                Ok(ResourceType::from_str(parts.resource_type.as_str())?)
            }
        }
    };

    // the pathway method is kinda hacked using the number of segments to determine parent,
    // vector matchin doesn't actually seem to work as expected
    let mut pathways = String::new();
    pathways.push_str("impl ResourceType {");
    pathways.push_str("pub fn path_segment_kind( &self ) -> ResourcePathSegmentKind {");

    pathways.push_str("match self {");
    pathways.push_str("Self::Root => ResourcePathSegmentKind::Root,");
    for resource in &parsed.resources {
        let _ident = resource.get_ident();
        pathways.push_str(format!("Self::{} => {{", resource.get_ident().to_string()).as_str());
        pathways.push_str("ResourcePathSegmentKind::");
        pathways.push_str(
            resource
                .path_part
                .as_ref()
                .expect("expected a path part")
                .to_string()
                .as_str(),
        );
        pathways.push_str("}");
    }
    pathways.push_str("}");

    pathways.push_str("}");
    pathways.push_str("pub fn parent_path_matcher( &self, path: Vec<ResourcePathSegmentKind> ) -> Result<ResourceType,Error> {");
    pathways.push_str("match self { ");
    pathways.push_str("Self::Root => Err(\"Root does not have a parent to match\".into()),");

    /*
    let mut state_schema= String::new();
    state_schema.push_str("impl ResourceType {");
    state_schema.push_str("pub fn state_schema( &self ) -> StateSchema {");
    state_schema.push_str("match self { ");
    state_schema.push_str("Self::Root => StateSchema::new(),");
     */

    for resource in &parsed.resources {
        let _ident = resource.get_ident();
        pathways.push_str(format!("Self::{} => {{", resource.get_ident().to_string()).as_str());

        for (parent, path) in parsed.build_paths(resource.clone()).iter() {
            pathways.push_str(format!("let {} = vec![", parent.to_lowercase()).as_str());
            for part in path {
                pathways.push_str("ResourcePathSegmentKind::");
                pathways.push_str(part);
                pathways.push_str(",");
            }
            pathways.push_str("];");
        }
        pathways.push_str("match (path.len(), path) {");
        for (parent, path) in parsed.build_paths(resource.clone()).iter() {
            pathways
                .push_str(format!("({}, {}) => {{", path.len(), parent.to_lowercase()).as_str());
            pathways.push_str(format!("Ok(Self::{})", parent).as_str());
            pathways.push_str("}");
        }

        pathways
            .push_str("(_,_) => Err(\"could not find parent match for particle pathway\".into())");

        pathways.push_str("}");

        pathways.push_str("},");

        /*
        state_schema.push_str( format!("Self::{}=>{{", particle.get_ident().to_string()).as_str() );
        state_schema.push_str( "let mut state_schema = StateSchema::new();" );
        for (key,value) in &particle.state_aspects {
            state_schema.push_str( format!("state_schema.insert( \"{}\".to_string(), DataAspectKind::{} );", key, value).as_str() );
        }
        state_schema.push_str( "state_schema" );
        state_schema.push_str( "}");

         */
    }

    pathways.push_str("}}}");
    //state_schema.push_str( "}}}");
    //println!("{}",pathways);
    let pathways = syn::parse_str::<Item>(pathways.as_str()).unwrap();
    let pathways = quote! {#pathways};

    /*
    let state_schema = syn::parse_str::<Item>( state_schema.as_str() ).unwrap();
    let state_schema = quote!{#state_schema};
     */

    let state_persistences: Vec<Path> = parsed
        .resources
        .clone()
        .iter()
        .map(|resource| {
            resource
                .state_persistence
                .as_ref()
                .expect("expected ResourceStatePersistenceManager to be set")
                .clone()
        })
        .collect();

    let idents: Vec<Ident> = parsed
        .resources
        .clone()
        .iter()
        .map(|resource| resource.get_ident())
        .collect();

    let parents: Vec<Vec<Ident>> = parsed
        .resources
        .iter()
        .map(|resource| resource.parents.clone())
        .collect();

    let resource_impl_def = quote! {
    impl ResourceType {
       pub fn parents(&self) -> Vec<Self> {
          match self {
            Self::Root => vec!(),
            #(Self::#idents => vec![#(Self::#parents),*]),*
          }
       }

      pub fn state_persistence(&self) -> ResourceStatePersistenceManager {
          match self {
                        Self::Root => ResourceStatePersistenceManager::None,
                        #(Self::#idents => #state_persistences),*
         }
       }
    }
    #pathways
        };

    let resources_def: Vec<Item> = parsed
        .resources
        .clone()
        .iter()
        .map(|resource| resource.item.clone())
        .collect();

    println!("resources_def.len() {}", resources_def.len());

    let keys = keys(&parsed);
    //    let identifiers = identifiers(&parsed);
    let ids = ids(&parsed);

    let kinds = kinds(&parsed);

    //    let paths = paths(&parsed);
    /*
    proc_macro::TokenStream::from( quote!{
       #extras
       #resource_type_enum_def
       #resource_impl_def
       #(#resources_def)*
       #keys

       #paths
       #keys
    })
     */

    proc_macro::TokenStream::from(quote! {
       #resource_type_enum_def
       #(#resources_def)*
       #ids
       #kinds
       #keys

       #resource_impl_def
    })
}
/*
fn paths(parsed: &ResourceParser) -> TokenStream {
    let mut paths = vec![];
    for particle in &parsed.resources {
        let _ident = Ident::new(
            particle.get_ident().to_string().as_str(),
            particle.get_ident().span(),
        );
        let path_ident = Ident::new(
            format!("{}Path", particle.get_ident().to_string()).as_str(),
            particle.get_ident().span(),
        );

        if particle.parents.len() == 1 {
            let parent = particle.parents.first().unwrap().clone();
            let parent_path = Ident::new(
                format!("{}Path", parent.to_string()).as_str(),
                parent.span(),
            );
            paths.push( quote!{
                 impl #path_ident {
                    pub fn parent(&self)->ResourcePath {
                        let mut parts = self.parts.clone();
                        parts.remove( parts.len()-1 );
                        #parent_path::try_from(parts).expect("expected it to parse since it had already parsed once").into()
                    }
                }
            } );
        } else {
            let _parents = particle.parents.clone();
            let _parent_path: Vec<Ident> = particle
                .parents
                .iter()
                .map(|parent| {
                    Ident::new(
                        format!("{}Path", parent.to_string()).as_str(),
                        parent.span(),
                    )
                })
                .collect();
            paths.push( quote!{
                 impl #path_ident {
                    pub fn parent(&self)->ResourcePath {
                        let parent_resource_type = self.resource_type().parent_path_matcher(self.parts.iter().map(|p|p.clone().to_kind()).collect()).expect("expected to find a parent match");
                        ResourcePath::from_parts_and_type(parent_resource_type, self.parts.clone() ).unwrap()
                    }
                }
            } );
        }
    }

    let idents: Vec<Ident> = parsed
        .resources
        .clone()
        .iter()
        .map(|particle| particle.get_ident())
        .collect();
    let idents2 = idents.clone();

    let path_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|particle| {
            Ident::new(
                format!("{}Path", particle.get_ident().to_string()).as_str(),
                particle.get_ident().span(),
            )
        })
        .collect();

    let path_idents2 = path_idents.clone();
    let path_idents3 = path_idents.clone();
    let path_idents4 = path_idents.clone();
    let path_idents5 = path_idents.clone();

    quote! {

        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub struct RootPath{

        }

        impl TryFrom<Vec<ResourcePathSegment>> for RootPath{
            type Error=Error;
            fn try_from( parts: Vec<ResourcePathSegment> ) -> Result<RootPath,Self::Error> {
                if parts.is_empty() {
                    Ok(RootPath{})
                } else {
                    Err("root path should not have remaining parts".into())
                }
            }
        }

        #(
            #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
            pub struct #path_idents {
            parts: Vec<ResourcePathSegment>
        }

        impl ToString for #path_idents2 {
           fn to_string(&self) -> String {
               let path: ResourcePath = self.clone().into();
               path.to_string()
           }
        }

        impl #path_idents2 {
           pub fn resource_type(&self) -> ResourceType {
               ResourceType::#idents
           }

           pub fn name(&self) -> String {
                self.parts.last().expect("expected last()").to_string()
           }
        }

          impl Into<Vec<ResourcePathSegment>> for #path_idents3 {
                fn into(self) -> Vec<ResourcePathSegment> {
                  self.parts
                }
          }

            impl Into<ResourceIdentifier> for #path_idents{
                fn into(self) -> ResourceIdentifier {
                    let path: ResourcePath = self.into();
                    path.into()
                }
            }

            impl TryFrom<ResourceIdentifier> for #path_idents {
                type Error=Error;
                fn try_from( identifier: ResourceIdentifier) -> Result<Self,Self::Error> {
                    if let ResourceIdentifier::Address( address ) = identifier {
                        address.try_into()
                    } else {
                        Err("cannot convert a key into an address".into())
                    }
                }
            }

           impl Into<ResourceAddress> for #path_idents {
                fn into(self) -> ResourceAddress{
                    ResourceAddress {
                        path: ResourcePath::#idents(self)
                    }
                }
            }

          impl TryFrom<ResourceAddress> for #path_idents3 {
                type Error=Error;
                fn try_from( path: ResourceAddress ) -> Result<Self,Self::Error> {
                    let ResourceAddress { path } = path;
                    path.try_into()
                }
            }

          impl TryFrom<Vec<ResourcePathSegment>> for #path_idents4 {
               type Error=Error;

               fn try_from(parts: Vec<ResourcePathSegment> )->Result<Self,Self::Error>{
                    Ok(Self{
                        parts: parts
                    })
               }
            }

         impl FromStr for #path_idents5 {
               type Err=Error;
               fn from_str( s: &str ) -> Result<Self,Self::Err> {
                    let (leftover,parts):(&str,Vec<ResourcePathSegment>) = parse_resource_path(s)?;
                    if leftover.len() > 0 {
                        Err(format!("tried to parse #path_idents5 for '{}' but ran into leftover '{}'", s, leftover).into() )
                    } else {
                        Ok(parts.try_into()?)
                    }
               }
         }

        )*

        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub enum ResourcePath {
            Root,
            #(#idents(#path_idents)),*
        }

        impl ResourcePath {
            pub fn resource_type(&self) -> ResourceType {
                match self {
                 Self::Root => ResourceType::Root,
                 #(Self::#idents(_)=>ResourceType::#idents2),*
                }
            }
        }


        impl ToString for ResourcePath {

            fn to_string(&self)->String {
                let segments:Vec<ResourcePathSegment> = self.clone().into();
                let mut rtn = String::new();
                for i in 0..segments.len() {
                    let segment = segments.get(i).unwrap();
                    rtn.push_str( segment.to_string().as_str() );
                    if i < segments.len()-1 {
                        rtn.push_str(":");
                    }
                }
                rtn
            }
        }

        impl Into<ResourceIdentifier> for ResourcePath {
           fn into(self) -> ResourceIdentifier{
                ResourceAddress{
                    path: self
                }.into()
            }
        }

        impl Into<Vec<ResourcePathSegment>> for ResourcePath {
            fn into(self) -> Vec<ResourcePathSegment> {
                match self {
                    Self::Root => vec![],
                    #(Self::#idents(path)=>path.into()),*
                }
            }
        }


        impl FromStr for ResourcePath {
            type Err=Error;

            fn from_str(s: &str) -> Result<Self,Self::Err>{
                let (leftover,(path,kind)) = parse_path(s)?;
                if leftover.len() > 0 {
                    return Err(format!("cannot process: '{}' from path '{}'", leftover, s).into());
                }
                let resource_type:ResourceType= kind.try_into()?;

                match resource_type {
                    ResourceType::Root => {
                        if !path.is_empty()  {
                            Err("root path must be empty".into())
                        } else {
                            Ok(ResourcePath::Root)
                        }
                    }
                    #(ResourceType::#idents=>Ok(#path_idents::try_from(path)?.into())),*
                }
            }

        }


        impl ResourcePath {
            pub fn from_parts_and_type( resource_type:ResourceType, parts: Vec<ResourcePathSegment> ) -> Result<ResourcePath,Error> {
               match  resource_type {
                    ResourceType::Root => Ok(ResourcePath::Root),
                    #(ResourceType::#idents => Ok(#path_idents::try_from(parts.clone())?.into()) ),*
               }
            }

            pub fn parent(&self)->Option<ResourcePath>{
                match self {
                     Self::Root => Option::None,
                     #(Self::#idents(path) => Option::Some(path.parent().into()) ),*
                 }
            }

            pub fn name(&self)->String{
                match self {
                     Self::Root => "".to_string(),
                     #(Self::#idents(path) => path.name() ),*
                 }
            }

        }

        #(#paths)*
    }
    //#(ResourceType::#idents => Ok(Option::Some(#path_idents::parent()?.unwrap().into())) ),*
}

 */

fn kinds(parsed: &ResourceParser) -> TokenStream {
    let mut kind_stuff = vec![];

    let mut resource_kind_enum = String::new();
    resource_kind_enum.push_str("#[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]");
    resource_kind_enum.push_str("pub enum ResourceKind {");
    resource_kind_enum.push_str("Root,");

    let mut resource_kind_from_parts = String::new();
    resource_kind_from_parts.push_str("impl TryFrom<ResourceKindParts> for ResourceKind {");
    resource_kind_from_parts.push_str("type Error = Error;");
    resource_kind_from_parts
        .push_str("fn try_from( parts: ResourceKindParts ) -> Result<Self,Self::Error> { ");
    resource_kind_from_parts.push_str("match parts.resource_type.as_str() { ");
    resource_kind_from_parts.push_str("\"Root\" => Ok(Self::Root),");

    let mut into_resource_kind_parts = String::new();
    into_resource_kind_parts.push_str("impl Into<ResourceKindParts> for ResourceKind {");
    into_resource_kind_parts.push_str("fn into(self) -> ResourceKindParts {");
    into_resource_kind_parts.push_str("match self {");
    into_resource_kind_parts.push_str( "Self::Root => ResourceKindParts{ resource_type: \"Root\".to_string(), kind:Option::None, specific:Option::None },");

    let mut resource_type = String::new();
    resource_type.push_str("impl ResourceKind {");
    resource_type.push_str("pub fn resource_type(&self) -> ResourceType {");
    resource_type.push_str("match self {");
    resource_type.push_str("Self::Root => ResourceType::Root,");

    for resource in &parsed.resources {
        if let Option::Some(kind) = parsed.kind_for(resource) {
            let _ident = resource.get_ident();
            let ident_kind = Ident::new(
                format!("{}Kind", resource.get_ident().to_string()).as_str(),
                resource.get_ident().span(),
            );
            kind_stuff.push(quote! {
                impl ToString for #ident_kind {
                    fn to_string(&self) -> String {
                        let kind: ResourceKind = self.clone().into();
                        kind.to_string()
                    }
                }
            });

            resource_kind_enum.push_str(
                format!(
                    "{}({}),",
                    resource.get_ident().to_string(),
                    kind.ident.to_string()
                )
                .as_str(),
            );
            resource_kind_from_parts.push_str(
                format!(
                    "\"{}\"=>Ok({}Kind::try_from(parts)?.into()),",
                    resource.get_ident().to_string(),
                    resource.get_ident().to_string()
                )
                .as_str(),
            );
            into_resource_kind_parts.push_str(
                format!(
                    "Self::{}(kind)=>kind.into(),",
                    resource.get_ident().to_string()
                )
                .as_str(),
            );
            resource_type.push_str(
                format!(
                    "Self::{}(_)=>ResourceType::{},",
                    resource.get_ident().to_string(),
                    resource.get_ident().to_string()
                )
                .as_str(),
            );
            let kind_cp = kind.clone();
            kind_stuff.push(quote! {#kind_cp});

            let kind_ident = kind.ident.clone();
            let resource_ident = resource.get_ident();

            let mut variants = vec![];
            let mut get_specific = String::new();
            get_specific.push_str(format!("impl {} {}", kind.ident.to_string(), "{").as_str());
            get_specific.push_str("pub fn get_specific(&self)->Option<Specific> {");
            get_specific.push_str("match self {");

            let mut has_specific = String::new();
            has_specific.push_str(format!("impl {} {}", kind.ident.to_string(), "{").as_str());
            has_specific.push_str("pub fn has_specific(&self)->bool {");
            has_specific.push_str("match self {");

            let mut from_parts = String::new();
            from_parts.push_str(
                format!(
                    "impl TryFrom<ResourceKindParts> for {} {{",
                    kind.ident.to_string()
                )
                .as_str(),
            );
            from_parts.push_str("type Error=Error;");
            from_parts.push_str("fn try_from(parts: ResourceKindParts)->Result<Self,Self::Error>{");
            if parsed.kind_for(resource).is_some() {
                from_parts.push_str(format!("match parts.kind.ok_or(\"kind must be specified for {}. i.e.: <Type<Kind>>\")?.as_str() {{", resource.get_ident().to_string()).as_str());
            } else {
                from_parts.push_str(format!("if parts.kind.is_some() {{  return Err(\"particle type <{}> does not have a kind\".into());}}", resource.get_ident().to_string() ).as_str() );

                from_parts.push_str(format!("Self::{}", resource.get_ident().to_string()).as_str());
            }

            let mut into_parts = String::new();
            into_parts.push_str(
                format!(
                    "impl Into<ResourceKindParts> for {} {{",
                    kind.ident.to_string()
                )
                .as_str(),
            );
            into_parts.push_str("fn into(self)->ResourceKindParts {");
            into_parts.push_str("match self {");

            for variant in &kind.variants {
                variants.push(variant.ident.clone());
                has_specific.push_str(format!("Self::{}", variant.ident.to_string()).as_str());
                if !variant.fields.is_empty() {
                    has_specific.push_str("(_)=>true,");
                } else {
                    has_specific.push_str("=>false,");
                }

                get_specific.push_str(format!("Self::{}", variant.ident.to_string()).as_str());
                if !variant.fields.is_empty() {
                    get_specific.push_str("(specific)=>Option::Some(specific.clone()),");
                } else {
                    get_specific.push_str("=>Option::None,");
                }

                if !variant.fields.is_empty() {
                    from_parts.push_str(format!("\"{}\" => Ok(Self::{}(parts.specific.ok_or(\"<{}> Kind expected a Specific: <Type<Kind<Specific>>>\")?)),", variant.ident.to_string(), variant.ident.to_string(), resource.get_ident().to_string(), ).as_str());
                } else {
                    from_parts.push_str(
                        format!(
                            "\"{}\" => Ok(Self::{}),",
                            variant.ident.to_string(),
                            variant.ident.to_string()
                        )
                        .as_str(),
                    );
                }

                if !variant.fields.is_empty() {
                    into_parts.push_str(format!("Self::{}(specific)=>ResourceKindParts{{resource_type:\"{}\".to_string(),kind:Option::Some(\"{}\".to_string()),specific:Option::Some(specific)}},",variant.ident.to_string(),resource.get_ident().to_string(),variant.ident.to_string(),).as_str() );
                } else {
                    into_parts.push_str(format!("Self::{}=>ResourceKindParts{{resource_type:\"{}\".to_string(),kind:Option::Some(\"{}\".to_string()),specific:Option::None}},",variant.ident.to_string(),resource.get_ident().to_string(),variant.ident.to_string(),).as_str() );
                }
            }
            has_specific.push_str("}}}");
            get_specific.push_str("}}}");
            from_parts.push_str("_ => Err(\"could not match kind\".into())");
            from_parts.push_str("}}}");
            into_parts.push_str("}}}");

            let has_specific = syn::parse_str::<Item>(has_specific.as_str()).unwrap();
            kind_stuff.push(quote! {#has_specific});

            let get_specific = syn::parse_str::<Item>(get_specific.as_str()).unwrap();
            kind_stuff.push(quote! {#get_specific});

            let from_parts = syn::parse_str::<Item>(from_parts.as_str()).unwrap();
            kind_stuff.push(quote! {#from_parts});

            let into_parts = syn::parse_str::<Item>(into_parts.as_str()).unwrap();
            kind_stuff.push(quote! {#into_parts});

            kind_stuff.push(quote! {

              impl #kind_ident{
                  pub fn resource_type(&self) -> ResourceType {
                      ResourceType::#resource_ident
                  }
              }


              impl Into<ResourceKind> for #kind_ident {
                    fn into(self) -> ResourceKind {
                        ResourceKind::#resource_ident(self)
                    }
                }

              impl TryInto<#kind_ident> for ResourceKind {
                    type Error=Error;
                    fn try_into(self) -> Result<#kind_ident,Self::Error>{
                        if let Self::#resource_ident(rtn) = self {
                            Ok(rtn)
                        } else {
                            Err("could not convert".into())
                        }
                    }
                }

            });
        } else {
            resource_kind_enum.push_str(format!("{},", resource.get_ident().to_string()).as_str());
            resource_kind_from_parts.push_str(
                format!(
                    "\"{}\"=>Ok(Self::{}),",
                    resource.get_ident().to_string(),
                    resource.get_ident().to_string()
                )
                .as_str(),
            );
            into_resource_kind_parts.push_str(format!("Self::{}=>ResourceKindParts{{resource_type:\"{}\".to_string(),kind:Option::None,specific:Option::None}},",resource.get_ident().to_string(),resource.get_ident().to_string()).as_str() );
            resource_type.push_str(
                format!(
                    "Self::{}=>ResourceType::{},",
                    resource.get_ident().to_string(),
                    resource.get_ident().to_string()
                )
                .as_str(),
            );
        }
    }
    resource_kind_enum.push_str("}");
    resource_kind_from_parts
        .push_str("what => Err(format!(\"cannot identify ResourceType: '{}'\",what).into())");
    resource_kind_from_parts.push_str("}}}");
    into_resource_kind_parts.push_str("}}}");
    resource_type.push_str("}}}");

    let resource_kind_emum = syn::parse_str::<Item>(resource_kind_enum.as_str()).unwrap();
    kind_stuff.push(quote! {#resource_kind_emum});

    let resource_kind_from_parts =
        syn::parse_str::<Item>(resource_kind_from_parts.as_str()).unwrap();
    kind_stuff.push(quote! {#resource_kind_from_parts});

    let into_resource_kind_parts =
        syn::parse_str::<Item>(into_resource_kind_parts.as_str()).unwrap();
    kind_stuff.push(quote! {#into_resource_kind_parts});

    let resource_type = syn::parse_str::<Item>(resource_type.as_str()).unwrap();
    kind_stuff.push(quote! {#resource_type});

    let rtn = quote! {
        #(#kind_stuff)*

        impl ToString for ResourceKind {
            fn to_string(&self) -> String {
                let parts: ResourceKindParts = self.clone().into();
                parts.to_string()
            }
        }


        impl FromStr for ResourceKind {
            type Err=Error;

            fn from_str(s: &str) -> Result<Self,Self::Err>{
                let parts = ResourceKindParts::from_str(s)?;
                Self::try_from(parts)
            }
        }
    };
    rtn
}

fn keys(parsed: &ResourceParser) -> TokenStream {
    let mut key_stuff = vec![];
    for resource in &parsed.resources {
        let ident = Ident::new(
            format!("{}Key", resource.get_ident().to_string()).as_str(),
            resource.get_ident().span(),
        );
        let id = Ident::new(
            format!("{}Id", resource.get_ident().to_string()).as_str(),
            resource.get_ident().span(),
        );
        let resource_ident = resource.get_ident();

        let key = if resource.parents.is_empty() {
            quote! {

                #[derive(Clone,Eq,PartialEq,Hash,Serialize,Deserialize,Debug)]
                pub struct #ident {
                    id: #id
                }

                impl #ident{
                    pub fn bin(&self) -> Result<Vec<u8>, Error> {
                        let mut bin = bincode::serialize(self)?;
                        Ok(bin)
                    }

                    pub fn from_bin(mut bin: Vec<u8>) -> Result<Self, Error> {
                        let mut key = bincode::deserialize::<Self>(bin.as_slice())?;
                        Ok(key)
                    }
                }

                impl #ident {
                    pub fn parent(&self) -> Option<ResourceKey> {
                        Option::None
                    }
               }
            }
        } else {
            let parent = if resource.parents.len() > 1 {
                let parent = Ident::new(
                    format!("{}ParentKey", resource.get_ident().to_string()).as_str(),
                    resource.get_ident().span(),
                );
                let parents = resource.parents.clone();
                let parents2 = resource.parents.clone();
                let mut parent_keys = vec![];
                let mut parent_x_parents = vec![];
                for p in &resource.parents {
                    parent_keys.push(Ident::new(
                        format!("{}Key", p.to_string()).as_str(),
                        p.span(),
                    ));
                    parent_x_parents.push(parent.clone());
                }

                key_stuff.push(quote! {

                    #[derive(Clone,Eq,PartialEq,Hash,Serialize,Deserialize,Debug)]
                    pub enum #parent {
                        #(#parents(#parent_keys)),*
                    }




                    impl #parent {


                        pub fn string_bit(&self) -> String {
                             match self {
                                #(Self::#parents(key)=>key.string_bit()),*
                             }
                        }

                        pub fn string_prefix(&self) -> String {
                             match self {
                                #(Self::#parents(key)=>key.string_prefix()),*
                             }
                        }

                    }

                    impl Into<ResourceKey> for #parent {
                        fn into(self)->ResourceKey {
                            match self {
                              #(Self::#parents(key)=>key.into()),*
                            }
                        }
                    }

                    impl TryInto<#parent> for ResourceKey {
                        type Error=Error;
                        fn try_into(self)->Result<#parent,Self::Error> {
                            match self {
                              #(Self::#parents(key)=>Ok(#parent::#parents2(key)),)*
                              _ => Err("no match".into())
                            }
                        }
                    }


                    #(
                       impl Into<#parent_x_parents> for #parent_keys {
                            fn into(self) -> #parent_x_parents {
                                #parent_x_parents::#parents(self)
                            }
                       }
                    )*

                });

                parent
            } else {
                let parent = resource.parents.last().unwrap();
                Ident::new(format!("{}Key", parent.to_string()).as_str(), parent.span())
            };

            let prefix = Ident::new(
                resource.key_prefix.as_ref().unwrap().clone().as_str(),
                resource.get_ident().span(),
            );
            quote! {

                #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
                pub struct #ident {
                    parent: #parent,
                    id: #id
                }

                impl #ident {
                    pub fn bin(&self) -> Result<Vec<u8>, Error> {
                        let mut bin = bincode::serialize(self)?;
                        Ok(bin)
                    }

                    pub fn from_bin(mut bin: Vec<u8>) -> Result<Self, Error> {
                        let mut key = bincode::deserialize::<Self>(bin.as_slice())?;
                        Ok(key)
                    }

                  fn from_keybit( parent: #parent, key_bit: KeyBit ) -> Result<Self,Error> {
                       if key_bit.key_type.as_str() != stringify!(#prefix) {
                          return Err(format!("cannot create '{}' from keybit: '{}'",key_bit.key_type.as_str(),stringify!(#prefix)).into())
                       }
                       Ok(Self {
                         parent: parent,
                         id: key_bit.id as u64
                       })
                    }

                    pub fn new( parent: #parent, id: #id ) -> Self {
                        Self {
                            parent: parent,
                            id: id
                        }
                    }

                    pub fn parent(&self) -> Option<ResourceKey> {
                        Option::Some(self.parent.clone().into())
                    }


                }

            }
        };
        key_stuff.push(key);

        //COMMON KEY
        let prefix: TokenStream = resource
            .key_prefix
            .as_ref()
            .expect("expected key prefix")
            .clone()
            .parse()
            .unwrap();
        let ident = Ident::new(
            format!("{}Key", resource.get_ident().to_string()).as_str(),
            resource.get_ident().span(),
        );
        let ident_lower = Ident::new(
            resource.get_ident().to_string().to_lowercase().as_str(),
            resource.get_ident().span(),
        );
        let resource = resource.get_ident();
        key_stuff.push(quote! {

            impl FromStr for #ident {
                type Err=Error;
                fn from_str( s: &str ) -> Result<Self,Self::Err> {
                    Ok(ResourceKey::from_str(s)?.try_into()?)
                }
            }


            impl #ident {
                pub fn string_bit(&self) -> String {
                    format!("{}{}",stringify!(#prefix),self.id.to_string())
                }

                pub fn string_prefix(&self) -> String {
                   stringify!(#prefix).to_string()
                }

                pub fn generate_address_tail(&self) -> String {
                   format!( "{}-{}", stringify!(#ident_lower), self.id )
                }
            }

            impl ToString for #ident{
                fn to_string(&self) -> String {
                    let rtn:ResourceKey = self.clone().into();
                    rtn.to_string()
                }
            }

            impl Into<ResourceKey> for #ident {
                fn into(self) -> ResourceKey {
                    ResourceKey::#resource(self)
                }
            }

            impl TryInto<#ident> for ResourceKey {
                type Error=Error;
                fn try_into(self)->Result<#ident,Self::Error> {
                     if let Self::#resource_ident(key) = self {
                          Ok(key)
                     } else {
                          Err(format!("cannot convert to {}", stringify!(ident)).into())
                     }
                }
            }

        });
    }

    let mut idents = vec![];
    let mut idents_keys = vec![];
    let mut prefixes = vec![];
    for resource in &parsed.resources {
        idents.push(resource.get_ident());
        idents_keys.push(Ident::new(
            format!("{}Key", resource.get_ident().to_string()).as_str(),
            resource.get_ident().span(),
        ));
        prefixes.push(Ident::new(
            resource.key_prefix.as_ref().unwrap().clone().as_str(),
            resource.get_ident().span(),
        ));
    }

    let rtn = quote! {
    #(#key_stuff)*

        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub struct RootKey();

        impl RootKey {
            pub fn new()->Self {
                Self()
            }
        }

        impl Into<ResourceKey> for RootKey {
            fn into(self) -> ResourceKey {
                ResourceKey::Root
            }
        }

        impl TryInto<RootKey> for ResourceKey {
            type Error=Error;
            fn try_into(self) -> Result<RootKey,Error> {
                if let Self::Root=self {
                    Ok(RootKey())
                } else {
                    Err("not an instance of a RootKey".into())
                }
            }
        }

        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub enum ResourceKey {
            Root,
            #(#idents(#idents_keys)),*
        }

         impl ResourceKey{

            pub fn new( parent: ResourceKey, id: ResourceId ) -> Result<Self,Error>{
                match id {
                    ResourceId::Root => Err("root cannot have a parent".into()),
                    #(ResourceId::#idents(id) => Ok( #idents_keys{ parent: parent.try_into()?, id: id  }.into()) ),*
                }
            }


            pub fn bin(&self) -> Result<Vec<u8>, Error> {
                let mut bin = bincode::serialize(self)?;
                Ok(bin)
            }

            pub fn from_bin(mut bin: Vec<u8>) -> Result<Self, Error> {
                let mut key = bincode::deserialize::<Self>(bin.as_slice())?;
                Ok(key)
            }
         }


        impl ResourceKey {

            pub fn root() -> Self {
                Self::Root
            }

            pub fn resource_type(&self)  -> ResourceType {
                match self {
                    Self::Root => ResourceType::Root,
                    #(Self::#idents(_)=>ResourceType::#idents),*
                }
            }

            pub fn parent(&self)->Option<ResourceKey> {
                match self {
                    #(Self::#idents(key) => key.parent(),)*
                    Root => Option::None
                }
            }

            pub fn generate_address_tail( &self ) -> String {
                  match self {
                    #(Self::#idents(key) => key.generate_address_tail(), )*
                    Root => "root".to_string()
                }
            }

            pub fn string_bit(&self) -> String {
                 match self {
                    #(Self::#idents(key) => key.string_bit(), )*
                    Root => "".to_string()
                }
            }

            pub fn string_prefix(&self) -> String {
                 match self {
                    #(Self::#idents(key) => key.string_prefix(), )*
                    Root => "".to_string()
                }
            }

            pub fn ancestors(&self) -> Vec<ResourceKey> {
                let mut rtn = vec![];
                let mut ancestor = self.clone();
                while let Option::Some(parent) = ancestor.parent() {
                   rtn.push( parent.clone() );
                   ancestor = parent.clone();
                }
                rtn
            }

            pub fn ancestors_not_root(&self) -> Vec<ResourceKey> {
                let mut rtn = vec![];
                let mut ancestor = self.clone();
                while let Option::Some(parent) = ancestor.parent() {
                   if parent.parent().is_some() {
                      rtn.push( parent.clone() );
                   }
                   ancestor = parent.clone();
                }
                rtn
            }

            fn from_keybit( parent: ResourceKey, key_bit: KeyBit ) -> Result<ResourceKey,Error> {
                match key_bit.key_type.as_str() {

                    #(stringify!(#prefixes) => {
                        Ok(#idents_keys::from_keybit(parent.try_into()?, key_bit )?.into())
                    } ,)*
                    _ => Err("unrecognized keybit".into())
                }
            }

            pub fn to_snake_case(&self) -> String {
                self.to_string().replace(":", "_")
            }

            pub fn to_skewer_case(&self) -> String {
                self.to_string().replace(":", "-")
            }
        }

        impl ToString for ResourceKey {
            fn to_string(&self) -> String {

                if let Self::Root = self {
                    return "root".to_string();
                }


                let mut ancestors = self.ancestors_not_root();
                ancestors.reverse();
                ancestors.push(self.clone());

                let mut rtn = String::new();
                for i in 0..ancestors.len() {
                    let ancestor = ancestors.get(i).unwrap();
                    rtn.push_str(ancestor.string_bit().as_str());
                    if i < ancestors.len()-1 {
                      rtn.push_str(":");
                    }
                }
                rtn
            }
        }

        impl FromStr for ResourceKey {
            type Err=Error;
            fn from_str( s: &str ) -> Result<Self,Self::Err> {
                let (leftover,keybits) = KeyBits::parse_key_bits(s)?;
                if leftover.len() > 0 {
                    Err(format!("leftover '{}' when trying to parse ResourceKey '{}'",leftover,s).into())
                } else {
                    let mut key = ResourceKey::Root;
                    for bit in keybits {
                        key = Self::from_keybit(key, bit)?;
                    }
                    Ok(key)
                }
            }
        }


    };
    rtn
}

fn extras() -> TokenStream {
    quote! {

    pub struct Error {
        message: String
    }




        }
}

/*
fn identifiers(parsed: &ResourceParser) -> TokenStream {
    let _idents: Vec<Ident> = parsed
        .resources
        .clone()
        .iter()
        .map(|particle| particle.get_ident())
        .collect();
    let identifier_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|particle| {
            Ident::new(
                format!("{}Identifier", particle.get_ident().to_string()).as_str(),
                particle.get_ident().span(),
            )
        })
        .collect();

    let key_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|particle| {
            Ident::new(
                format!("{}Key", particle.get_ident().to_string()).as_str(),
                particle.get_ident().span(),
            )
        })
        .collect();

    let path_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|particle| {
            Ident::new(
                format!("{}Path", particle.get_ident().to_string()).as_str(),
                particle.get_ident().span(),
            )
        })
        .collect();

    quote! {
        #(
            pub enum #identifier_idents {
                Address(#path_idents),
                Key(#key_idents)
            }


            impl Into<ResourceIdentifier> for #identifier_idents {
                fn into(self) -> ResourceIdentifier {
                    match self {
                        Self::Address(address) => ResourceIdentifier::Address( ResourceAddress{path:address.into()}),
                        Self::Key(key) => ResourceIdentifier::Key( key.into() ),
                    }
                }
            }

            impl From<#path_idents> for #identifier_idents {
                fn from(path: #path_idents) -> #identifier_idents{
                        Self::Address(path)
                }
            }

            impl From<#key_idents> for #identifier_idents {
                fn from(key: #key_idents) -> #identifier_idents{
                        Self::Key(key)
                }
            }

            impl TryFrom<ResourceIdentifier> for #identifier_idents {
                type Error=Error;
                fn try_from(id:ResourceIdentifier) -> Result<Self,Self::Error>{
                    match id{
                        ResourceIdentifier::Address(address) => Ok(Self::Address( address.try_into()? )),
                        ResourceIdentifier::Key(key) => Ok(Self::Key( key.try_into()? )),
                    }
                }
            }


        )*

    }
}
 */

fn ids(parsed: &ResourceParser) -> TokenStream {
    let idents: Vec<Ident> = parsed
        .resources
        .clone()
        .iter()
        .map(|resource| resource.get_ident())
        .collect();
    let ids: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|resource| {
            Ident::new(
                format!("{}Id", resource.get_ident().to_string()).as_str(),
                resource.get_ident().span(),
            )
        })
        .collect();

    let _key_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|resource| {
            Ident::new(
                format!("{}Key", resource.get_ident().to_string()).as_str(),
                resource.get_ident().span(),
            )
        })
        .collect();

    let _path_idents: Vec<Ident> = parsed
        .resources
        .iter()
        .map(|resource| {
            Ident::new(
                format!("{}Path", resource.get_ident().to_string()).as_str(),
                resource.get_ident().span(),
            )
        })
        .collect();

    quote! {
        #(
            pub type #ids = u64;
        )*



        impl ResourceId {
            pub fn resource_type(&self) -> ResourceType {
                match self {
                    Self::Root => ResourceType::Root,
                    #(Self::#idents(_) => ResourceType::#idents),*
                }
            }
        }

        #[derive(Clone,Debug,Eq,PartialEq,Hash,Serialize,Deserialize)]
        pub enum ResourceId {
            Root,
            #(#idents(#ids)),*
        }

        impl ToString for ResourceId {
            fn to_string(&self) -> String {
                 match self {
                    Self::Root => "root".to_string(),
                    #(Self::#idents(id) => id.to_string()),*
                }
            }
        }

    }
}

fn is_uppercase(a: char) -> bool {
    (a as char).is_uppercase()
}

fn parse_camel(input: &str) -> Res<&str, Vec<String>> {
    context("camel", many1(tuple((anychar, take_till1(is_uppercase)))))(input).map(
        |(input, vec)| {
            let vec: Vec<String> = vec
                .iter()
                .map(|(c, s): &(char, &str)| format!("{}{}", c, s))
                .collect();
            (input, vec)
        },
    )
}

#[cfg(test)]
mod tests {
    use crate::parse_camel;

    #[test]
    fn camel() {
        let (leftover, rtn) = parse_camel("RomulanVarool").unwrap();

        assert!(leftover.is_empty());
        if let Option::Some(romulan) = rtn.get(0).cloned() {
            assert_eq!("Romulan".to_string(), romulan);
        } else {
            assert!(false)
        }

        assert!(leftover.is_empty());
        if let Option::Some(romulan) = rtn.get(1).cloned() {
            assert_eq!("Varool".to_string(), romulan);
        } else {
            assert!(false)
        }
    }
}
