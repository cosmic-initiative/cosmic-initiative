#![crate_type = "lib"]
#![allow(warnings)]

#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate strum_macros;

use proc_macro::TokenStream;
use std::str::FromStr;

use chrono::{DateTime, Utc};
use cosmic_space::point;
use nom::combinator::into;
use nom_locate::LocatedSpan;
use proc_macro2::Ident;
use quote::__private::ext::RepToTokensExt;
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use regex::Regex;
use syn::__private::TokenStream2;
use syn::parse::{Parse, ParseBuffer, ParseStream};
use syn::parse_quote::ParseQuote;
use syn::spanned::Spanned;
use syn::token::Async;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Attribute, Data, DataEnum, DataUnion,
    DeriveInput, FieldsNamed, FieldsUnnamed, FnArg, GenericArgument, ImplItem, ItemImpl,
    ItemStruct, PathArguments, PathSegment, ReturnType, Signature, Type, Visibility,
};

use cosmic_space::parse::route_attribute_value;
use cosmic_space::util::log;
use cosmic_space::wasm::Timestamp;

#[no_mangle]
extern "C" fn cosmic_uuid() -> point::Uuid {
    point::Uuid::from(uuid::Uuid::new_v4().to_string()).unwrap()
}

#[no_mangle]
extern "C" fn cosmic_timestamp() -> Timestamp {
    Timestamp::new(Utc::now().timestamp_millis())
}

/// This macro will auto implement the `cosmic_space::wave::exchange::asynch::DirectedHandler` trait.
/// In order to finalize the implementation a `#[handler]` attribute must also be specified
/// above one of the impls.
#[proc_macro_derive(DirectedHandler)]
pub fn directed_handler(item: TokenStream) -> TokenStream {
    TokenStream::from(quote! {})
}

/// This impl attribute creates a `fn handler` which receives directed waves and routes them to the first local method
/// which the route selector matches.
/// To implement:
/// ```
///
/// use cosmic_space::err::SpaceErr;
/// use cosmic_space::hyper::HyperSubstance;
/// use cosmic_space::log::PointLogger;
/// use cosmic_space::substance::Substance;
/// use cosmic_space::substance::Substance::Text;
/// use cosmic_space::wave::core::ReflectedCore;
/// use cosmic_space::wave::exchange::asynch::InCtx;
///
/// #[derive(DirectedHandler)]
/// pub struct MyHandler {
///   logger: PointLogger
/// }
///
/// #[handler]
/// impl MyHandler {
///     /// the route attribute captures an ExtMethod implementing a custom `MyNameIs`
///     /// notice that the InCtx will accept any valid cosmic_space::substance::Substance
///     #[route("Ext<MyNameIs>")]
///     pub async fn hello(&self, ctx: InCtx<'_, Text>) -> Result<String, SpaceErr> {
///         /// also we can return any Substance in our Reflected wave
///         Ok(format!("Hello, {}", ctx.input.to_string()))
///     }
///
///     /// if the function returns nothing then an Empty Ok Reflected will be returned unless
///     /// the wave type is `Wave<Signal>`
///     #[route("Ext<Bye>")]
///     pub async fn bye(&self, ctx: InCtx<'_,()>) {
///        self.logger.info("funny that! He left without saying a word!");
///     }
/// }
#[proc_macro_attribute]
pub fn handler(attr: TokenStream, item: TokenStream) -> TokenStream {
    _handler(attr, item, true)
}

#[proc_macro_attribute]
pub fn handler_sync(attr: TokenStream, item: TokenStream) -> TokenStream {
    _handler(attr, item, false)
}

fn _handler(attr: TokenStream, item: TokenStream, _async: bool) -> TokenStream {
    let item_cp = item.clone();
    let mut impl_item = parse_macro_input!(item_cp as syn::ItemImpl);
    //    let mut selectors = vec![];
    let mut static_selectors = vec![];
    let mut static_selector_keys = vec![];
    let mut idents = vec![];
    let impl_name = find_impl_type(&impl_item);

    //    let mut output = vec![];

    for item_impl in &impl_item.items {
        if let ImplItem::Method(call) = item_impl {
            if let Some(attr) = find_route_attr(&call.attrs) {
                let internal = attr.tokens.to_token_stream().to_string();
                idents.push(format_ident!("__{}__route", call.sig.ident.clone()));
                let selector_ident = format_ident!("__{}_{}__", impl_name, call.sig.ident);
                let route_selector = attr.to_token_stream().to_string();
                static_selector_keys.push(selector_ident.clone());
                let static_selector = quote! {
                    static ref #selector_ident : cosmic_space::config::bind::RouteSelector = cosmic_space::parse::route_attribute(#route_selector).unwrap();
                };
                static_selectors.push(static_selector);
            //println!(" ~~ ROUTE {}", attr.tokens.to_string() );
            /*                let route = route( attr.to_token_stream().into(), call.to_token_stream().into() );
                           let mut route = parse_macro_input!(route as syn::ImplItem );
                           output.push(route);

            */
            } else {
                //                output.push( ImplItem::Method(call) );
            }
        } else {
            //           output.push( item_impl );
        }
    }

    //    impl_item.items.append( & mut output );

    let self_ty = impl_item.self_ty.clone();
    let generics = impl_item.generics.clone();
    let where_clause = impl_item.generics.where_clause.clone();

    let attr: TokenStream2 = attr.into();

    let rtn = if attr.is_empty() {
        quote! {Ok(RespCore::not_found())}
    } else {
        let rtn = match _async {
            true => quote! {
            #attr.handle(request).await },
            false => quote! {
            #attr.handler.handle(request) },
        };
        rtn
    };

    let selector = match _async {
        true => quote! {cosmic_space::wave::exchange::asynch::DirectedHandlerSelector},
        false => quote! {cosmic_space::wave::exchange::synch::DirectedHandlerSelector},
    };

    let handler = match _async {
        true => quote! {cosmic_space::wave::exchange::asynch::DirectedHandler},
        false => quote! {cosmic_space::wave::exchange::synch::DirectedHandler},
    };

    let root_ctx = match _async {
        true => quote! {cosmic_space::wave::exchange::asynch::RootInCtx},
        false => quote! {cosmic_space::wave::exchange::synch::RootInCtx},
    };

    let _await = match _async {
        true => quote! {.await},
        false => quote! {},
    };

    let _async_trait = match _async {
        true => quote! {#[async_trait]},
        false => quote! {},
    };

    let _async = match _async {
        true => quote! {async},
        false => quote! {},
    };

    let rtn = quote! {
        impl #generics #selector for #self_ty #where_clause{
              fn select<'a>( &self, select: &'a cosmic_space::wave::RecipientSelector<'a>, ) -> Result<&dyn #handler, ()> {
                if select.wave.core().method == cosmic_space::wave::core::Method::Cmd(cosmic_space::wave::core::cmd::CmdMethod::Bounce) {
                    return Ok(self);
                }
                #(
                    if #static_selector_keys.is_match(&select.wave).is_ok() {
                        return Ok(self);
                    }
                )*
                Err(())
              }
        }

        #_async_trait
        impl #generics #handler for #self_ty #where_clause{
            #_async fn handle( &self, ctx: #root_ctx) -> cosmic_space::wave::core::CoreBounce {
                #(
                    if #static_selector_keys.is_match(&ctx.wave).is_ok() {
                       return self.#idents( ctx )#_await;
                    }
                )*
                if ctx.wave.core().method == cosmic_space::wave::core::Method::Cmd(cosmic_space::wave::core::cmd::CmdMethod::Bounce) {
                    return self.bounce(ctx)#_await;
                }
                ctx.not_found().into()
             }
        }

        lazy_static! {
            #( #static_selectors )*
        }

    };

    //    println!("{}", rtn.to_string());

    TokenStream2::from_iter(vec![rtn, TokenStream2::from(item)]).into()
}

fn find_impl_type(item_impl: &ItemImpl) -> Ident {
    if let Type::Path(path) = &*item_impl.self_ty {
        path.path.segments.last().as_ref().unwrap().ident.clone()
    } else {
        panic!("could not get impl name")
    }
}

fn find_route_attr(attrs: &Vec<Attribute>) -> Option<Attribute> {
    for attr in attrs {
        if attr
            .path
            .segments
            .last()
            .expect("segment")
            .to_token_stream()
            .to_string()
            .as_str()
            == "route"
        {
            return Some(attr.clone());
        }
    }
    return None;
}

/*
#[proc_macro_attribute]
pub fn route(attr: TokenStream, item: TokenStream ) -> TokenStream {
    item
}

 */

#[proc_macro_attribute]
pub fn route(attr: TokenStream, input: TokenStream) -> TokenStream {
    //  let combined = TokenStream::from_iter( vec![attr,item]);

    let input = parse_macro_input!(input as syn::ImplItemMethod);

    log(route_attribute_value(attr.to_string().as_str())).expect("valid route selector");

    //    attr.to_tokens().next();
    // we do this just to mem for a valid selector...
    //log(wrapped_route_selector(attr.tokens.to_string().as_str())).expect("properly formatted route selector");

    let params: Vec<FnArg> = input.sig.inputs.clone().into_iter().collect();
    let ctx = params
        .get(1)
        .expect("route expected InCtx<I,M> as first parameter");
    let ctx = messsage_ctx(ctx).expect("route expected InCtx<I,M> as first parameter");

    let __await = match input.sig.asyncness {
        None => quote! {},
        Some(_) => quote! {.await},
    };

    let root_ctx = match input.sig.asyncness {
        None => quote! {cosmic_space::wave::exchange::synch::RootInCtx},
        Some(_) => quote! {cosmic_space::wave::exchange::asynch::RootInCtx},
    };

    let in_ctx = match input.sig.asyncness {
        None => quote! {cosmic_space::wave::exchange::synch::InCtx},
        Some(_) => quote! {cosmic_space::wave::exchange::asynch::InCtx},
    };

    let __async = match input.sig.asyncness {
        None => quote! {},
        Some(_) => quote! {async},
    };

    let orig = input.sig.ident.clone();
    let ident = format_ident!("__{}__route", input.sig.ident);
    let rtn_type = rtn_type(&input.sig.output);
    let item = ctx.item;

    let expanded = quote! {
      #__async fn #ident( &self, mut ctx: #root_ctx ) -> cosmic_space::wave::core::CoreBounce {
          let ctx: #in_ctx<'_,#item> = match ctx.push::<#item>() {
              Ok(ctx) => ctx,
              Err(err) => {
                    if ctx.wave.is_signal() {
                      return cosmic_space::wave::core::CoreBounce::Absorbed;
                    }
                    else {
                      return cosmic_space::wave::core::CoreBounce::Reflected(err.into());
                    }
              }
          };

          let result = self.#orig(ctx)#__await;
          #rtn_type
      }

      #input

    };

    //println!("{}", expanded.to_string());
    TokenStream::from(expanded)
}

pub(crate) enum Item {
    Request,
    RequestCore,
    Payload,
}

impl FromStr for Item {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "Request" => Ok(Item::Request),
            "RequestCore" => Ok(Item::RequestCore),
            "Payload" => Ok(Item::Payload),
            what => panic!("cannot convert Request to type '{}'", what),
        }
    }
}

pub(crate) struct RequestCtx {
    pub item: GenericArgument,
}

fn messsage_ctx(input: &FnArg) -> Result<RequestCtx, String> {
    if let FnArg::Typed(i) = input {
        if let Type::Path(path) = &*i.ty {
            if let PathArguments::AngleBracketed(generics) = &path
                .path
                .segments
                .last()
                .expect("expected last segment")
                .arguments
            {
                let mut args = generics.args.clone();
                let item = args
                    .pop()
                    .expect("expecting a generic for Context Item")
                    .into_value();

                let ctx = RequestCtx { item };

                return Ok(ctx);
            }
        }
    }
    Err("Parameter is not a RequestCtx".to_string())
}

fn rtn_type(output: &ReturnType) -> TokenStream2 {
    match output {
        ReturnType::Default => {
            quote! {cosmic_space::wave::Bounce::Absorbed}
        }
        ReturnType::Type(_, path) => {
            if let Type::Path(path) = &**path {
                let PathSegment { ident, arguments } = path.path.segments.last().unwrap();
                match ident.to_string().as_str() {
                    "Result" => {
                        if let PathArguments::AngleBracketed(brackets) = arguments {
                            let arg = brackets.args.first().unwrap();
                            if "Substance" == arg.to_token_stream().to_string().as_str() {
                                quote! {
                                 use cosmic_space::err::CoreReflector;
                                 match result {
                                     Ok(rtn) => cosmic_space::wave::core::CoreBounce::Reflected(cosmic_space::wave::core::ReflectedCore::ok_body(rtn)),
                                     Err(err) => cosmic_space::wave::core::CoreBounce::Reflected(err.as_reflected_core())
                                 }
                                }
                            } else {
                                quote! {
                                 use cosmic_space::err::CoreReflector;
                                 match result {
                                     Ok(rtn) => cosmic_space::wave::core::CoreBounce::Reflected(rtn.into()),
                                     Err(err) => cosmic_space::wave::core::CoreBounce::Reflected(err.as_reflected_core())
                                 }
                                }
                            }
                        } else {
                            panic!("Result without angle brackets")
                        }
                    }
                    "Bounce" => {
                        quote! {
                            let rtn : cosmic_space::wave::core::CoreBounce = result.to_core_bounce();
                            rtn
                        }
                    }
                    "CoreBounce" => {
                        quote! {
                           result
                        }
                    }
                    "ReflectedCore" => {
                        quote! {
                           cosmic_space::wave::core::CoreBounce::Reflected(result)
                        }
                    }
                    what => {
                        panic!("unknown return type: {}", what);
                    }
                }
            } else {
                panic!("expecting a path segment")
            }
        }
    }
}

struct RouteAttr {
    attribute: Attribute,
}

impl Parse for RouteAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut attribute = input.call(Attribute::parse_outer)?;
        Ok(RouteAttr {
            attribute: attribute.remove(0),
        })
    }
}
