/*#![feature(proc_macro_quote)]*/
#![crate_type = "lib"]
#![allow(warnings)]

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;

use quote::quote;
use quote::ToTokens;
use syn::{parse_macro_input, Data, DeriveInput, PathArguments, Type};

/// Takes a given enum (which in turn accepts child enums) and auto generates a `Parent::From` so the child
/// can turn into the parent and a `TryInto<Child> for Parent` so the Parent can attempt to turn into the child.
/// ```
/// #[derive(Autobox)]
/// pub enum Parent {
///   Child(Child)
/// }
///
/// pub enum Child {
///   Variant1,
///   Variant2
/// }
/// ```
/// Will generate something like:
/// ```
///
/// impl From<Child> for Parent {
///   fn from( child: Child ) -> Self {
///      Self::Child(child)
///   }
/// }
///
/// impl TryInto<Child> for Parent {
///   type Err=SpaceErr;
///
///   fn try_into(self) -> Result<Child,Self::Err> {
///     if let Self::Child(child) = self {
///        Ok(self)
///     } else {
///        Err("err")
///     }
///   }
/// }
/// ```
#[proc_macro_derive(Autobox)]
pub fn autobox(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let ident = &input.ident;

    let mut xforms = vec![];
    if let Data::Enum(data) = &input.data {
        for variant in data.variants.clone() {
            if variant.fields.len() > 1 {
                panic!("derive Transform only works on Enums with single value tuples")
            }

            let variant_ident = variant.ident.clone();

            if variant.fields.len() == 1 {
                let mut i = variant.fields.iter();
                let field = i.next().unwrap().clone();
                let ty = field.ty.clone();
                match ty {
                    Type::Path(path) => {
                        let segment = path.path.segments.last().cloned().unwrap();
                        if segment.ident == format_ident!("{}", "Box") {
                            let ty = match segment.arguments {
                                PathArguments::AngleBracketed(ty) => {
                                    format_ident!("{}", ty.args.to_token_stream().to_string())
                                }
                                _ => panic!("expecting angle brackets"),
                            };

                            let ty_str = ty.to_string();

                            xforms.push(quote! {
                                impl TryInto<#ty> for #ident {
                                    type Error=SpaceErr;

                                    fn try_into(self) -> Result<#ty,Self::Error> {
                                        match self {
                                        Self::#variant_ident(val) => Ok(*val),
                                        _ => Err(SpaceErr::from_str(format!("expected {}",#ty_str)))
                                        }
                                    }
                                }


                                impl From<#ty> for #ident {
                                    fn from(f: #ty) -> #ident {
                                    #ident::#variant_ident(Box::new(f))
                                }
                            }
                                    });
                        } else {
                            let ty = segment.ident;
                            let ty_str = ty.to_token_stream().to_string();
                            xforms.push(quote! {
                                impl TryInto<#ty> for #ident {
                                    type Error=SpaceErr;

                                    fn try_into(self) -> Result<#ty,Self::Error> {
                                        match self {
                                            Self::#variant_ident(val) => Ok(val),
                                            _ => Err(format!("expected {}",#ty_str).into())
                                        }
                                    }
                                }


                                impl From<#ty> for #ident {
                                    fn from(f: #ty) -> #ident {
                                        #ident::#variant_ident(f)
                                    }
                                }
                            });
                        }
                    }
                    _ => {
                        panic!("TransformVariants can only handle Path types")
                    }
                }
            }
        }
    } else {
        panic!("derive Transform only works on Enums")
    }

    let rtn = quote! { #(#xforms)* };

    rtn.into()
}

#[proc_macro_derive(ToSubstance)]
pub fn to_substance(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let ident = &input.ident;

    let mut xforms = vec![];
    if let Data::Enum(data) = &input.data {
        for variant in data.variants.clone() {
            if variant.fields.len() > 1 {
                panic!("derive Transform only works on Enums with single value tuples")
            }

            let variant_ident = variant.ident.clone();

            if variant.fields.len() == 1 {
                let mut i = variant.fields.iter();
                let field = i.next().unwrap().clone();
                let ty = field.ty.clone();
                match ty {
                    Type::Path(path) => {
                        let segment = path.path.segments.last().cloned().unwrap();
                        if segment.ident == format_ident!("{}", "Box") {
                            let ty = match segment.arguments {
                                PathArguments::AngleBracketed(ty) => {
                                    format_ident!("{}", ty.args.to_token_stream().to_string())
                                }
                                _ => panic!("expecting angle brackets"),
                            };

                            let ty_str = ty.to_string();

                            xforms.push(quote! {
                            impl ToSubstance<#ty> for #ident {
                                fn to_substance(self) -> anyhow::Result<#ty> {
                                    match self {
                                    Self::#variant_ident(val) => Ok(*val),
                                    _ => Err(anyhow!("expected {}",#ty_str).into())
                                    }
                                }

                                fn to_substance_ref(&self) -> anyhow::Result<&#ty> {
                                    match self {
                                    Self::#variant_ident(val) => Ok(val.as_ref()),
                                    _ => Err(anyhow!("expected {}",#ty_str).into())
                                    }
                                }
                            }

                                });
                        } else {
                            let ty = segment.ident;
                            let ty_str = ty.to_token_stream().to_string();
                            xforms.push(quote! {
                            impl ToSubstance<#ty> for #ident {
                                fn to_substance(self) -> anyhow::Result<#ty> {
                                    match self {
                                    Self::#variant_ident(val) => Ok(val),
                                    _ => Err(anyhow!("expected {}",#ty_str).into())
                                    }
                                }
                                 fn to_substance_ref(&self) -> anyhow::Result<&#ty> {
                                    match self {
                                    Self::#variant_ident(val) => Ok(val),
                                    _ => Err(anyhow!("expected {}",#ty_str).into())
                                    }
                                }
                            }

                            });
                        }
                    }
                    _ => {
                        panic!("ToSubstance can only handle Path types")
                    }
                }
            } else {
                xforms.push(quote! {
                impl ToSubstance<()> for #ident {
                    fn to_substance(self) -> anyhow::Result<()> {
                        match self {
                        Self::#variant_ident => Ok(()),
                        _ => Err(anyhow!("expected Empty").into())
                        }
                    }
                     fn to_substance_ref(&self) -> anyhow::Result<&()> {
                        match self {
                        Self::#variant_ident => Ok(&()),
                        _ => Err(anyhow!("expected Empty").into())
                        }
                    }
                }

                });
            }
        }
    } else {
        panic!("derive ToSubstance only works on Enums")
    }

    let rtn = quote! { #(#xforms)* };

    rtn.into()
}

#[proc_macro_derive(MechErr)]
pub fn mech_err(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);
    let ident = &input.ident;

    let from = vec![
        quote!(Box<bincode::ErrorKind>),
        quote!(mechtron::err::MembraneErr),
        quote!(starlane_space::err::SpaceErr),
        quote!(String),
        quote!(&'static str),
        quote!(mechtron::err::GuestErr),
        quote!(std::string::FromUtf8Error),
    ];

    let rtn = quote! {

        impl MechErr for #ident {
            fn to_uni_err(self) -> starlane_space::err::anyhowSpaceErr {
               starlane_space::err::SpaceErr::server_error(self.to_string())
            }
        }

        impl From<#ident> for mechtron::err::GuestErr{
            fn from(e: #ident) -> Self {
                        mechtron::err::GuestErr {
                            message: e.to_string()
                        }
            }
        }

        impl starlane_space::err::CoreReflector for #ident {
                fn as_reflected_core(self) -> starlane_space::wave::core::ReflectedCore {
                   starlane_space::wave::core::ReflectedCore{
                        headers: Default::default(),
                        status: starlane_space::wave::core::http2::StatusCode::from_u16(500u16).unwrap(),
                        body: starlane_space::substance::Substance::Err(self.to_uni_err()),
                    }
            }
        }


        impl ToString for #ident {
            fn to_string(&self) -> String {
                self.message.clone()
            }
        }

        #(
            impl From<#from> for #ident {
                fn from(e: #from ) -> Self {
                    Self {
                        message: e.to_string()
                    }
                }
            }
        )*
    };
    //println!("{}", rtn.to_string());
    rtn.into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
