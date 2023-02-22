extern crate proc_macro;

use proc_macro::*;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote, Attribute, Data, DataEnum, DataStruct, DeriveInput, Field,
    Fields, GenericParam, Generics, Index, Meta, Result, Token,
};

pub(crate) trait DeriveTestYo {
    fn hello_macros() -> String; // return this struct name
}

pub(crate) trait GiveMeFields {
    fn give_me_fields() -> Vec<String>;
}

#[proc_macro_derive(DeriveTestYo)]
pub fn derive_test_yo(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    println!("{:?}", input);

    let name = input.ident;
    let namename = format!("{}", name);
    let expanded = quote! {
        impl #name {
            fn hello_macros() -> String {
                #namename.into()
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(GiveMeFields, attributes(this))]
pub fn derive_test_fields(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    //println!("{:?}", input.data);
    let mut all_fields: Vec<String> = vec![];
    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            println!("fields: {:?}", fields);
            for nn in &fields.named {
                if nn
                    .attrs
                    .iter()
                    .filter(|att| {
                        att.path
                            .segments
                            .iter()
                            .filter(|seg| seg.ident == "this")
                            .next()
                            .is_some()
                    })
                    .next()
                    .is_some()
                {
                    all_fields.push(nn.ident.clone().unwrap().to_string());
                }
            }
        }
        _ => {}
    }

    let fields_tokens = quote! {
        vec![#(#all_fields),*].iter().map(|s| s.to_string()).collect::<Vec<String>>()
    };

    dbg!(&fields_tokens.to_string());

    let name = input.ident;
    let namename = format!("{}", name);
    let expanded = quote! {
        impl #name {
            fn give_me_fields() -> Vec<String> {
                let mut result = vec![#namename.into()];
                let mut ff = #fields_tokens;
                result.append(&mut ff);
                result
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

// looks like normal_attribute can only work with function/struct?
#[proc_macro_attribute]
pub fn normal_attribute(attr: TokenStream, item: TokenStream) -> TokenStream {
    // let input = parse_macro_input!(attr as Attribute);
    // println!("{:?}", input);
    println!("attr: {:?}, item: {:?}", attr, item);

    proc_macro::TokenStream::new()
}

#[proc_macro_derive(AutoImpl, attributes(to))]
pub fn derive_fields_to(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    //println!("{:#?}", input.data);
    let mut var_to_pairs = vec![];
    match input.data {
        // Data::Struct(DataStruct {
        //     fields: Fields::Named(fields),
        //     ..
        // }) => {
        //     for f in fields.named {
        //         //println!("{:#?}", f);
        //     }
        // }
        Data::Enum(DataEnum { variants, .. }) => {
            for v in variants {
                let mut ids = vec![];
                if let Some(attr) = v
                    .attrs
                    .iter()
                    .filter(|att| {
                        att.path
                            .segments
                            .iter()
                            .filter(|seg| seg.ident == "to")
                            .next()
                            .is_some()
                    })
                    .next()
                {
                    println!("attr.tokens: {:#?}", attr.tokens);

                    match attr.parse_meta() {
                        Ok(meta_list) => {
                            println!("{:?}", meta_list);
                            match meta_list {
                                Meta::List(meta_l) => {
                                    meta_l.nested.into_iter().for_each(|m| match m {
                                        syn::NestedMeta::Meta(mm) => match mm.path().get_ident() {
                                            Some(id) => {
                                                ids.push(id.to_string());
                                            }
                                            None => unreachable!(),
                                        },
                                        syn::NestedMeta::Lit(_) => unreachable!(),
                                    })
                                }
                                _ => unreachable!(),
                            }
                        }
                        Err(e) => std::panic::panic_any(e),
                    }
                    //println!("lit: {:#?}", lit);
                    //let inner_tokens = attr.tokens;
                    //println!("meta: {:#?}", parse_macro_input!(inner_tokens));
                    //let gg = parse_macro_input!();

                    // let group =
                    //     parse_macro_input!(attr.tokens.into_iter().next().unwrap() as DeriveInput);
                    // var_to_pairs.push((v.ident.to_string(),))
                    var_to_pairs.push((v.ident.to_string(), ids))
                }
            }
        }
        //Data::Union(_) => todo!(),
        _ => {}
    }

    println!("var_to_pairs: {:?}", var_to_pairs);
    proc_macro::TokenStream::new()
}

// struct UnitStruct {
//     attrs: Vec<Attribute>,
//     struct_token: Token![struct],
//     name: Ident,
//     semi_token: Token![;],
// }

// impl Parse for UnitStruct {
//     fn parse(input: ParseStream) -> Result<Self> {
//         Ok(UnitStruct {
//             attrs: input.call(Attribute::parse_outer)?,
//             struct_token: input.parse()?,
//             name: input.parse()?,
//             semi_token: input.parse()?,
//         })
//     }
// }
