extern crate proc_macro;

use proc_macro::*;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Data, DataStruct, DeriveInput, Fields, GenericParam, Generics,
    Index,
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
    let mut all_fields: Vec<Ident> = vec![];
    match input.data {
        Data::Struct(DataStruct {
            fields: Fields::Named(ref fields),
            ..
        }) => {
            println!("{:?}", fields);
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
                    all_fields.push(nn.ident.unwrap());
                }
            }
        }
        Data::Enum(_) => {}
        Data::Union(_) => {}
        _ => {}
    }

    let name = input.ident;
    //let namename = format!("{}", name);
    let expanded = quote! {
        impl #name {
            fn give_me_fields() -> Vec<String> {
                let mut result = vec![#name.into()];
                //result.append(#all_fields)

            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}
