extern crate proc_macro;

use proc_macro::*;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics, Index,
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
    //println!("{:?}", input);

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

//#[proc_macro_derive(GiveMeFields)]
