#![doc = r"The macros here should auto generate several traits and the major TildeAble trait.

For example:

```rust
#[derive(Debug, PartialEq, TildeAble)]
pub enum TildeKind {
    /// ~C ~:C
    #[implTo(char)]
    Char,

    /// ~$ ~5$ ~f
    #[implTo(float)]
    Float(Option<String>),

    /// ~d ~:d ~:@d
    Digit(Option<String>),

    /// ~a
    #[implTo(float, char, String)]
    Va,

    /// loop
    Loop(Vec<Tilde>),

    /// text inside the tilde
    Text(String),

    /// vec
    VecTilde(Vec<Tilde>),
}
```

Will generate:

```rust
/// all default method is return none.
trait TildeAble {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar>{None}
    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa>{None}
    // and all other fields...
}

impl TildeAble for char {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar> {
        Some(self)
    }

    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(self)
    }
}

impl TildeAble for float {
    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(self)
    }
}

impl TildeAble for String {
    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(self)
    }
}

trait TildeKindChar {}
trait TildeKindVa {}

```
"]

use std::{collections::HashMap, error::Error};

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{parse_macro_input, Attribute, Data, DataEnum, DeriveInput, Variant};

#[proc_macro_derive(TildeAble, attributes(implTo))]
pub fn derive_tilde_able(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    //let mut all_methods_headers = vec![];
    let mut return_types_traits = vec![];
    let mut all_default_methods = vec![];
    let mut types_impl_methods = HashMap::new();

    match input.data {
        Data::Enum(DataEnum { ref variants, .. }) => {
            let all_vars = variants.iter().map(|var| parse_variant_attrs(var));

            all_vars.for_each(|(field, tys)| {
                let fname = Ident::new(
                    &(String::from("into_tildekind_") + &field.to_lowercase()),
                    Span::call_site(),
                );

                let return_type =
                    Ident::new(&(String::from("TildeKind") + &field), Span::call_site());

                // add default methods to TildeAble
                all_default_methods
                    .push(quote! {
						fn #fname(&self) -> Option<&dyn #return_type> {
							None
						}});

                // impl for types
                tys.for_each(|ty| {
                    let en = types_impl_methods.entry(ty).or_insert(vec![]);
                    en.push(quote! {fn #fname(&self) -> Option<&dyn #return_type> {
						Some(self)
					}})
                });

                //
                return_types_traits.push(quote! {
                    pub trait #return_type: Debug {
                        fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
                            Err("un-implenmented yet".into())
                        }
                }})
            });
        }
        _ => panic!("only support the enum"),
    };

    let mut result = vec![];

    // trait TildeAble defination
    let tilde_able_trait = quote! {pub trait TildeAble:Debug {#(#all_default_methods)*}};

    let mut auto_impl_for_types = types_impl_methods
        .iter()
        .map(|(ty, methods)| {
            quote! {impl TildeAble for #ty {#(#methods)*}}
        })
        .collect();

    // merge together
    result.push(tilde_able_trait);
    result.append(&mut auto_impl_for_types);
    result.append(&mut return_types_traits);

    proc_macro2::TokenStream::from_iter(result.into_iter()).into()
}

/// return the field Ident and all types implTo. Empty if there is no implTo types
fn parse_variant_attrs(variant: &Variant) -> (String, impl Iterator<Item = Ident> + '_) {
    let all_impl_to_type = variant
        .attrs
        .iter()
        .filter(|attr| attr.path.get_ident().map(|d| d.to_string()) == Some("implTo".to_string()))
        .map(|attr| get_types_impl_to(attr).unwrap())
        .flatten();

    let field = variant.ident.to_string();

    (field.clone(), all_impl_to_type)
}

/// parse the `implTo` attribute
fn get_types_impl_to(attribute: &Attribute) -> Result<impl Iterator<Item = Ident>, Box<dyn Error>> {
    match attribute.parse_meta()? {
        syn::Meta::List(meta_l) => Ok(meta_l.nested.into_iter().filter_map(|m| match m {
            syn::NestedMeta::Meta(mm) => mm.path().get_ident().cloned(),
            _ => None,
        })),
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use syn::{parse2, parse_quote, Variant};

    #[test]
    fn test_get_types_impl_to() -> Result<(), Box<dyn Error>> {
        let test_case: Attribute = parse_quote! {
                #[implTo(a,b,c,d)]
        };

        //dbg!(test_case);
        assert_eq!(
            vec!["a", "b", "c", "d"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
            get_types_impl_to(&test_case)
                .unwrap()
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
        );

        let test_case: Attribute = parse_quote! {
                #[implTo(a)]
        };

        //dbg!(test_case);
        assert_eq!(
            vec!["a"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
            get_types_impl_to(&test_case)
                .unwrap()
                .into_iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
        );

        Ok(())
    }

    #[test]
    fn test_parse_variant_attrs() -> Result<(), Box<dyn Error>> {
        let test_case: Variant = parse_quote! {
            #[implTo(a,b,c,d)]
            A
        };

        //dbg!(test_case);
        let result = parse_variant_attrs(&test_case);
        assert_eq!(result.0, "A");
        assert_eq!(
            result.1.map(|i| i.to_string()).collect::<Vec<_>>(),
            vec!["a", "b", "c", "d"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
        );

        //
        let test_case: Variant = parse_quote! {
            B
        };

        //dbg!(&test_case);
        let mut result = parse_variant_attrs(&test_case);
        assert_eq!(result.0, "B");
        assert_eq!(result.1.next(), None);

        Ok(())
    }
}