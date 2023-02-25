#![doc = r"The macros here should auto generate several traits and the major TildeAble trait.

For example:

```rust
#[derive(Debug, PartialEq, TildeAble)]
pub enum TildeKind {
    /// ~C ~:C
    #[implTo(char)]
    Char,

    /// ~$ ~5$ ~f
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
trait TildeAble {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar>;
    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa>;
}

impl TildeAble for char {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar> {
        Some(&self)
    }

    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(&self)
    }
}

impl TildeAble for float {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar> {
        None
    }

    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(&self)
    }
}

impl TildeAble for String {
    fn into_tildekind_char(&self) -> Option<&dyn TildeKindChar> {
        None
    }

    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(&self)
    }
}

trait TildeKindChar {}
trait TildeKindVa {}

```
"]

use std::error::Error;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Punct, Span};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Attribute, Data, DataEnum, DataStruct, DeriveInput, Fields,
    GenericParam, Generics, Index, Variant,
};

#[proc_macro_derive(TildeAble, attributes(implTo))]
pub fn derive_tilde_able(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match input.data {
        Data::Enum(DataEnum { ref variants, .. }) => {
            //variants.iter().filter_map(|v| v.attrs.)
        }
        _ => panic!("only support the enum"),
    }

    proc_macro2::TokenStream::new().into()
}

/// return the field Ident and all types implTo if it has implTo
fn parse_variant_attrs(variant: &Variant) -> (Ident, impl Iterator<Item = Ident> + '_) {
    let all_impl_to_type = variant
        .attrs
        .iter()
        .filter(|attr| attr.path.get_ident().map(|d| d.to_string()) == Some("implTo".to_string()))
        .map(|attr| get_types_impl_to(attr).unwrap())
        .flatten();

    let field = &variant.ident;

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
        assert_eq!(result.0, Ident::new("A", Span::call_site()));
        assert_eq!(
            result.1.map(|i| i.to_string()).collect::<Vec<_>>(),
            vec!["a", "b", "c", "d"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>(),
        );

        let test_case: Variant = parse_quote! {
            B
        };

        //dbg!(&test_case);
        let mut result = parse_variant_attrs(&test_case);
        assert_eq!(result.0, Ident::new("B", Span::call_site()));
        assert_eq!(result.1.next(), None);

        Ok(())
    }
}
