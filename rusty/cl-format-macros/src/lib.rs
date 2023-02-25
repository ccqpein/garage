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

use proc_macro::TokenStream;
use proc_macro2::{Ident, Punct, Span};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Data, DataStruct, DeriveInput, Fields, GenericParam, Generics,
    Index,
};

#[proc_macro_derive(TildeAble, attributes(implTo))]
pub fn derive_tilde_able(input: TokenStream) -> TokenStream {
    proc_macro2::TokenStream::new().into()
}
