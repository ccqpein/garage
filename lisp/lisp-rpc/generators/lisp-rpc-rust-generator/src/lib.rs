#![feature(iter_array_chunks)]
#![feature(box_patterns)]

pub mod def_msg;
pub mod def_rpc;
pub mod generater;

use std::error::Error;

pub use def_msg::*;
pub use def_rpc::*;
pub use generater::*;

use lisp_rpc_rust_parser::data::Data;

#[derive(Debug)]
enum SpecErrorType {
    InvalidInput,
}

#[derive(Debug)]
struct SpecError {
    msg: String,
    err_type: SpecErrorType,
}

impl std::fmt::Display for SpecError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for SpecError {}

pub fn kebab_to_pascal_case(s: &str) -> String {
    s.split('-')
        .map(|segment| {
            let mut chars = segment.chars();
            match chars.next() {
                None => String::new(),
                Some(first_char) => first_char.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect()
}

pub fn kebab_to_snake_case(s: &str) -> String {
    s.replace('-', "_")
}

/// the function translate the type, the sym's first chat is upper because the kebab_to_pascal_case
fn type_translate(sym: &str) -> String {
    match kebab_to_pascal_case(sym).as_str() {
        "Number" => "i64".to_string(),
        s @ _ => s.to_string(),
    }
}
