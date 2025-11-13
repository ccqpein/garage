#![feature(iter_array_chunks)]
#![feature(box_patterns)]

pub mod def_msg;
pub mod def_rpc;
pub mod generater;

pub use def_msg::*;
pub use def_rpc::*;
pub use generater::*;

use lisp_rpc_rust_parser::data::Data;

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

/// the function translate the type
fn type_translate(sym: &str) -> String {
    match sym {
        "string" => "String".to_string(),
        _ => String::new(),
    }
}

/// translate the field types
fn data_to_field_type(d: &Data) -> String {
    match d {
        Data::Data(expr_data) => todo!(), // maybe // need give the other struct name
        Data::List(list_data) => todo!(), // need give the other struct name
        Data::Map(map_data) => todo!(),   // need give the other struct name
        Data::Value(type_value) => todo!(), // just change to rust type
        Data::Error(data_error) => todo!(), // still thinking
    }
}
