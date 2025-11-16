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

/// the function translate the type
/// also need the translate to created structure
fn type_translate(sym: &str) -> String {
    match sym {
        "string" => "String".to_string(),
        "number" => "i64".to_string(),
        _ => sym.to_string(),
    }
}

/// translate the field types
fn data_to_field_type(d: &Data) -> Result<String, Box<dyn Error + '_>> {
    match d {
        Data::List(list_data) => todo!(), // need give the other struct name
        Data::Map(map_data) => todo!(),   // need give the other struct name
        Data::Value(lisp_rpc_rust_parser::TypeValue::Symbol(s)) => Ok(type_translate(s)),
        Data::Data(_) => Err(Box::new(SpecError {
            msg: format!(
                "data {} convert to type error, cannot be expr in as type",
                d
            ),
            err_type: SpecErrorType::InvalidInput,
        })),
        Data::Error(data_error) => Err(Box::new(data_error)),
        _ => Err(Box::new(SpecError {
            msg: format!("data {} convert to type error", d),
            err_type: SpecErrorType::InvalidInput,
        })),
    }
}

#[cfg(test)]
mod tests {
    use lisp_rpc_rust_parser::TypeValue;

    use super::*;

    fn test_type_translate() {
        assert_eq!(type_translate("string"), "String".to_string());
        assert_eq!(type_translate("number"), "i64".to_string());
    }

    fn test_data_to_field_type() {
        let d = Data::Value(TypeValue::Symbol("number".to_string()));
        assert_eq!(data_to_field_type(&d).unwrap(), "i64".to_string());

        let d = Data::Value(TypeValue::Symbol("string".to_string()));
        assert_eq!(data_to_field_type(&d).unwrap(), "String".to_string());
    }
}
