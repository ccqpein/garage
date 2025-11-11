#![feature(iter_array_chunks)]
#![feature(box_patterns)]

pub mod def_msg;
pub mod def_rpc;

pub use def_msg::*;
pub use def_rpc::*;

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
