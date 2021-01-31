#![feature(test)]
extern crate test;

use serde::{Deserialize, Serialize};

#[repr(C)]
#[derive(Serialize, Deserialize, Debug)]
pub struct Method {
    name: String,
    doc: String,
    types: Vec<String>,
    parameters: Vec<String>,
    requireds: Vec<String>,
    descriptions: Vec<String>,
}

#[no_mangle]
pub extern "C" fn json_parser(raw_json: &str) -> Vec<Method> {
    serde_json::from_str(&raw_json).unwrap()
}

#[no_mangle]
pub extern "C" fn hello() {
    println!("hello");
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::prelude::*;
    use test::Bencher;

    #[bench]
    fn bench_json_parser(b: &mut Bencher) {
        let mut file = File::open("../methods.json").unwrap();
        let mut contents = String::new();
        file.read_to_string(&mut contents).unwrap();
        b.iter(|| json_parser(&contents));
    }
}
