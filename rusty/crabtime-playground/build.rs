use std::env;

fn main() {
    println!("cargo:rerun-if-changed=ALWAYS_RERUN");
    println!("output path is: {}", env::var("OUT_DIR").unwrap());
}
