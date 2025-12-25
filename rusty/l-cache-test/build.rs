use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;

fn main() {
    // This logic attempts to find the size for macOS or Linux.
    let l1_size = if cfg!(target_os = "macos") {
        let output = Command::new("sysctl")
            .arg("-n")
            .arg("hw.l1dcachesize")
            .output()
            .expect("Failed to execute sysctl");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    } else if cfg!(target_os = "linux") {
        let output = Command::new("getconf")
            .arg("LEVEL1_DCACHE_SIZE")
            .output()
            .expect("Failed to execute getconf");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    } else {
        // Fallback for Windows or unknown (32KB is a safe standard default)
        "32768".to_string()
    };

    // This logic attempts to find the size for macOS or Linux.
    let cache_line_size = if cfg!(target_os = "macos") {
        let output = Command::new("sysctl")
            .arg("-n")
            .arg("hw.cachelinesize")
            .output()
            .expect("Failed to execute sysctl");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    } else if cfg!(target_os = "linux") {
        let output = Command::new("getconf")
            .arg("LEVEL1_DCACHE_LINESIZE")
            .output()
            .expect("Failed to execute getconf");
        String::from_utf8(output.stdout).unwrap().trim().to_string()
    } else {
        // Fallback for Windows or unknown (32KB is a safe standard default)
        "32768".to_string()
    };

    // Ensure we got a number, otherwise default to 32KB
    let l1_size: usize = l1_size.parse().unwrap_or(32768);
    let cache_line_size: usize = cache_line_size.parse().unwrap_or(32768);

    // Define where to write the constant
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("constants.rs");

    // Write the Rust code to that file
    fs::write(
        &dest_path,
        format!(
            r#"pub const L1_CACHE_SIZE: usize = {};
pub const CACHE_LINE_SIZE: usize = {};"#,
            l1_size, cache_line_size
        ),
    )
    .unwrap();

    // Tell Cargo to rerun this script if build.rs changes
    println!("cargo:rerun-if-changed=build.rs");
}
