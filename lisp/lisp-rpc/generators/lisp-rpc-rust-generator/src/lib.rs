#![feature(iter_array_chunks)]
#![feature(box_patterns)]

pub mod def_msg;
pub mod def_rpc;
pub mod generater;

use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use url::Url;

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
pub fn type_translate(sym: &str) -> String {
    match kebab_to_pascal_case(sym).as_str() {
        "Number" => "i64".to_string(),
        s @ _ => s.to_string(),
    }
}

/// read from file or url
pub fn read_single_template_content(source: &str) -> Result<String, Box<dyn Error>> {
    if let Ok(url) = Url::parse(source) {
        if url.scheme() == "http" || url.scheme() == "https" {
            println!("Attempting to fetch content from URL: {}", url);
            let response = reqwest::blocking::get(url.as_str())?.error_for_status()?;
            return Ok(response.text()?);
        }
    }

    let path = Path::new(source);
    println!(
        "Attempting to read content from local file: {}",
        path.display()
    );
    fs::read_to_string(path).map_err(|e| e.into())
}

pub fn get_all_file_paths_in_folder(folder_path: &Path) -> Result<Vec<PathBuf>, Box<dyn Error>> {
    if !folder_path.is_dir() {
        return Err(format!("Path is not a directory: {}", folder_path.display()).into());
    }

    println!(
        "Scanning directory for files (using std recursion): {}",
        folder_path.display()
    );
    let mut file_paths = Vec::new();
    let mut entries_to_process: Vec<PathBuf> = Vec::new();

    entries_to_process.push(folder_path.to_path_buf());

    while let Some(current_path) = entries_to_process.pop() {
        if current_path.is_file() {
            file_paths.push(current_path);
        } else if current_path.is_dir() {
            for entry_result in fs::read_dir(&current_path)? {
                let entry = entry_result?;
                entries_to_process.push(entry.path());
            }
        }
    }

    Ok(file_paths)
}
