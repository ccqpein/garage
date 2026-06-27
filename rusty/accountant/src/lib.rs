use serde::{Deserialize, Serialize};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;

/// Appends a string to the end of a file inside the specified directory.
///
/// Returns an error if the directory does not exist or is not a folder.
pub fn append_string_to_file(content: &str, folder: PathBuf, filename: &str) -> anyhow::Result<()> {
    if !folder.is_dir() {
        anyhow::bail!("Path '{:?}' is not a directory or does not exist", folder);
    }

    let file_path = folder.join(filename);
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(file_path)?;

    file.write_all(content.as_bytes())?;
    Ok(())
}

/// Mode of the application.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Mode {
    Dev,
    Prod,
}

/// Config struct holding application configurations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    pub data_folder: PathBuf,
    pub mode: Mode,
}

impl Config {
    /// Loads configuration from the environment variables or configuration files.
    ///
    /// Expected environment variables:
    /// - `ACCOUNTANT_DATA_FOLDER`
    /// - `ACCOUNTANT_MODE`
    pub fn load() -> anyhow::Result<Self> {
        let settings = config::Config::builder()
            // Look for `accountant.toml` (or yaml/json) in the project root
            .add_source(config::File::with_name("accountant").required(false))
            // Layer environment variables
            .add_source(config::Environment::with_prefix("ACCOUNTANT").separator("_"))
            .build()?;

        Ok(settings.try_deserialize()?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_append_string_to_file_success() {
        let test_dir = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("test_append_dir");
        fs::create_dir_all(&test_dir).unwrap();

        let filename = "test_file.txt";
        let file_path = test_dir.join(filename);
        if file_path.exists() {
            fs::remove_file(&file_path).unwrap();
        }

        // Write first part
        append_string_to_file("Hello, ", test_dir.clone(), filename).unwrap();
        // Append second part
        append_string_to_file("World!", test_dir.clone(), filename).unwrap();

        let content = fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "Hello, World!");

        // Clean up
        fs::remove_file(&file_path).unwrap();
    }

    #[test]
    fn test_append_string_to_file_not_dir() {
        let test_file = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("not_a_dir_file.txt");
        fs::write(&test_file, "temp").unwrap();

        // Should return error because `test_file` is a file, not a directory
        let result = append_string_to_file("test", test_file.clone(), "some_file.txt");
        assert!(result.is_err());

        fs::remove_file(&test_file).unwrap();
    }
}
