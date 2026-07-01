use std::fs::OpenOptions;
use std::io::Write;
use std::path::{Path, PathBuf};

/// Appends a string to the end of a file inside the specified directory.
///
/// Returns an error if the directory does not exist or is not a folder.
pub fn append_string_to_file(content: &str, folder: PathBuf, filename: &str) -> anyhow::Result<()> {
    if !folder.exists() {
        anyhow::bail!("Folder '{:?}' does not exist", folder);
    }
    if !folder.is_dir() {
        anyhow::bail!("Path '{:?}' is not a directory", folder);
    }

    let file_path = folder.join(filename);
    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(file_path)?;

    file.write_all(content.as_bytes())?;
    Ok(())
}

/// Checks if the folder exists. If not, logs a warning and creates it.
pub fn ensure_directory_exists(folder: &Path) -> anyhow::Result<()> {
    if !folder.exists() {
        log::warn!("Directory '{:?}' does not exist, creating it.", folder);
        std::fs::create_dir_all(folder)?;
    } else if !folder.is_dir() {
        anyhow::bail!("Path '{:?}' exists but is not a directory", folder);
    }
    Ok(())
}

/// Checks if the file exists. If not, creates it.
pub fn ensure_file_exists(file_path: &Path) -> anyhow::Result<()> {
    if !file_path.exists() {
        if let Some(parent) = file_path.parent() {
            if !parent.as_os_str().is_empty() {
                std::fs::create_dir_all(parent)?;
            }
        }
        std::fs::File::create(file_path)?;
    } else if !file_path.is_file() {
        anyhow::bail!("Path '{:?}' exists but is not a file", file_path);
    }
    Ok(())
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

    #[test]
    fn test_append_string_to_file_not_exist() {
        let non_existent_dir = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("non_existent_folder_xyz");
        if non_existent_dir.exists() {
            fs::remove_dir_all(&non_existent_dir).unwrap();
        }

        let result = append_string_to_file("test", non_existent_dir, "some_file.txt");
        assert!(result.is_err());
    }

    #[test]
    fn test_ensure_directory_exists() {
        let test_dir = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("test_ensure_dir");
        if test_dir.exists() {
            fs::remove_dir_all(&test_dir).unwrap();
        }

        // It shouldn't exist, so it should log a warning and create it
        ensure_directory_exists(&test_dir).unwrap();
        assert!(test_dir.is_dir());

        // Call again when it already exists (should do nothing and succeed)
        ensure_directory_exists(&test_dir).unwrap();

        fs::remove_dir_all(&test_dir).unwrap();
    }

    #[test]
    fn test_ensure_file_exists() {
        let test_file = std::env::current_dir()
            .unwrap()
            .join("target")
            .join("test_ensure_file_dir")
            .join("test_file.txt");

        let parent = test_file.parent().unwrap();
        if parent.exists() {
            fs::remove_dir_all(parent).unwrap();
        }

        // It shouldn't exist, should create directories and the file
        ensure_file_exists(&test_file).unwrap();
        assert!(test_file.is_file());

        // Call again when it already exists (should do nothing and succeed)
        ensure_file_exists(&test_file).unwrap();

        fs::remove_dir_all(parent).unwrap();
    }
}
