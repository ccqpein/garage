use chrono::NaiveDate;
use dioxus::prelude::*;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, fs};

#[derive(Deserialize, Serialize, Clone)]
pub struct Blogs {
    map: HashMap<String, Blog>,
    ordered_blogs: Vec<String>,
}

impl Blogs {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            ordered_blogs: vec![],
        }
    }

    pub fn all_titles(&self) -> Vec<String> {
        self.ordered_blogs.iter().map(|t| t.to_string()).collect()
    }

    pub fn all_blogs(&self) -> &HashMap<String, Blog> {
        &self.map
    }

    /// get the prev blog of this one
    pub fn prev_blog(&self, this_blog: &str) -> Option<String> {
        match self
            .ordered_blogs
            .iter()
            .enumerate()
            .find(|(_, title)| *title == this_blog)
        {
            Some((this_ind, _)) => {
                if this_ind > 0 {
                    Some(self.ordered_blogs.get(this_ind - 1).unwrap().to_string())
                } else {
                    None
                }
            }
            None => None,
        }
    }

    /// get the next blog of this one
    pub fn next_blog(&self, this_blog: &str) -> Option<String> {
        match self
            .ordered_blogs
            .iter()
            .enumerate()
            .find(|(_, title)| *title == this_blog)
        {
            Some((this_ind, _)) => {
                if this_ind == self.ordered_blogs.len() - 1 {
                    None
                } else {
                    Some(self.ordered_blogs.get(this_ind + 1).unwrap().to_string())
                }
            }
            None => None,
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Eq, PartialEq, Clone)]
pub struct Blog {
    pub title: String,
    pub content: String,

    #[serde(skip)]
    pub date: NaiveDate,
}

impl Ord for Blog {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .date
            .cmp(&self.date)
            .then_with(|| self.title.cmp(&other.title))
    }
}

impl PartialOrd for Blog {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[server]
pub async fn all_blogs() -> Result<Blogs, ServerFnError> {
    //tokio::time::sleep(tokio::time::Duration::from_secs(8)).await;
    let bs = read_markdown_posts()?;
    let ordered_blogs = bs.iter().map(|b| b.title.to_string()).collect();
    Ok(Blogs {
        map: bs.into_iter().map(|b| (b.title.to_string(), b)).collect(),
        ordered_blogs: ordered_blogs,
    })
}

pub fn read_markdown_posts() -> Result<Vec<Blog>, std::io::Error> {
    let mut posts = Vec::new();

    // Get the current working directory, assumed to be the project root.
    let current_dir = std::env::current_dir()?;
    let posts_dir = current_dir.join("posts");

    // Check if the 'post' directory exists and is actually a directory.
    if !posts_dir.exists() || !posts_dir.is_dir() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("'post' directory not found at: {}", posts_dir.display()),
        ));
    }

    // Iterate over entries in the 'post' directory.
    for entry_result in fs::read_dir(&posts_dir)? {
        let entry = entry_result?; // Handle potential errors for each directory entry.
        let path = entry.path();

        // Process only regular files.
        if path.is_file() {
            if let Some(extension) = path.extension() {
                if extension == "md" {
                    let filename_os_str = path.file_name().ok_or_else(|| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!("Could not get file name for: {}", path.display()),
                        )
                    })?;
                    let filename_str = filename_os_str.to_string_lossy();

                    // Attempt to parse the date from the beginning of the filename.
                    // Expected format: YYYY-MM-DD-
                    let date_prefix_len = 10; // "YYYY-MM-DD" is 10 characters
                    if filename_str.len() < date_prefix_len + 1
                        || filename_str.as_bytes()[date_prefix_len] != b'-'
                    {
                        // Skip files that don't conform to the date prefix convention
                        // Or return an error, depending on desired strictness.
                        // For now, let's return an error for clarity of failure.
                        return Err(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "Filename '{}' does not start with YYYY-MM-DD-",
                                filename_str
                            ),
                        ));
                    }
                    let date_str = &filename_str[..date_prefix_len]; // "YYYY-MM-DD"

                    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").map_err(|e| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!(
                                "Failed to parse date '{}' from filename '{}': {}",
                                date_str, filename_str, e
                            ),
                        )
                    })?;

                    // Extract the title: remove the date prefix and the ".md" extension.
                    let title = filename_str
                        .strip_prefix(&format!("{}-", date_str))
                        .and_then(|s| s.strip_suffix(".md"))
                        .map(|s| s.to_string())
                        .ok_or_else(|| {
                            std::io::Error::new(
                                std::io::ErrorKind::InvalidData,
                                format!(
                                    "Could not properly parse title from filename: {}",
                                    filename_str
                                ),
                            )
                        })?;

                    // Read the entire content of the Markdown file into a String.
                    let content = fs::read_to_string(&path)?;

                    // Create a new Blog struct and add it to our vector.
                    posts.push(Blog {
                        title,
                        content,
                        date,
                    });
                }
            }
        }
    }

    // Sort the posts from latest to oldest.
    // Because we implemented `Ord` for `Blog` to sort in descending date order,
    // we can simply call `sort()`.
    posts.sort();

    Ok(posts)
}
#[cfg(test)]
mod tests {
    use crate::blog_content::read_markdown_posts;

    #[test]
    fn test_read_markdown_posts() {
        dbg!(read_markdown_posts());
    }
}
