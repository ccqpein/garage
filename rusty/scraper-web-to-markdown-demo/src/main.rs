use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::io::Cursor;
use std::path::PathBuf;
use url::Url;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// The URL to query
    #[arg(short, long)]
    url: String,

    /// Output path (directory or filename). Defaults to current folder.
    #[arg(short, long, value_name = "PATH")]
    output: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let url_str = cli.url;
    let url = Url::parse(&url_str).context("Failed to parse URL")?;

    println!("Fetching {}", url);

    // Fetch the content
    let client = reqwest::blocking::Client::new();
    let response = client.get(url.clone()).send().context("Failed to fetch URL")?;
    let content_bytes = response.bytes().context("Failed to read response body")?;
    let mut reader = Cursor::new(content_bytes);

    // Extract content using readability
    let product = readability::extractor::extract(&mut reader, &url)
        .map_err(|e| anyhow::anyhow!("Readability extraction failed: {:?}", e))?;

    let title = product.title;
    let clean_html = product.content;

    println!("Title: {}", title);

    // Convert to Markdown
    let markdown = html2md::parse_html(&clean_html);
    
    // Add title to the beginning of markdown
    let final_markdown = format!("# {}

{}", title, markdown);

    // Determine output path
    let output_path = match cli.output {
        Some(path) => {
            if path.exists() && path.is_dir() {
                path.join(sanitize_filename(&title) + ".md")
            } else {
                path
            }
        },
        None => {
            PathBuf::from(sanitize_filename(&title) + ".md")
        }
    };

    // Ensure parent directory exists
    if let Some(parent) = output_path.parent() {
        if !parent.as_os_str().is_empty() && !parent.exists() {
             fs::create_dir_all(parent).context("Failed to create output directory")?;
        }
    }

    fs::write(&output_path, final_markdown).context("Failed to write output file")?;

    println!("Saved to {:?}", output_path);

    Ok(())
}

fn sanitize_filename(filename: &str) -> String {
    let invalid_chars = ['/', '\\', '?', '%', '*', ':', '|', '"', '<', '>','.'];
    filename
        .chars()
        .map(|c| if invalid_chars.contains(&c) || c.is_control() { '_' } else { c })
        .collect()
}