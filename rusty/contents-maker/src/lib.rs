mod regex_handler;

use regex_handler::*;
use std::io::prelude::*;
use std::io::Result;

pub fn handle_file(filepath: &str) -> Result<Vec<String>> {
    let mut buf = vec![];
    let mut f: std::fs::File = std::fs::File::open(filepath)?;
    f.read_to_end(&mut buf)?;
    let mut buf = buf.as_slice();

    let mut result = vec![];
    let mut cache = String::new();
    loop {
        match buf.read_line(&mut cache) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                match line_handler(&cache, &mut result) {
                    Ok(_) => cache.clear(),
                    Err(_) => (),
                };
            }
        }
    }

    Ok(result)
}

fn line_handler(s: &str, bucket: &mut Vec<String>) -> std::result::Result<(), String> {
    match capture_title(s) {
        Some(cap) => {
            let m = pick_the_head(&cap)?;
            let space_len = head_count(&m) - 1;
            let content = pick_the_head_content(&cap)?;
            let content = content.trim_end_matches(" ");
            let line = format!(
                "{}-[{}](#{})",
                std::iter::repeat("  ").take(space_len).collect::<String>(),
                content,
                content.split(" ").collect::<Vec<_>>().join("-")
            );
            bucket.push(line);
            Ok(())
        }
        None => Err(format!("capture title failed: {}", s)),
    }
}

#[cfg(test)]
mod tests {
    use crate::line_handler;

    #[test]
    fn test_line_handler() -> Result<(), String> {
        let mut bucket = vec![];

        let case = "## level 2 ##";
        line_handler(case, &mut bucket)?;
        assert_eq!(bucket[0], "  -[level 2](#level-2)".to_string());

        let case = "# level 1  ";
        line_handler(case, &mut bucket)?;
        assert_eq!(bucket[1], "-[level 1](#level-1)".to_string());

        Ok(())
    }
}
