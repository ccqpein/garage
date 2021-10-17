mod regex_handler;

use regex_handler::*;
use std::io::prelude::*;
use std::io::Result;

fn handle_file(filepath: &str) -> Result<Vec<String>> {
    let mut buf = vec![];
    let mut f: std::fs::File = std::fs::File::open(filepath)?;
    f.read_to_end(&mut buf)?;
    let mut buf = buf.as_slice();

    let mut result = vec![];
    let mut cache = String::new();
    loop {
        match buf.read_line(&mut cache) {
            Ok(_) => {
                line_handler(&cache, &mut result);
                cache.clear();
            }
            Err(_) => break,
        }
    }

    Ok(result)
}

fn line_handler(s: &str, bucket: &mut Vec<String>) {
    match capture_title(s) {
        Some(cap) => match pick_the_head(&cap) {
            Ok(m) => {
                let space_len = head_count(&m) - 1;
                let content = pick_the_head_content(&cap);
                //:= link
                //:= bucket.push()
            }
            Err(_) => todo!(),
        },
        None => return,
    }
}
