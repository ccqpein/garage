use dioxus::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize, Serialize)]
pub struct AllBlog {
    map: HashMap<String, Blog>,
    ordered_blogs: Vec<String>,
}

impl AllBlog {
    pub fn all_titles(&self) -> &[String] {
        &self.ordered_blogs
    }
}

#[derive(Deserialize, Serialize)]
pub struct Blog {
    title: String,
    content: String,
}

#[server]
pub async fn all_blogs() -> Result<HashMap<String, Blog>, ServerFnError> {
    Ok(vec![
        (
            "aa".to_string(),
            Blog {
                title: "aa".to_string(),
                content: "aa".to_string(),
            },
        ),
        (
            "bb".to_string(),
            Blog {
                title: "bb".to_string(),
                content: "bb".to_string(),
            },
        ),
        (
            "cc".to_string(),
            Blog {
                title: "cc".to_string(),
                content: "cc".to_string(),
            },
        ),
    ]
    .into_iter()
    .collect())
}
