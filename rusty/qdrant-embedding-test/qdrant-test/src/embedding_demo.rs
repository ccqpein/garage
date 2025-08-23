//! call those AI platform embedding api and trying to connect with qdrant

use serde::{Deserialize, Serialize};
use std::env;

//:= next
struct APIClient {}

#[derive(Serialize, Debug)]
struct Part {
    text: String,
}

#[derive(Serialize, Debug)]
struct Content {
    parts: Vec<Part>,
}

#[derive(Serialize, Debug)]
struct EmbedContentRequest {
    content: Content,
    #[serde(rename = "model")]
    model_id: String,
}

// Response
#[derive(Deserialize, Debug)]
struct Embedding {
    values: Vec<f32>,
}

#[derive(Deserialize, Debug)]
struct EmbedContentResponse {
    embedding: Embedding,
}

pub async fn call() -> Result<(), Box<dyn std::error::Error>> {
    let api_key = env::var("GEMINI_API_KEY")
        .expect("GEMINI_API_KEY must be set in your environment variables");

    let text_to_embed = "Hello, world! This is a test for the Gemini Embedding API.";

    let endpoint =
        "https://generativelanguage.googleapis.com/v1beta/models/embedding-001:embedContent";
    let model_id = "models/embedding-001".to_string();

    let request_body = EmbedContentRequest {
        content: Content {
            parts: vec![Part {
                text: text_to_embed.to_string(),
            }],
        },
        model_id,
    };

    let client = reqwest::Client::new();

    println!("Sending request to Gemini Embedding API...");
    println!("Text to embed: \"{}\"", text_to_embed);

    let response = client
        .post(endpoint)
        .header("x-goog-api-key", api_key)
        .header("Content-Type", "application/json")
        .json(&request_body)
        .send()
        .await?;

    if response.status().is_success() {
        let embedding_response: EmbedContentResponse = response.json().await?;
        println!("\nSuccessfully received embedding from Gemini API:");
        println!("\n{:?}", embedding_response);
    }

    Ok(())
}
