//! call those AI platform embedding api and trying to connect with qdrant

use serde::{Deserialize, Serialize};
use std::error::Error;
use std::{env, marker::PhantomData};

#[derive(Debug)]
struct APIClient<Req: Serialize, Resp: for<'a> Deserialize<'a>> {
    client: reqwest::Client,
    endpoint: String,

    api_key_name: String,
    api_key: String,

    model_version: String,

    req: PhantomData<Req>,
    resp: PhantomData<Resp>,
}

#[derive(Debug)]
struct APIError {}

impl std::fmt::Display for APIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "api call has error")
    }
}
impl Error for APIError {}

impl<Req: Serialize, Resp: for<'a> Deserialize<'a>> APIClient<Req, Resp> {
    fn new(endpoint: String, api_key_name: String, api_key: String, model_version: String) -> Self {
        Self {
            client: reqwest::Client::new(),
            endpoint,

            api_key_name,
            api_key,

            model_version,

            req: PhantomData,
            resp: PhantomData,
        }
    }

    async fn call_api(&self, req: Req) -> Result<Resp, Box<dyn Error>> {
        let resp = self
            .client
            .post(self.endpoint.clone())
            .header(self.api_key_name.clone(), self.api_key.clone())
            .header("Content-Type", "application/json")
            .json(&req)
            .send()
            .await?;

        if resp.status().is_success() {
            Ok(resp.json::<Resp>().await?)
        } else {
            Err(Box::new(APIError {}))
        }
    }
}

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

#[derive(Deserialize, Debug)]
struct Embedding {
    values: Vec<f32>,
}

#[derive(Deserialize, Debug)]
struct EmbedContentResponse {
    embedding: Embedding,
}

// entry function for calling
// hard code everything
pub async fn call() -> Result<(), Box<dyn std::error::Error>> {
    let google_client = APIClient::new(
        "https://generativelanguage.googleapis.com/v1beta/models/embedding-001:embedContent"
            .to_string(),
        "x-goog-api-key".to_string(),
        env::var("GEMINI_API_KEY")
            .expect("GEMINI_API_KEY must be set in your environment variables"),
        "models/embedding-001".to_string(),
    );

    let text_to_embed = "Hello, world! This is a test for the Gemini Embedding API.";

    let request_body = EmbedContentRequest {
        content: Content {
            parts: vec![Part {
                text: text_to_embed.to_string(),
            }],
        },
        model_id: google_client.model_version.clone(),
    };

    println!("Sending request to Gemini Embedding API...");
    println!("Text to embed: \"{}\"", text_to_embed);

    let embedding_response: EmbedContentResponse = google_client.call_api(request_body).await?;
    println!("\nSuccessfully received embedding from Gemini API:");
    println!("\n{:?}", embedding_response);

    Ok(())
}

//////////////
// save to Qdrant below
////////
