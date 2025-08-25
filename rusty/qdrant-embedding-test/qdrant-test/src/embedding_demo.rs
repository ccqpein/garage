//! call those AI platform embedding api and trying to connect with qdrant

use qdrant_client::qdrant::{
    vector, Condition, CreateCollectionBuilder, Distance, Filter, PointId, PointStruct,
    ScalarQuantizationBuilder, SearchParamsBuilder, SearchPointsBuilder, UpsertPointsBuilder,
    VectorParamsBuilder,
};
use qdrant_client::{Payload, Qdrant};
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::{env, marker::PhantomData};
use uuid::Uuid;

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
        dbg!(serde_json::json!(req));
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

#[derive(Serialize, Debug, Default)]
struct Content {
    parts: Vec<Part>,
}

#[derive(Serialize, Debug)]
struct EmbeddingConfig {
    output_dimensionality: u64,
}

#[derive(Serialize, Debug, Default)]
struct EmbedContentRequest {
    content: Content,
    #[serde(rename = "model")]
    model_id: String,

    #[serde(skip_serializing_if = "Option::is_none")]
    embedding_config: Option<EmbeddingConfig>,
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
pub async fn call(msg: Option<&'static str>) -> Result<Vec<f32>, Box<dyn std::error::Error>> {
    let google_client = APIClient::new(
        "https://generativelanguage.googleapis.com/v1beta/models/embedding-001:embedContent"
            .to_string(),
        "x-goog-api-key".to_string(),
        env::var("GEMINI_API_KEY")
            .expect("GEMINI_API_KEY must be set in your environment variables"),
        "models/embedding-001".to_string(),
    );

    let text_to_embed = msg.unwrap_or("Hello, world! This is a test for the Gemini Embedding API.");

    let request_body = EmbedContentRequest {
        content: Content {
            parts: vec![Part {
                text: text_to_embed.to_string(),
            }],
        },
        model_id: google_client.model_version.clone(),
        ..Default::default()
    };

    println!("Sending request to Gemini Embedding API...");
    println!("Text to embed: \"{}\"", text_to_embed);

    let embedding_response: EmbedContentResponse = google_client.call_api(request_body).await?;
    println!("\nSuccessfully received embedding from Gemini API");
    //println!("\n{:?}", &embedding_response);

    Ok(embedding_response.embedding.values)
}

//////////////
// save to Qdrant below
////////

pub async fn save_and_load_from_qbrant() -> Result<(), Box<dyn std::error::Error>> {
    let qdclient = Qdrant::from_url("http://localhost:6334").build()?;

    // assume I already has collections "demo"
    // in start_up.rs
    let collection_name = "demo";

    // clean up
    qdclient.delete_collection(collection_name).await?;
    qdclient
        .create_collection(
            CreateCollectionBuilder::new(collection_name)
                // the size is matter: 768, 1536, or 3072
                // https://ai.google.dev/gemini-api/docs/embeddings#control-embedding-size
                .vectors_config(VectorParamsBuilder::new(768, Distance::Cosine))
                .quantization_config(ScalarQuantizationBuilder::default()),
        )
        .await?;

    // some demo payload
    let payload: Payload = serde_json::json!(
        {
            "foo": "Bar",
            "bar": 12,
            "baz": {
                "qux": "quux"
            }
        }
    )
    .try_into()
    .unwrap();

    let vectors = call(None).await?;
    let id = Uuid::new_v4().to_string();
    let points = vec![PointStruct::new(id.clone(), vectors, payload)];
    println!("this point id is {}", id);

    qdclient
        .upsert_points(UpsertPointsBuilder::new(collection_name, points))
        .await?;

    // start to query
    let query_vec = call(Some("Hello, world! This is a test for something")).await?;
    let dim = query_vec.len() as u64;
    let search_result = qdclient
        .search_points(
            SearchPointsBuilder::new(collection_name, query_vec, dim)
                .filter(Filter::all([Condition::matches("bar", 12)]))
                .with_payload(true)
                .params(SearchParamsBuilder::default().exact(true)),
        )
        .await?;

    dbg!(&search_result);

    // try the other one
    let query_vec = call(Some("Hello, world! This is from the Gemini Embedding API.")).await?;
    let dim = query_vec.len() as u64;
    let search_result = qdclient
        .search_points(
            SearchPointsBuilder::new(collection_name, query_vec, dim)
                .filter(Filter::all([Condition::matches("foo", "Bar".to_string())]))
                .with_payload(true)
                .params(SearchParamsBuilder::default().exact(true)),
        )
        .await?;

    dbg!(&search_result);

    Ok(())
}
