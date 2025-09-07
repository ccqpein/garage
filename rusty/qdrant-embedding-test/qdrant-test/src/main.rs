mod embedding_demo;
mod start_up;

use embedding_demo::*;
use qdrant_client::QdrantError;
use start_up::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    //dbg!(start_up_demo().await?);
    //dbg!(call(None).await);
    save_and_load_from_qbrant().await?;
    // call_gemma(&vec![
    //     "This is a test sentence.",
    //     "What about another one?",
    //     "Hello, world!",
    // ])
    //     .await?;

    save_and_load_from_qbrant_gemma().await?;
    Ok(())
}
