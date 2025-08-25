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
    Ok(())
}
