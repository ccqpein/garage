mod embedding_demo;
mod start_up;

use embedding_demo::*;
use qdrant_client::QdrantError;
use start_up::*;

#[tokio::main]
async fn main() -> Result<(), QdrantError> {
    //dbg!(start_up_demo().await?);
    dbg!(call().await);
    Ok(())
}
