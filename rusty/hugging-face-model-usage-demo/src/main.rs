use kalosm::language::*;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Setup the model.
    // You can point to a local GGUF file or a Hugging Face repo.
    let model = Llama::builder()
        .with_source(LlamaSource::new(
            "bartowski/gemma-3-27b-it-GGUF",
            "gemma-3-27b-it-Q8_0.gguf",
            "tokenizer.json",
        ))
        .build()
        .await?;

    // 2. Create a chat session
    let mut chat = model
        .chat()
        .with_system_prompt("You are a specialized Rust programming assistant.");

    println!("Gemma 3 is ready. Ask your question:");

    // 3. Stream the response
    let mut response = chat
        .add_message("How do I implement a custom trait in Rust?")
        .await?;

    while let Some(token) = response.next().await {
        print!("{token}");
        use std::io::{self, Write};
        io::stdout().flush()?;
    }

    Ok(())
}
