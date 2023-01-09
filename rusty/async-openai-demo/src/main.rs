use async_openai::{
    types::{CreateCompletionRequestArgs, CreateImageRequestArgs, ImageSize, ResponseFormat},
    Client,
};

#[tokio::main]
async fn main() {
    let client = Client::new();
    let request = CreateCompletionRequestArgs::default()
        .model("text-davinci-003")
        .prompt("Tell me the recipe of alfredo pasta")
        .max_tokens(40_u16)
        .build()
        .unwrap();

    let response = client
        .completions() // Get the API "group" (completions, images, etc.) from the client
        .create(request) // Make the API call in that "group"
        .await
        .unwrap();

    println!("{}", response.choices.first().unwrap().text);
}
