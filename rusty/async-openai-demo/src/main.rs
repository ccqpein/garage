use async_openai::{
    types::{CreateCompletionRequestArgs, CreateImageRequestArgs, ImageSize, ResponseFormat},
    Client,
};
use reqwest::Response;
use serde_json::json;

async fn old_api_call() {
    let client = Client::new();
    let request = CreateCompletionRequestArgs::default()
        .model("text-davinci-003")
        //.model("gpt-3.5-turbo") // doesn't work
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

async fn chat_api_call() -> Result<serde_json::Value, reqwest::Error> {
    let resp = reqwest::Client::new();
    let body = json!({
        "model": "gpt-3.5-turbo",
        "messages": json!([
            json!({
                "role": "system", "content": "You are a helpful assistant."
            }),
            json!({"role": "user", "content": "Who won the world series in 2020?"}),
            json!({"role": "assistant", "content": "The Los Angeles Dodgers won the World Series in 2020."}),
            json!({"role": "user", "content": "Where was it played?"})
        ])
    });

    println!("body: {}", body.to_string());

    resp.post("https://api.openai.com/v1/chat/completions")
        .bearer_auth(std::env!("OPENAI_API_KEY"))
        .header("Content-Type", "application/json")
        //         .body(
        //             r#"{
        //   "model": "gpt-3.5-turbo",
        //   "messages": [
        //         {"role": "system", "content": "You are a helpful assistant."},
        //         {"role": "user", "content": "Who won the world series in 2020?"},
        //         {"role": "assistant", "content": "The Los Angeles Dodgers won the World Series in 2020."},
        //         {"role": "user", "content": "Where was it played?"}
        //     ]
        // }"#,
        //         )
        .body(body.to_string())
        .send()
        .await?
        .json::<serde_json::Value>()
        .await
}

#[tokio::main]
async fn main() {
    //old_api_call().await;
    let va = chat_api_call().await.unwrap();
    let role = &va["choices"][0]["message"]["role"];
    let content = &va["choices"][0]["message"]["content"];
    println!("role: {role}, content: {content}");
}
