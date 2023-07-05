use reqwest::Client;
use rss::Channel;
use std::env;
use std::io::Cursor;
use std::{error::Error, fs::File, io::BufReader};

async fn example_feed() -> Result<Channel, Box<dyn Error>> {
    let client = Client::builder().user_agent("Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36").build()?;
    // let response = reqwest::get("https://www.sciencedaily.com/rss/top/technology.xml")
    //     .await
    //     .expect("request failed");

    let request = client
        .get("https://www.sciencedaily.com/rss/top/technology.xml")
        .build()?;

    let response = client.execute(request).await?;
    dbg!(&response);
    let content = response.text().await?;

    dbg!(&content);
    let channel = Channel::read_from(Cursor::new(content))?;

    // let file = File::open(env!("HOME").to_string() + "/Desktop/technology.xml").unwrap();
    // let channel = Channel::read_from(BufReader::new(file)).unwrap();
    Ok(channel)
}

async fn test_download() {
    let resp = reqwest::get("https://sh.rustup.rs")
        .await
        .expect("request failed");
    dbg!(&resp);
    let body = resp.text().await.expect("body invalid");
    //dbg!(body);
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    test_download().await;
    println!("{}", example_feed().await?.link());
    Ok(())
}
