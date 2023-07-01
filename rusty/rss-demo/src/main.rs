use rss::Channel;
use std::error::Error;

async fn example_feed() -> Result<Channel, Box<dyn Error>> {
    let content = reqwest::get("http://example.com/feed.xml")
        .await?
        .bytes()
        .await?;
    let channel = Channel::read_from(&content[..])?;
    Ok(channel)
}

#[tokio::main]
async fn main() {
    println!("Hello, world!");
}
