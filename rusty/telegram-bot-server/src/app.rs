use clap::Clap;
use futures::Future;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::pin::Pin;
use telegram_bot::{
    types::{requests::SendMessage, MessageChat, MessageKind, Update, UpdateKind},
    Api, Message,
};
use tokio::sync::mpsc::Sender;
use tracing::info;

use async_trait::async_trait;

mod github_api;
mod reminder;

pub use github_api::*;
pub use reminder::*;

/// app layer opts
#[derive(Default, Clap, Clone)]
pub struct Opts {
    #[clap(short, long)]
    pub vault: String,
}

pub async fn update_router(update: Update, channel: &Sender<Message>) -> Result<(), String> {
    match update.kind {
        UpdateKind::Message(message) => match (&message.kind, &message.chat) {
            // message from private
            (MessageKind::Text { ref data, .. }, ch @ MessageChat::Private(_)) => {
                info!("Receive message data: {:?}", data);
                match data.to_lowercase().as_str() {
                    _ => {
                        info!("Send message to Watcher: {:?}", message);
                        channel
                            .send(message.clone())
                            .await
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
            _ => {}
        },
        _ => {}
    }

    Ok(())
}

pub trait AppInput {}
pub trait AppOutput {}

impl AppInput for str {}
impl AppOutput for Result<(), String> {}
impl AppOutput for Result<String, String> {}

#[async_trait]
pub trait App {
    /// match if this message match this app, return necessary information
    /// from message
    fn match_str(&self, msg: &str) -> Option<Vec<&str>>;

    /// run this app
    async fn run(&self, input: &[&str]) -> Result<String, String>;
}

// pub fn app_picker(msg: &str) -> Box<&dyn App> {
//     todo!()
// }

#[cfg(test)]
mod tests {
    use super::App;
    use async_trait::async_trait;

    #[test]
    fn test_async_trait() {
        struct TestCase;

        #[async_trait]
        impl App for TestCase {
            async fn run(&self, input: &[&str]) -> Result<String, String> {
                let inner = async {
                    println!("inside aync");
                    Ok("ok".to_string())
                };

                inner.await
            }

            fn match_str(&self, msg: &str) -> Option<Vec<&str>> {
                None
            }
        };
    }
}
