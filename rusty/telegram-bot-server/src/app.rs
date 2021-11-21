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

#[async_trait]
pub trait App<'a> {
    type Input;
    type Output;

    /// match if this message match this app, return necessary information
    /// from message
    fn match_str(&self, msg: &'a str) -> Option<Vec<&'a str>>;

    /// run this app
    async fn run(&self, input: Self::Input) -> Self::Output
    where
        'a: 'async_trait;
}

#[cfg(test)]
mod tests {
    use super::App;
    use async_trait::async_trait;

    #[test]
    fn test_async_trait() {
        struct TestCase;

        #[async_trait]
        impl<'a> App<'a> for TestCase {
            type Input = &'static str;
            type Output = Result<(), String>;

            async fn run(&self, input: Self::Input) -> Self::Output {
                let inner = async {
                    println!("inside aync");
                    Ok(())
                };

                inner.await
            }

            fn match_str(&self, msg: &'a str) -> Option<Vec<&'a str>> {
                None
            }
        };
    }
}
