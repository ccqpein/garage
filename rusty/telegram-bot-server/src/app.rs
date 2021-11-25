use clap::Clap;
use futures::Future;
use futures::TryFutureExt;
use rustls::StoresClientSessions;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::pin::Pin;
use telegram_bot::{
    types::{requests::SendMessage, MessageChat, MessageKind, Update, UpdateKind},
    Api, Message,
};
use tokio::sync::mpsc;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;
use tracing::debug;
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

/// the layer after watcher and manage registering app
struct AppLayer<'app> {
    registers_match: HashMap<String, Box<dyn Fn(&Message) -> Option<Vec<String>> + 'app>>,
    registers_app: HashMap<String, Sender<Vec<String>>>,
    message_rec: Receiver<Message>,
}

impl<'app> AppLayer<'app> {
    fn new() -> (Self, Sender<Message>) {
        let (snd, rev) = mpsc::channel(10);
        (
            Self {
                registers_match: HashMap::new(),
                registers_app: HashMap::new(),
                message_rec: rev,
            },
            snd,
        )
    }

    fn register_app(&mut self, app: impl App + 'app) -> Result<(), String> {
        self.registers_match
            .insert(app.name(), Register::message_match(&app));
        self.registers_app.insert(app.name(), app.sender());
        Ok(())
    }

    async fn run(&mut self) {
        while let Some(msg) = self.message_rec.recv().await {
            'inner: for (k, v) in &self.registers_match {
                if let Some(data) = v(&msg) {
                    if let Some(snd) = self.registers_app.get(k) {
                        match snd.send(data).await {
                            Ok(_) => {
                                info!("send to {app} channel has successed", app = k)
                            }
                            Err(e) => {
                                debug!("send to {} channel has issue {}", k, e.to_string());
                            }
                        };
                        break 'inner;
                    }
                }
            }
        }
    }
}

/// this message_match function gives the function that message parser of this app
pub trait Register {
    /// return function which match the message of this register
    fn message_match<F>(&self) -> F
    where
        F: Fn(&Message) -> Option<Vec<String>>,
        Self: Sized;
}

pub trait App: Register {
    fn name(&self) -> String;

    /// this app's sender channel
    fn sender(&self) -> Sender<Vec<String>>;
}

#[cfg(test)]
mod tests {
    use super::App;
    use async_trait::async_trait;

    #[test]
    fn test_async_trait() {
        struct TestCase;

        // #[async_trait]
        // impl App for TestCase {
        //     async fn run(&self, input: &[String]) -> Result<String, String> {
        //         let inner = async {
        //             println!("inside aync");
        //             Ok("ok".to_string())
        //         };

        //         inner.await
        //     }

        //     fn match_str(&self, msg: &str) -> Option<Vec<String>> {
        //         None
        //     }
        // };
    }
}
