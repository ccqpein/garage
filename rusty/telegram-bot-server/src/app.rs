use clap::Clap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use telegram_bot::{
    types::{requests::SendMessage, MessageChat, MessageKind, Update, UpdateKind},
    Api, Message,
};
use tokio::sync::mpsc::Sender;
use tracing::info;

mod github_api;
pub use github_api::my_github_commits;

#[derive(Default, Clap, Clone)]
pub struct Opts {
    #[clap(short, long)]
    pub vault: String,
}

pub async fn update_router(
    update: Update,
    api: &Api,
    opts: &Opts,
    channel: &Sender<Message>,
) -> Result<(), String> {
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
