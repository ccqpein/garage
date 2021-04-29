use async_std::sync::Mutex;
use clap::Clap;
use std::io::BufReader;
use std::{fs::File, sync::Arc};
use std::{io::prelude::*, ops::Deref};
use telegram_bot::{
    types::{requests::SendMessage, MessageChat, MessageKind, Update, UpdateKind},
    Api, Message,
};
use tokio::sync::mpsc::Sender;
use tracing::info;

mod github_api;

#[derive(Default, Clap, Clone)]
pub struct Opts {
    #[clap(short, long)]
    pub vault: String,
}

/// Keep some status
pub struct Status {}

impl Status {
    fn pending_add_remind() {}
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
                    // commit
                    "commit" => {
                        let msg = my_github_commits(
                            message.from.username.unwrap_or(String::new()),
                            &opts.vault,
                        )
                        .await
                        .unwrap_or("Inner error".to_string());

                        let _ = api
                            .send(SendMessage::new(ch, msg))
                            .await
                            .map_err(|e| e.to_string())?;
                    }
                    // remind
                    //"remind" => {}
                    _ => {
                        channel.send(message.clone()).await;
                    }
                }
                // api.send(SendMessage::new(ch, mes))
                //     .await
                //     .map_err(|e| e.to_string())?;
            }
            _ => {}
        },
        _ => {}
    }

    Ok(())
}

async fn my_github_commits(username: String, vault: &String) -> Result<String, String> {
    let f = BufReader::new(File::open(vault.clone() + "/myname").map_err(|e| e.to_string())?);
    let myname = f
        .lines()
        .next()
        .ok_or("Read 'myname' failed".to_string())?
        .map_err(|e| e.to_string())?;

    if username == myname {
        if github_api::does_this_user_have_commit_today(
            "ccqpein",
            Some(vault.clone() + "/githubtoken"),
        )
        .await?
        {
            Ok(format!("You have commit today"))
        } else {
            Ok(format!("You haven't commit today yet"))
        }
    } else {
        Ok("Not for you".to_string())
    }
}
