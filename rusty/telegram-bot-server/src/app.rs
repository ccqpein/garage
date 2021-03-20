use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufReader};
use telegram_bot::{
    types::{requests::SendMessage, MessageKind, Update, UpdateKind},
    Api,
};
use tracing::info;

mod github_api;

pub async fn update_router(update: Update, api: &Api) -> Result<(), String> {
    match update.kind {
        UpdateKind::Message(message) => match message.kind {
            MessageKind::Text { ref data, .. } => {
                info!("Receive message data: {:?}", data);
                let mes = if data.to_lowercase() == "commit" {
                    my_github_commits(message.from.username.unwrap_or(String::new()))
                        .await
                        .unwrap_or("Inner error".to_string())
                } else {
                    format!(
                        "Hi, {}! You just wrote '{}'",
                        &message.from.first_name, data
                    )
                };
                api.send(SendMessage::new(message.chat, mes))
                    .await
                    .map_err(|e| e.to_string())?;
            }
            _ => {}
        },
        _ => {}
    };

    Ok(())
}

async fn my_github_commits(username: String) -> Result<String, String> {
    let f = BufReader::new(File::open("./vault/myname").map_err(|e| e.to_string())?);
    let myname = f
        .lines()
        .next()
        .ok_or("Read 'myname' failed".to_string())?
        .map_err(|e| e.to_string())?;

    if username == myname {
        if github_api::does_this_user_have_commit_today("ccqpein", Some("./vault/githubtoken"))
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
