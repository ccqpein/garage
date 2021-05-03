use telegram_bot::{Api, ChatId, SendMessage};
use tokio::sync::mpsc::Receiver;

use tracing::{debug, info};

#[derive(Debug)]
pub struct Msg2Deliver {
    command: String,
    chatid: ChatId,
    msg: String,
}

impl Msg2Deliver {
    pub fn new(command: String, chatid: ChatId, msg: String) -> Self {
        Self {
            command,
            chatid,
            msg,
        }
    }
}

pub struct Deliver {
    api: Api,
    ch: Receiver<Msg2Deliver>,
}

impl Deliver {
    pub fn new(api: Api, ch: Receiver<Msg2Deliver>) -> Self {
        Self { api, ch }
    }

    pub async fn run(&mut self) {
        while let Some(ref d) = self.ch.recv().await {
            match d.command.as_ref() {
                "send" => {
                    if let Err(s) = self.send_message(&d.chatid, &d.msg).await {
                        info!("{}", s);
                    }
                }
                other @ _ => {
                    debug!("Deliver doesn't support {} command yer", other)
                }
            }
        }
    }

    async fn send_message(&self, id: &ChatId, msg: &str) -> Result<(), String> {
        self.api
            .send(SendMessage::new(id, msg))
            .await
            .map_err(|e| e.to_string())?;

        Ok(())
    }
}
