use telegram_bot::{Api, ChatId, SendMessage};
use tokio::sync::mpsc::Receiver;

use tracing::info;

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
    fn new(api: Api, ch: Receiver<Msg2Deliver>) -> Self {
        Self { api, ch }
    }

    async fn run(&mut self) {
        while let Some(ref d) = self.ch.recv().await {
            match d.command.as_ref() {
                "send" => {
                    if let Err(s) = self.send_message(&d.chatid, &d.msg).await {
                        info!("{}", s);
                    }
                }
                _ => {}
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
