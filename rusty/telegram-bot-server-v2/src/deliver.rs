use telegram_bot::{Api, ChatId, MessageId, SendMessage};
use tokio::sync::mpsc::Receiver;

use tracing::{debug, info};

#[derive(Debug, Clone)]
pub struct Msg2Deliver {
    command: String,
    chatid: ChatId,
    msg: String,
    reply_to: Option<MessageId>,
}

impl Msg2Deliver {
    pub fn new(command: String, chatid: ChatId, msg: String, reply_to: Option<MessageId>) -> Self {
        Self {
            command,
            chatid,
            msg,
            reply_to,
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
        info!("Deliver is running");
        while let Some(ref d) = self.ch.recv().await {
            match d.command.as_ref() {
                "send" => {
                    if let Err(s) = self.send_message(&d.chatid, &d.msg).await {
                        info!("{}", s);
                    }
                }
                "reply_to" => {
                    if let Err(s) = self.reply_message(&d.chatid, &d.reply_to, &d.msg).await {
                        info!("{}", s);
                    }
                }
                other @ _ => {
                    debug!("Deliver doesn't support {} command yet", other)
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

    async fn reply_message(
        &self,
        id: &ChatId,
        reply_to_id: &Option<MessageId>,
        msg: &str,
    ) -> Result<(), String> {
        match reply_to_id {
            Some(m_id) => {
                self.api
                    .send(SendMessage::new(id, msg).reply_to(m_id))
                    .await
                    .map_err(|e| e.to_string())?;
                Ok(())
            }
            None => Err("reply message id is none".to_string()),
        }
    }
}
