use telegram_bot::{Api, ChatId, Message, MessageId, MessageOrChannelPost, SendMessage};
use tokio::sync::mpsc::Receiver;

use tracing::{debug, info};

use crate::app::insert_new_reply;
use crate::app::DB;

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
                // reply to only used by chat gpt so far.
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
                let resp = self
                    .api
                    .send(SendMessage::new(id, msg).reply_to(m_id))
                    .await
                    .map_err(|e| e.to_string())?;
                debug!("reply response: {:?}", resp);

                let (m, resp_content) = get_reply_text_msg(&resp)?;

                insert_new_reply(m, "assistant", None, DB.lock().await.as_ref().unwrap())
                    .await
                    .map_err(|e| e.to_string())?;

                Ok(())
            }
            None => Err("reply message id is none".to_string()),
        }
    }
}

fn get_reply_text_msg(mm: &MessageOrChannelPost) -> Result<(&Message, String), String> {
    match mm {
        MessageOrChannelPost::Message(m) => match &m.kind {
            telegram_bot::MessageKind::Text { data, .. } => Ok((m, data.to_string())),

            _ => Err("only support text so far".into()),
        },
        MessageOrChannelPost::ChannelPost(_) => Err("only support Message so far".into()),
    }
}
