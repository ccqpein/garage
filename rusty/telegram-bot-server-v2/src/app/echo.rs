use async_trait::async_trait;
use telegram_bot::{ChatId, MessageChat, MessageKind};
use tokio::sync::mpsc::Sender;
use tracing::debug;

use super::*;

pub struct EchoInput {
    content: String,
    chatid: ChatId,
}

impl AppInput for EchoInput {
    fn parse_input(msg: &telegram_bot::Message) -> Option<Self>
    where
        Self: Sized,
    {
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(Self {
                content: data.to_string(),
                chatid: msg.chat.id(),
            }),
            _ => None,
        }
    }
}

pub struct Echo {
    sender: Sender<Msg2Deliver>,
}

#[async_trait]
impl App for Echo {
    type Input = EchoInput;

    async fn run(&mut self, EchoInput { content, chatid }: Self::Input) -> Result<(), String> {
        match self
            .sender
            .send(Msg2Deliver::new("send".into(), chatid, content))
            .await
        {
            Ok(_) => Ok(()),
            Err(e) => {
                debug!("Error {} happens in sending reply", e.to_string());
                Err(e.to_string())
            }
        }
    }
}
