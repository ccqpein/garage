use async_trait::async_trait;
use telegram_bot::{ChatId, MessageChat, MessageKind};
use tokio::sync::mpsc::Sender;
use tracing::debug;

use super::*;

pub struct EchoInput {
    content: String,
    chatid: ChatId,
}

impl<'m> AppInput<'m> for EchoInput {
    fn parse_input(msg: &'m telegram_bot::Message) -> Option<Self>
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

    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> {
        self
    }
}

pub struct Echo {
    sender: Sender<Msg2Deliver>,
}

#[async_trait]
impl App for Echo {
    //type Input = EchoInput;

    async fn run<'m, 's: 'm>(
        &'s mut self,
        //EchoInput { content, chatid }: EchoInput,
        ei: Box<dyn AppInput + Send + 'm>,
    ) -> Result<(), String> {
        let ei: EchoInput = Box::<EchoInput>::into_inner(ei.into_any().downcast().unwrap());

        match self
            .sender
            .send(Msg2Deliver::new("send".into(), ei.chatid, ei.content))
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
