use async_trait::async_trait;
use telegram_bot::{ChatId, MessageChat, MessageKind};
use tokio::sync::mpsc::{Receiver, Sender};
use tracing::debug;

use super::*;

pub struct EchoInput {
    content: String,
    chatid: ChatId,
}

impl EchoInput {
    fn check_msg(msg: &Message) -> Option<Self> {
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(Self {
                content: data.to_string(),
                chatid: msg.chat.id(),
            }),
            _ => None,
        }
    }
}

// impl AppInput for EchoInput {
// fn parse_input(msg: &'m telegram_bot::Message) -> Option<Self>
// where
//     Self: Sized,
// {
//     match (&msg.chat, &msg.kind) {
//         (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(Self {
//             content: data.to_string(),
//             chatid: msg.chat.id(),
//         }),
//         _ => None,
//     }
// }

// fn into_any(self: Box<Self>) -> Box<dyn std::any::Any> {
//     self
// }

//     fn parse_input(msg: &telegram_bot::Message) -> Option<Self>
//     where
//         Self: Sized,
//     {
//         match (&msg.chat, &msg.kind) {
//             (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(Self {
//                 content: data.to_string(),
//                 chatid: msg.chat.id(),
//             }),
//             _ => None,
//         }
//     }
// }

pub struct Echo {
    //sender: Sender<EchoInput>,
    //receiver: Receiver<EchoInput>,
    deliver_sender: Sender<Msg2Deliver>,
}

impl Echo {
    pub fn new(deliver_sender: Sender<Msg2Deliver>) -> Self {
        Self { deliver_sender }
    }
}

#[async_trait]
impl App for Echo {
    async fn consume(&mut self, msg: &Message) -> Option<()> {
        let (chatid, content) =
            if let Some(EchoInput { chatid, content }) = EchoInput::check_msg(msg) {
                (chatid, content)
            } else {
                return None;
            };

        match self
            .deliver_sender
            .send(Msg2Deliver::new("send".into(), chatid, content))
            .await
        {
            Ok(_) => Some(()),
            Err(e) => {
                debug!("Error {} happens in sending reply", e.to_string());
                Some(())
                //Err(e.to_string())
            }
        }
    }

    //type Input = EchoInput;

    // async fn run<'m, 's: 'm>(
    //     &'s mut self,
    //     //EchoInput { content, chatid }: EchoInput,
    //     ei: Box<dyn AppInput + Send + 'm>,
    // ) -> Result<(), String> {
    //     let ei: EchoInput = Box::<EchoInput>::into_inner(ei.into_any().downcast().unwrap());

    //     match self
    //         .sender
    //         .send(Msg2Deliver::new("send".into(), ei.chatid, ei.content))
    //         .await
    //     {
    //         Ok(_) => Ok(()),
    //         Err(e) => {
    //             debug!("Error {} happens in sending reply", e.to_string());
    //             Err(e.to_string())
    //         }
    //     }
    // }

    // fn sender(&self) -> Sender<Self::Input> {
    //     self.sender.clone()
    // }
}
