use super::*;
use telegram_bot::{ChatId, MessageChat, MessageKind};
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Debug)]
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

pub struct EchoInputCheck {
    sender: Sender<EchoInput>,
}

#[async_trait]
impl AppConsumer for EchoInputCheck {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match EchoInput::check_msg(msg) {
            Some(input) => match self.sender.send(input).await {
                Ok(_) => return Ok(ConsumeStatus::Taken),
                Err(e) => return Err(e.to_string()),
            },
            None => Ok(ConsumeStatus::NotMine),
        }
    }
}

pub struct Echo {
    sender: Sender<EchoInput>,
    receiver: Receiver<EchoInput>,
    deliver_sender: Sender<Msg2Deliver>,
}

impl Echo {
    pub fn new(deliver_sender: Sender<Msg2Deliver>) -> Self {
        let (sender, receiver) = mpsc::channel(10);
        Self {
            deliver_sender,
            sender,
            receiver,
        }
    }

    async fn consume(&mut self, EchoInput { chatid, content }: EchoInput) -> Result<(), String> {
        match self
            .deliver_sender
            .send(Msg2Deliver::new("send".into(), chatid, content))
            .await
        {
            Ok(_) => Ok(()),
            Err(e) => {
                debug!("Error {} happens in sending reply", e.to_string());
                Ok(())
            }
        }
    }
}

#[async_trait]
impl App for Echo {
    type Consumer = EchoInputCheck;

    fn consumer(&self) -> Self::Consumer {
        EchoInputCheck {
            sender: self.sender.clone(),
        }
    }

    async fn run(mut self) -> Result<(), String> {
        info!("app echo is running");
        while let Some(msg) = self.receiver.recv().await {
            info!("echo receive message: {:?}", msg);
            match self.consume(msg).await {
                Ok(_) => continue,
                Err(e) => error!("error: {}", e),
            }
        }
        Ok(())
    }
}
