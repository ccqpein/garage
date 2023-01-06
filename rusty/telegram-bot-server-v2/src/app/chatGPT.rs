use super::*;
use telegram_bot::{ChatId, MessageChat, MessageKind, UserId};

/// receive message and return back
struct ChatGPT {
    // user or group
    sender: Sender<ChatGPTInput>,
    receiver: Receiver<ChatGPTInput>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl ChatGPT {
    fn new(deliver_sender: Sender<Msg2Deliver>, vault_path: String) -> Self {
        let (sender, receiver) = mpsc::channel(10);
        ChatGPT {
            sender,
            receiver,
            deliver_sender,
        }
    }

    async fn handle_chat(&self, msg: ChatGPTInput) -> Result<(), String> {
        // check if legal or not

        // call open ai
        // send to deliver

        Ok(())
    }
}

#[async_trait]
impl App for ChatGPT {
    type Consumer = ChatGPTInputConsumer;

    fn consumer(&self) -> Self::Consumer {
        ChatGPTInputConsumer {
            sender: self.sender.clone(),
        }
    }

    async fn run(mut self) -> Result<(), String> {
        info!("ChatGPT is running");
        while let Some(msg) = self.receiver.recv().await {
            match self.handle_chat(msg).await {
                Ok(_) => continue,
                Err(e) => error!("error: {}", e),
            }
        }
        Ok(())
    }
}

pub struct ChatGPTInput {
    data: String,
    user_name: String,
    chat_id: ChatId, // group id or private chat id
}

impl ChatGPTInput {
    fn check_msg_comm(msg: &Message) -> Option<Self> {
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(user), MessageKind::Text { data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            info!("receive command {}", data);
                            if data.starts_with("/chat_gpt") {
                                Some(Self {
                                    data: data[en.length as usize + 1..].into(),
                                    user_name: msg.from.username.clone().unwrap_or(String::new()),
                                    chat_id: msg.chat.id(),
                                })
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    None => None,
                }
            }
            _ => None,
        }
    }
}

pub struct ChatGPTInputConsumer {
    sender: Sender<ChatGPTInput>,
}

#[async_trait]
impl AppConsumer for ChatGPTInputConsumer {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match ChatGPTInput::check_msg_comm(msg) {
            Some(input) => match self.sender.send(input).await {
                Ok(_) => return Ok(ConsumeStatus::Taken),
                Err(e) => return Err(e.to_string()),
            },
            None => Ok(ConsumeStatus::NotMine),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_text() {}
}
