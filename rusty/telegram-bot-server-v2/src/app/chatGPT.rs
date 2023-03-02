use super::*;
use async_openai::{types::CreateCompletionRequestArgs, Client};
use std::{
    collections::HashSet,
    fs::{File, OpenOptions},
    io::{prelude::*, BufRead, BufReader},
};
use telegram_bot::{ChatId, GroupId, MessageChat, MessageId, MessageKind};

/// receive message and return back
pub struct ChatGPT {
    vault_path: String,
    // user or group
    my_name: String,

    gpt_client: Client,

    waken_groups: HashSet<String>,

    sender: Sender<ChatGPTInput>,
    receiver: Receiver<ChatGPTInput>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl ChatGPT {
    pub fn new(deliver_sender: Sender<Msg2Deliver>, vault_path: String) -> Result<Self, String> {
        let (sender, receiver) = mpsc::channel(10);
        let f =
            BufReader::new(File::open(vault_path.clone() + "/myname").map_err(|e| e.to_string())?);
        let my_name = f
            .lines()
            .next()
            .ok_or("Read 'myname' failed".to_string())?
            .map_err(|e| e.to_string())?;

        let f = BufReader::new(
            File::open(vault_path.clone() + "/gpt_token").map_err(|e| e.to_string())?,
        );
        let gpt_token = f
            .lines()
            .next()
            .ok_or("Read 'gpt_token' failed".to_string())?
            .map_err(|e| e.to_string())?;
        let gpt_client = Client::new().with_api_key(gpt_token);

        let f = BufReader::new(
            File::open(vault_path.clone() + "/stored_groups").map_err(|e| e.to_string())?,
        );
        let stored_groups = f
            .lines()
            .filter_map(|l| l.ok())
            .collect::<HashSet<String>>();

        info!(
            "these group added to waken_groups directly: {:?}",
            stored_groups
        );

        Ok(ChatGPT {
            vault_path,
            my_name,
            sender,
            receiver,
            deliver_sender,
            gpt_client,
            waken_groups: stored_groups,
        })
    }

    fn write_to_group_list_file(&self, g_id: String) -> Result<(), String> {
        let mut f = OpenOptions::new()
            .append(true)
            .create(true)
            .open(self.vault_path.clone() + "/stored_groups")
            .map_err(|e| e.to_string())?;

        f.write_all(b"\n").map_err(|e| e.to_string())?;
        f.write_all(g_id.as_bytes()).map_err(|e| e.to_string())?;
        f.flush().map_err(|e| e.to_string())?;

        Ok(())
    }

    async fn handle_chat(&mut self, msg: ChatGPTInput) -> Result<(), String> {
        // check if chat legal or not
        let data = match (msg.group_id, msg.user_name.as_str(), msg.data.as_str()) {
            (None, name, data) => {
                if name != self.my_name {
                    match self
                        .deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            msg.chat_id,
                            String::from("not for you"),
                            None,
                        ))
                        .await
                    {
                        Err(e) => return Err(e.to_string()),
                        _ => (),
                    }
                    return Err(format!("{:?} call chat gpt", msg.user_name));
                } else {
                    data
                }
            }
            (Some(ref g_id), name, "wake_up") => {
                if name == self.my_name {
                    let reply = if self.waken_groups.contains(g_id) {
                        String::from("already")
                    } else {
                        self.waken_groups.insert(g_id.clone());
                        self.write_to_group_list_file(g_id.clone())?;
                        String::from("sure")
                    };

                    self.deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            msg.chat_id,
                            reply,
                            None,
                        ))
                        .await;
                } else {
                    error!("{} calls wake_up", name);
                    self.deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            msg.chat_id,
                            "only my owner can wake me up".into(),
                            None,
                        ))
                        .await;
                }
                return Ok(());
            }
            (Some(ref g_id), _, data) => {
                if self.waken_groups.contains(g_id) {
                    data
                } else {
                    info!("un waken group {} call", g_id);
                    return Ok(());
                }
            }
            (a, b, c) => return Err(format!("unmatched pattern: {:?}, {:?}, {:?}", a, b, c)),
        };

        // call open ai
        let request = match CreateCompletionRequestArgs::default()
            .model("text-davinci-003")
            .prompt(data)
            .max_tokens(3500_u16)
            .build()
        {
            Ok(args) => args,
            Err(e) => return Err(e.to_string()),
        };

        let result = match self.gpt_client.completions().create(request).await {
            Ok(response) => match self
                .deliver_sender
                .send(Msg2Deliver::new(
                    "reply_to".to_string(),
                    msg.chat_id,
                    format!(
                        "{}",
                        response
                            .choices
                            .first()
                            .map(|choice| format!(
                                "{}{}:\n{}",
                                msg.first_name,
                                msg.last_name
                                    .map(|ln| String::from(" ") + &ln)
                                    .unwrap_or(String::new()),
                                choice.text.trim_start_matches(['\n', ',', ' ']) //:= TODO: clean some utf8 stuff
                            ))
                            .unwrap_or("sorry, something wrong".into())
                    ),
                    Some(msg.this_message_id),
                ))
                .await
            {
                Err(e) => Err(e.to_string()),
                _ => Ok(()),
            },
            Err(e) => Err(e.to_string()),
        };

        match result {
            re @ Err(_) => {
                self.deliver_sender
                    .send(Msg2Deliver::new(
                        "send".to_string(),
                        msg.chat_id,
                        "sorry, something wrong from server".into(),
                        None,
                    ))
                    .await;
                re
            }
            _ => Ok(()),
        }
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

    first_name: String,
    last_name: Option<String>,

    chat_id: ChatId,
    group_id: Option<String>,

    this_message_id: MessageId,
}

impl ChatGPTInput {
    fn check_msg_comm(msg: &Message) -> Option<Self> {
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data.starts_with("/chat_gpt") {
                                info!("receive command {}", data);
                                if let Some(words) = data.get(en.length as usize + 1..) {
                                    return Some(Self {
                                        data: words.into(),
                                        user_name: msg
                                            .from
                                            .username
                                            .clone()
                                            .unwrap_or(String::new()),

                                        chat_id: msg.chat.id(),
                                        group_id: None,
                                        first_name: msg.from.first_name.clone(),
                                        last_name: msg.from.last_name.clone(),
                                        this_message_id: msg.id,
                                    });
                                }
                            }
                            None
                        }
                        _ => None,
                    },
                    None => None,
                }
            }

            // group
            (MessageChat::Group(group), MessageKind::Text { data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data.starts_with("/wake_up") {
                                info!("receive command {}", data);
                                return Some(Self {
                                    data: String::from("wake_up"),
                                    user_name: msg.from.username.clone().unwrap_or(String::new()),
                                    first_name: msg.from.first_name.clone(),
                                    last_name: msg.from.last_name.clone(),
                                    chat_id: msg.chat.id(),
                                    group_id: Some(group.id.to_string()),
                                    this_message_id: msg.id,
                                });
                            } else if data.starts_with("/chat_gpt") {
                                info!("receive command {}", data);
                                if let Some(words) = data.get(en.length as usize + 1..) {
                                    Some(Self {
                                        data: words.into(),
                                        user_name: msg
                                            .from
                                            .username
                                            .clone()
                                            .unwrap_or(String::new()),

                                        chat_id: msg.chat.id(),
                                        group_id: Some(group.id.to_string()),
                                        first_name: msg.from.first_name.clone(),
                                        last_name: msg.from.last_name.clone(),
                                        this_message_id: msg.id,
                                    })
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    None => None,
                }
            }
            //super group
            (MessageChat::Supergroup(group), MessageKind::Text { data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data.starts_with("/wake_up") {
                                info!("receive command {}", data);
                                return Some(Self {
                                    data: String::from("wake_up"),
                                    user_name: msg.from.username.clone().unwrap_or(String::new()),
                                    first_name: msg.from.first_name.clone(),
                                    last_name: msg.from.last_name.clone(),
                                    chat_id: msg.chat.id(),
                                    group_id: Some(group.id.to_string()),
                                    this_message_id: msg.id,
                                });
                            } else if data.starts_with("/chat_gpt") {
                                info!("receive command {}", data);
                                if let Some(words) = data.get(en.length as usize + 1..) {
                                    Some(Self {
                                        data: words.into(),
                                        user_name: msg
                                            .from
                                            .username
                                            .clone()
                                            .unwrap_or(String::new()),

                                        chat_id: msg.chat.id(),
                                        group_id: Some(group.id.to_string()),
                                        first_name: msg.from.first_name.clone(),
                                        last_name: msg.from.last_name.clone(),
                                        this_message_id: msg.id,
                                    })
                                } else {
                                    None
                                }
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
    //use super::*;

    #[test]
    fn test_parse_text() {}
}
