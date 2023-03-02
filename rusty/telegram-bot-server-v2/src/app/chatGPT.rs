use super::*;
use async_openai::{types::CreateCompletionRequestArgs, Client};
use lazy_static::*;
use serde_json::{json, Value};
use std::{
    collections::{HashMap, HashSet},
    fs::{File, OpenOptions},
    io::{prelude::*, BufRead, BufReader},
};
use telegram_bot::{ChatId, GroupId, MessageChat, MessageId, MessageKind, MessageText};
use tokio::sync::Mutex;

/// add tables for storing the chat
lazy_static! {
    static ref CHAT_CHAIN_TABLE: Mutex<HashMap<MessageId, Option<MessageId>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
    static ref CHAT_DETAIL_TABLE: Mutex<HashMap<MessageId, ChatDetail>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

pub struct ChatDetail {
    role: String,
    message: String,
}

impl ChatDetail {
    fn new(role: &str, message: &str) -> Self {
        Self {
            role: role.into(),
            message: message.into(),
        }
    }

    fn to_json_message(&self) -> Value {
        json!({"role": &self.role, "content": &self.message})
    }
}

pub async fn if_reply_chat_in_the_chain(msg: &Message) -> bool {
    match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(mm) => if_in_chain(&mm.id).await,
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => false,
        },
        None => false,
    }
}

pub async fn if_in_chain(this_id: &MessageId) -> bool {
    CHAT_CHAIN_TABLE.lock().await.contains_key(this_id)
}

//:= dont need to return error, chang it later
/// get this and all its parent chat ids
pub async fn get_chats_ids(
    this_id: MessageId,
    len: usize,
) -> Result<Vec<MessageId>, Box<dyn std::error::Error>> {
    let table = CHAT_CHAIN_TABLE.lock().await;
    let mut result = Vec::with_capacity(len);
    let mut this = this_id;
    for _ in 0..len {
        result.push(this);
        this = match table.get(&this) {
            Some(p_id) => {
                if let Some(id) = p_id {
                    *id
                } else {
                    break;
                }
            }
            None => break,
        }
    }
    result.reverse();
    Ok(result)
}

async fn make_messages_in_body(
    this_id: MessageId,
    len: usize,
) -> Result<Value, Box<dyn std::error::Error>> {
    let table = CHAT_DETAIL_TABLE.lock().await;
    let details = get_chats_ids(this_id, len)
        .await?
        .iter()
        .filter_map(|id| table.get(id))
        .map(|detail| detail.to_json_message())
        .collect::<Vec<_>>();

    Ok(json!(details))
}

/// connect this message with its parent and add this message detail to table
pub async fn insert_new_reply(msg: &Message, role: &str) -> Result<(), Box<dyn std::error::Error>> {
    let reply_to_id = match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(m) => match &m.kind {
                MessageKind::Text { data, .. } => Some(m.id),
                _ => return Err("only support the text".into()),
            },
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => {
                return Err("ChannelPost isn't support yet".into())
            }
        },
        None => None,
    };

    let content = match &msg.kind {
        MessageKind::Text { data, .. } => data,
        _ => return Err("only support the text".into()),
    };

    CHAT_CHAIN_TABLE.lock().await.insert(msg.id, reply_to_id);
    //.ok_or::<String>("insert CHAT_CHAIN_TABLE has issue".into())?;
    CHAT_DETAIL_TABLE
        .lock()
        .await
        .insert(msg.id, ChatDetail::new(role, &content));
    //.ok_or::<String>("insert CHAT_DETAIL_TABLE has issue".into())?;

    info!("insert {} with its parent {:?}", msg.id, reply_to_id);

    Ok(())
}

/// receive message and return back
pub struct ChatGPT {
    vault_path: String,
    // user or group
    my_name: String,

    gpt_client: Client,
    openai_token: String,
    reqwest_client: reqwest::Client,

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
        //let gpt_client = Client::new().with_api_key(gpt_token);

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
            gpt_client: Client::new().with_api_key(""), //:= DEL
            waken_groups: stored_groups,
            reqwest_client: reqwest::Client::new(),
            openai_token: gpt_token,
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
        // let request = match CreateCompletionRequestArgs::default()
        //     .model("text-davinci-003")
        //     .prompt(data)
        //     .max_tokens(3500_u16)
        //     .build()
        // {
        //     Ok(args) => args,
        //     Err(e) => return Err(e.to_string()),
        // };

        let body = self
            .make_chat_messages(&msg.this_message_id)
            .await
            .map_err(|e| e.to_string())?;

        debug!("body: {}", body.to_string());

        let response_from_chat_gpt = self
            .reqwest_client
            .post("https://api.openai.com/v1/chat/completions")
            .bearer_auth(&self.openai_token)
            .header("Content-Type", "application/json")
            .body(body.to_string())
            .send()
            .await
            .map_err(|e| e.to_string())?
            .json::<serde_json::Value>()
            .await
            .map_err(|e| e.to_string())?;

        debug!("response: {}", response_from_chat_gpt.to_string());

        let role = &response_from_chat_gpt["choices"][0]["message"]["role"];
        let content = &response_from_chat_gpt["choices"][0]["message"]["content"];

        let result = if !role.is_null() && !content.is_null() {
            match self
                .deliver_sender
                .send(Msg2Deliver::new(
                    "reply_to".to_string(),
                    msg.chat_id,
                    format!(
                        "{}",
                        content
                            .to_string()
                            .trim_start_matches(['\n', ',', ' ', '"'])
                            .trim_end_matches(['"'])
                    ),
                    Some(msg.this_message_id),
                ))
                .await
            {
                Err(e) => Err(e.to_string()),
                _ => Ok(()),
            }
        } else {
            Err("getting role or content has issue".to_string())
        };

        // let result = match self.gpt_client.completions().create(request).await {
        //     Ok(response) => match self
        //         .deliver_sender
        //         .send(Msg2Deliver::new(
        //             "reply_to".to_string(),
        //             msg.chat_id,
        //             format!(
        //                 "{}",
        //                 response
        //                     .choices
        //                     .first()
        //                     .map(|choice| format!(
        //                         "{}",
        //                         choice.text.trim_start_matches(['\n', ',', ' ']) //:= TODO: clean some utf8 stuff
        //                     ))
        //                     .unwrap_or("sorry, something wrong".into())
        //             ),
        //             Some(msg.this_message_id),
        //         ))
        //         .await
        //     {
        //         Err(e) => Err(e.to_string()),
        //         _ => Ok(()),
        //     },
        //     Err(e) => Err(e.to_string()),
        // };

        // if err happen
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

    /// get the last ten replies of this msg_id
    async fn get_reply_chain(
        &mut self,
        msg_id: &MessageId,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        make_messages_in_body(*msg_id, 10).await
    }

    async fn make_chat_messages(
        &mut self,
        msg_id: &MessageId,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        let body = self.get_reply_chain(msg_id).await?;
        let body = json!({
            "model": "gpt-3.5-turbo",
            "messages": body
        });

        Ok(body)
    }

    //:= TODO
    /// clean the message older than 3600 * 24 * 5
    async fn clean_table() {}
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
    async fn check_msg_comm(msg: &Message) -> Option<Self> {
        let mut group_id = None;

        // check the start with command
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data.starts_with("/chat_gpt") {
                                info!("receive command {}", data);
                                if let Some(words) = data.get(en.length as usize + 1..) {
                                    if let Err(e) = insert_new_reply(msg, "user").await {
                                        error!("insert_new_reply error: {}", e.to_string())
                                    }

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
                        }
                        _ => (),
                    },
                    None => (),
                }
            }

            // group
            (MessageChat::Group(group), MessageKind::Text { data, entities }) => {
                group_id = Some(group.id.to_string());
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
                                    if let Err(e) = insert_new_reply(msg, "user").await {
                                        error!("insert_new_reply error: {}", e.to_string())
                                    }
                                    return Some(Self {
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
                                    });
                                }
                            }
                        }
                        _ => (),
                    },
                    None => (),
                }
            }
            //super group
            (MessageChat::Supergroup(group), MessageKind::Text { data, entities }) => {
                group_id = Some(group.id.to_string());
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
                                    if let Err(e) = insert_new_reply(msg, "user").await {
                                        error!("insert_new_reply error: {}", e.to_string())
                                    }
                                    return Some(Self {
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
                                    });
                                }
                            }
                        }
                        _ => (),
                    },
                    None => (),
                }
            }
            _ => (),
        }

        // pass all check upper
        // check if this message reply some message in CHAT_CHAIN_TABLE
        if if_reply_chat_in_the_chain(msg).await {
            if let Err(e) = insert_new_reply(msg, "user").await {
                error!("insert_new_reply error: {}", e.to_string())
            }
            let data = msg.kind.text().unwrap_or("".into());
            Some(Self {
                data: data,
                user_name: msg.from.username.clone().unwrap_or(String::new()),
                chat_id: msg.chat.id(),
                group_id: group_id,
                first_name: msg.from.first_name.clone(),
                last_name: msg.from.last_name.clone(),
                this_message_id: msg.id,
            })
        } else {
            None
        }
    }
}

pub struct ChatGPTInputConsumer {
    sender: Sender<ChatGPTInput>,
}

#[async_trait]
impl AppConsumer for ChatGPTInputConsumer {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match ChatGPTInput::check_msg_comm(msg).await {
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
