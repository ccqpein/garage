use super::*;
use lazy_static::*;
use serde_json::{json, Value};
use std::{
    collections::{HashMap, HashSet},
    fs::{File, OpenOptions},
    io::{prelude::*, BufRead, BufReader},
    time::Duration,
};
use telegram_bot::{
    ChatId, GroupId, MessageChat, MessageId, MessageKind, MessageText, SupergroupId,
};
use tokio::sync::Mutex;

/// add tables for storing the chat
lazy_static! {
    static ref CHAT_CHAIN_TABLE: Mutex<HashMap<ChatSpace, Option<MessageId>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
    static ref CHAT_DETAIL_TABLE: Mutex<HashMap<ChatSpace, ChatDetail>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

/// the number of how many messages inside the request body
static CHAT_COUNT: usize = 20;

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

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
enum ChatSpaceKind {
    Group(GroupId),
    Private(ChatId),
    SuperGroup(SupergroupId),
}

impl From<&Message> for ChatSpaceKind {
    fn from(value: &Message) -> Self {
        match &value.chat {
            c @ MessageChat::Private(_) => Self::Private(c.id()),
            MessageChat::Group(g) => Self::Group(g.id),
            MessageChat::Supergroup(g) => Self::SuperGroup(g.id),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct ChatSpace {
    kind: ChatSpaceKind,
    id: MessageId,
}

impl ChatSpace {
    fn update_id(&mut self, id: MessageId) {
        self.id = id
    }
}

impl From<&Message> for ChatSpace {
    fn from(value: &Message) -> Self {
        Self {
            kind: ChatSpaceKind::from(value),
            id: value.id,
        }
    }
}

pub async fn if_in_chain(cs: &ChatSpace) -> bool {
    CHAT_CHAIN_TABLE.lock().await.contains_key(cs)
}

pub async fn if_reply_chat_in_the_chain(msg: &Message) -> bool {
    let mut cs = ChatSpace::from(msg);
    match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(mm) => {
                cs.update_id(mm.id); // update to reply id
                if_in_chain(&cs).await
            }
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => false,
        },
        None => false,
    }
}

//:= dont need to return error, chang it later
/// get this and all its parent chat ids
pub async fn get_chats_ids(
    this_msg: &Message,
    len: usize,
) -> Result<Vec<ChatSpace>, Box<dyn std::error::Error>> {
    let mut cs = ChatSpace::from(this_msg);

    let table = CHAT_CHAIN_TABLE.lock().await;
    let mut result = Vec::with_capacity(len);
    let mut this;

    for _ in 0..len {
        result.push(cs.clone());
        this = match table.get(&cs) {
            Some(p_id) => {
                if let Some(id) = p_id {
                    *id
                } else {
                    break;
                }
            }
            None => break,
        };
        cs.update_id(this);
    }
    result.reverse();
    Ok(result)
}

async fn make_messages_in_body(
    msg: &Message,
    len: usize,
) -> Result<Value, Box<dyn std::error::Error>> {
    let table = CHAT_DETAIL_TABLE.lock().await;
    let details = get_chats_ids(msg, len)
        .await?
        .iter()
        .filter_map(|id| table.get(id))
        .map(|detail| detail.to_json_message())
        .collect::<Vec<_>>();

    Ok(json!(details))
}

/// connect this message with its parent and add this message detail to table
/// replace_content replace the data of message to insert.
pub async fn insert_new_reply(
    msg: &Message,
    role: &str,
    replace_content: Option<&str>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (mut reply_to_id, reply_msg_data) = match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(m) => match &m.kind {
                MessageKind::Text { data, .. } => (Some(m.id), data.to_string()),
                _ => return Err("only support the text".into()),
            },
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => {
                return Err("ChannelPost isn't support yet".into())
            }
        },
        None => (None, "".to_string()),
    };

    let mut content = match replace_content {
        Some(s) => s.to_string(),
        None => match &msg.kind {
            MessageKind::Text { data, .. } => data.clone(),
            _ => return Err("only support the text".into()),
        },
    };

    // reply some post but not inside chain
    content = match reply_to_id {
        Some(_) => {
            if !if_reply_chat_in_the_chain(&msg).await {
                reply_to_id = None; // merge this two meg together
                vec![r#"Original Post: \n""#, &reply_msg_data, r#""\n"#, &content].concat()
            } else {
                content
            }
        }
        None => content,
    };

    CHAT_CHAIN_TABLE
        .lock()
        .await
        .insert(msg.into(), reply_to_id);
    CHAT_DETAIL_TABLE
        .lock()
        .await
        .insert(msg.into(), ChatDetail::new(role, &content));

    info!("insert {} with its parent {:?}", msg.id, reply_to_id);

    Ok(())
}

/// receive message and return back
pub struct ChatGPT {
    vault_path: String,
    // user or group
    my_name: String,

    openai_token: String,
    reqwest_client: reqwest::Client,

    waken_groups: HashSet<String>,
    waken_usernames: HashSet<String>,

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

        // group
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

        // users
        let f = BufReader::new(
            File::open(vault_path.clone() + "/stored_usernames").map_err(|e| e.to_string())?,
        );
        let stored_usernames = f
            .lines()
            .filter_map(|l| l.ok())
            .collect::<HashSet<String>>();

        info!(
            "these usernames added to waken_usernames directly: {:?}",
            stored_usernames
        );

        Ok(ChatGPT {
            vault_path,
            my_name,
            sender,
            receiver,
            deliver_sender,
            waken_groups: stored_groups,
            waken_usernames: stored_usernames,
            reqwest_client: reqwest::Client::builder()
                .connect_timeout(Duration::from_secs(60))
                .build()
                .map_err(|e| e.to_string())?,
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
                if name != self.my_name && !self.waken_usernames.contains(name) {
                    match self
                        .deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            msg.chat_id,
                            String::from("this feature is not for you"),
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

        // add to reply here
        insert_new_reply(&msg.this_message, "user", Some(&msg.data))
            .await
            .map_err(|e| e.to_string())?;

        // start to call open ai
        let body = self
            .make_chat_messages(&msg.this_message)
            .await
            .map_err(|e| e.to_string())?;

        debug!("body: {}", body.to_string());

        let response_from_chat_gpt = match self
            .reqwest_client
            .post("https://api.openai.com/v1/chat/completions")
            .bearer_auth(&self.openai_token)
            .header("Content-Type", "application/json")
            .body(body.to_string())
            .send()
            .await
        {
            Ok(v) => match v.json::<serde_json::Value>().await {
                Ok(vv) => vv,
                Err(e) => {
                    self.deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            msg.chat_id,
                            "sorry, something wrong from server".into(),
                            None,
                        ))
                        .await;
                    return Err(e.to_string());
                }
            },
            Err(e) => {
                self.deliver_sender
                    .send(Msg2Deliver::new(
                        "send".to_string(),
                        msg.chat_id,
                        "sorry, something wrong from server".into(),
                        None,
                    ))
                    .await;
                return Err(e.to_string());
            }
        };

        debug!("response: {}", response_from_chat_gpt.to_string());

        let role = &response_from_chat_gpt["choices"][0]["message"]["role"];
        let content = &response_from_chat_gpt["choices"][0]["message"]["content"];

        let result = if !role.is_null() && !content.is_null() {
            let deliver_msg = trim_string_helper(content.to_string())?;

            debug!("delivery msg: {}", deliver_msg);

            match self
                .deliver_sender
                .send(Msg2Deliver::new(
                    "reply_to".to_string(),
                    msg.chat_id,
                    deliver_msg,
                    Some(msg.this_message.id),
                ))
                .await
            {
                Err(e) => Err(e.to_string()),
                _ => Ok(()),
            }
        } else {
            Err("getting role or content has issue".to_string())
        };

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
        msg: &Message,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        make_messages_in_body(msg, CHAT_COUNT).await
    }

    async fn make_chat_messages(
        &mut self,
        msg: &Message,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        let body = self.get_reply_chain(msg).await?;
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

fn trim_string_helper(s: String) -> Result<String, String> {
    let s = s.replace("\\n", "\n").replace(r#"\""#, r#"""#);
    let mut result = vec![];
    let mut flag = false;
    for b in s.bytes() {
        if flag {
            result.push(b);
            continue;
        }
        if b == b'"' {
            flag = true
        }
    }

    let end = result.iter().enumerate().rfind(|(ind, b)| **b == b'"');
    let result = match end {
        Some((e, _)) => String::from_utf8(result[0..e].to_vec()).map_err(|e| e.to_string())?,
        None => String::from_utf8(result).map_err(|e| e.to_string())?,
    };

    Ok(result
        .replace("\\n", "\n")
        .replace(r#"\""#, r#"""#)
        .trim_start_matches(['\n', ',', ' '])
        .to_string())
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
                Err(e) => {
                    error!("error: {}", e)
                }
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

    this_message: Message,
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
                                //:= do I need the system role here?
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
                                        this_message: msg.clone(),
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
                                    this_message: msg.clone(),
                                });
                            } else if data.starts_with("/chat_gpt") {
                                //:= do I need the system role here?
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
                                        group_id: Some(group.id.to_string()),
                                        first_name: msg.from.first_name.clone(),
                                        last_name: msg.from.last_name.clone(),
                                        this_message: msg.clone(),
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
                                    this_message: msg.clone(),
                                });
                            } else if data.starts_with("/chat_gpt") {
                                //:= do I need the system role here?
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
                                        group_id: Some(group.id.to_string()),
                                        first_name: msg.from.first_name.clone(),
                                        last_name: msg.from.last_name.clone(),
                                        this_message: msg.clone(),
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
            let data = msg.kind.text().unwrap_or("".into());
            Some(Self {
                data,
                user_name: msg.from.username.clone().unwrap_or(String::new()),
                chat_id: msg.chat.id(),
                group_id,
                first_name: msg.from.first_name.clone(),
                last_name: msg.from.last_name.clone(),
                this_message: msg.clone(),
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
mod tests {
    //use super::*;

    use super::trim_string_helper;

    #[test]
    fn test_clean_text() {
        let a = "a\\nb";
        dbg!(a.replace(r"\\n", r"\n"));
        println!("{}", a.replace("\\n", "\n"));
    }

    #[test]
    fn test_trim_string_helper() -> Result<(), Box<dyn std::error::Error>> {
        let a = r#""  content ""#.to_string();
        assert_eq!(trim_string_helper(a)?, r#"content "#.to_string());

        let a = r#""  "content" ""#.to_string();
        assert_eq!(trim_string_helper(a)?, r#""content" "#.to_string());

        Ok(())
    }
}
