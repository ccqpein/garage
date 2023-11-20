use super::*;
use entity::prelude::*;
use lazy_static::*;
use sea_orm::{
    ColumnTrait, Condition, DatabaseConnection, DbBackend, EntityTrait, QueryFilter, QuerySelect,
};
use serde::{Deserialize, Serialize};
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
    pub static ref DB: Mutex<Option<DatabaseConnection>> = Mutex::new(None);
}

/// the number of how many messages inside the request body
static CHAT_COUNT: usize = 40;

/// get the group type. Return the group type and the group id
fn get_space_info(msg: &Message) -> (String, String) {
    match &msg.chat {
        c @ MessageChat::Private(_) => (String::from("private"), c.id().to_string()),
        MessageChat::Group(g) => (String::from("group"), g.id.to_string()),
        MessageChat::Supergroup(g) => (String::from("supergroup"), g.id.to_string()),
        _ => unreachable!(),
    }
}

fn chat_record_to_json(cr: &entity::chat_records::Model) -> Value {
    json!({"role": cr.role, "content": cr.content})
}

async fn if_reply_chat_in_the_chain3(msg: &Message, db: &DatabaseConnection) -> bool {
    let (space_type, space_id) = get_space_info(msg);

    let reply_to_id = match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(m) => match &m.kind {
                MessageKind::Text { .. } => m.id,
                _ => return false,
            },
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => return false,
        },
        None => return false,
    };

    if_reply_chat_in_the_chain2(&space_type, &space_id, reply_to_id, db).await
}

async fn if_reply_chat_in_the_chain2(
    space_type: &String,
    space_id: &String,
    reply_id: MessageId,
    db: &DatabaseConnection,
) -> bool {
    let x = ChatRecords::find()
        .filter(
            Condition::all()
                .add(entity::chat_records::Column::SpaceType.eq(space_type))
                .add(entity::chat_records::Column::SpaceId.eq(space_id))
                .add(entity::chat_records::Column::MessageId.eq(reply_id.to_string())),
        )
        .one(db)
        .await;

    x.is_ok() && x.unwrap().is_some()
}

pub async fn insert_new_reply(
    msg: &Message,
    role: &str,
    replace_content: Option<&str>,
    db: &DatabaseConnection,
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

    let (space_type, space_id) = get_space_info(msg);

    let mut content = match replace_content {
        Some(s) => s.to_string(),
        None => match &msg.kind {
            MessageKind::Text { data, .. } => data.clone(),
            _ => return Err("only support the text".into()),
        },
    };

    // reply some post but not inside chain
    content = match reply_to_id {
        Some(rid) => {
            if !if_reply_chat_in_the_chain2(&space_type, &space_id, rid, db).await {
                reply_to_id = None; // merge this two meg together
                vec![r#"Original Post: \n""#, &reply_msg_data, r#""\n"#, &content].concat()
            } else {
                content
            }
        }
        None => content,
    };

    let new_chat_record = entity::chat_records::ActiveModel {
        space_type: sea_orm::ActiveValue::Set(space_type),
        space_id: sea_orm::ActiveValue::Set(space_id),
        message_id: sea_orm::ActiveValue::Set(msg.id.to_string()),
        reply_to: sea_orm::ActiveValue::Set(reply_to_id.map(|id| id.to_string())),
        role: sea_orm::ActiveValue::Set(role.to_string()),
        content: sea_orm::ActiveValue::Set(Some(content)),
        create_at: sea_orm::ActiveValue::Set(chrono::offset::Utc::now().to_string()),
        ..Default::default()
    };

    ChatRecords::insert(new_chat_record).exec(db).await?;
    Ok(())
}

/// receive message and return back
pub struct ChatGPT {
    vault_path: String,

    my_name: String,
    db: DatabaseConnection,

    openai_token: String,
    reqwest_client: reqwest::Client,

    waken_groups: HashSet<String>,
    waken_usernames: HashSet<String>,

    sender: Sender<ChatGPTInput>,
    receiver: Receiver<ChatGPTInput>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl ChatGPT {
    pub async fn new(
        deliver_sender: Sender<Msg2Deliver>,
        vault_path: String,
        db: &DatabaseConnection,
    ) -> Result<Self, String> {
        let (sender, receiver) = mpsc::channel(10);
        // cache my id
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

        // cached groups ids
        let stored_groups = match GptGroupWhitelist::find().all(db).await {
            Ok(m) => m.into_iter().map(|x| x.group_id).collect(),
            Err(e) => return Err(e.to_string()),
        };

        info!(
            "these group added to waken_groups directly: {:?}",
            stored_groups
        );

        // cache the users users
        let stored_usernames = match GptWhiteList::find().all(db).await {
            Ok(m) => m.into_iter().map(|x| x.username).collect(),
            Err(e) => return Err(e.to_string()),
        };

        info!(
            "these usernames added to waken_usernames directly: {:?}",
            stored_usernames
        );

        // need to assign the static db connection
        *DB.lock().await = Some(db.clone());

        Ok(ChatGPT {
            vault_path,
            db: db.clone(),
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

    async fn write_to_group_list_table(&self, g_id: String) -> Result<(), String> {
        let new_group = entity::gpt_group_whitelist::ActiveModel {
            group_id: sea_orm::ActiveValue::Set(g_id),
            ..Default::default()
        };

        GptGroupWhitelist::insert(new_group)
            .exec(&self.db)
            .await
            .map_err(|e| e.to_string())?;

        Ok(())
    }

    /// get the last CHAT_COUNT replies of this msg_id
    async fn get_reply_chain(
        &mut self,
        msg: &Message,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        let (space_type, space_id) = get_space_info(msg);

        let mut msg_id = msg.id.to_string();
        let mut msgs = vec![];

        for _ in 0..CHAT_COUNT {
            let x = match ChatRecords::find()
                .filter(
                    Condition::all()
                        .add(entity::chat_records::Column::SpaceType.eq(&space_type))
                        .add(entity::chat_records::Column::SpaceId.eq(&space_id))
                        .add(entity::chat_records::Column::MessageId.eq(&msg_id)),
                )
                .one(&self.db)
                .await?
            {
                Some(x) => x,
                None => return Err("cannot find this message".into()),
            };

            msgs.push(chat_record_to_json(&x));
            // update the msg_id
            msg_id = match x.reply_to {
                Some(x) => x,
                None => break,
            };
        }
        msgs.reverse();
        Ok(json!(msgs))
    }

    async fn make_chat_message_to_openai_agent(
        &mut self,
        msg: &Message,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        self.get_reply_chain(msg).await
    }

    async fn call_py_openai_client(&self, endpoint: &str, body: &str) -> Result<String, String> {
        match self
            .reqwest_client
            .post(format!("http://127.0.0.1:8080{}", endpoint))
            .header("Content-Type", "application/json")
            .body(body.to_string())
            .send()
            .await
        {
            Ok(v) => v.text().await.map_err(|e| e.to_string()),
            Err(e) => Err(e.to_string()),
        }
    }

    //:= TODO
    /// clean the message older than 3600 * 24 * 5
    async fn clean_table() {}

    //:= TODO: return value should be used by chat gpt app
    async fn download_file(&self) {}

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
                        self.write_to_group_list_table(g_id.clone()).await?;
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

        //:= NEXT: download file here
        // let file_loc = if let Some(f) = reply_msg_is_media(&msg) {
        // 	self.download_file(f)?
        // }

        // add to reply here
        insert_new_reply(&msg.this_message, "user", Some(&msg.data), &self.db)
            .await
            .map_err(|e| e.to_string())?;

        // start to call open ai
        let body = self
            .make_chat_message_to_openai_agent(&msg.this_message)
            .await
            .map_err(|e| e.to_string())?;

        debug!("chat_message_to_openai_agent: {}", body.to_string());

        let result = match self.call_py_openai_client("/chat", &body.to_string()).await {
            Ok(s) => {
                //let deliver_msg = trim_string_helper(s)?;
                let deliver_msg = s;

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
            }
            Err(e) => {
                self.deliver_sender
                    .send(Msg2Deliver::new(
                        "send".to_string(),
                        msg.chat_id,
                        format!("sorry, something wrong from server: {}", e.to_string()),
                        None,
                    ))
                    .await;
                return Err(e.to_string());
            }
        };

        // if err happen
        match &result {
            re @ Err(e) => {
                self.deliver_sender
                    .send(Msg2Deliver::new(
                        "send".to_string(),
                        msg.chat_id,
                        format!("sorry, something wrong from server: {}", e.to_string()),
                        None,
                    ))
                    .await;
                re.clone()
            }
            _ => Ok(()),
        }
    }
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

/*
The App impl below
 */

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
        if if_reply_chat_in_the_chain3(msg, DB.lock().await.as_ref().unwrap()).await {
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

//:= NEXT: should I add these two to reminder file?
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct OpenAIAgentResponse {
    content: Option<String>,
    tool_calls: Option<ToolCalls>,
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct ToolCalls {
    name: String,
    arguments: Value,
}

// ChatGPTInputConsumer type
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
    use sea_orm::QueryTrait;

    use super::*;

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

    #[test]
    fn test_chat_record() {
        let new_chat_record = entity::chat_records::ActiveModel {
            space_type: sea_orm::ActiveValue::Set("".to_string()),
            space_id: sea_orm::ActiveValue::Set("".to_string()),
            message_id: sea_orm::ActiveValue::Set("".to_string()),
            reply_to: sea_orm::ActiveValue::Set(Some("".to_string())),
            role: sea_orm::ActiveValue::Set("".to_string()),
            content: sea_orm::ActiveValue::Set(Some("".to_string())),
            ..Default::default()
        };

        dbg!(new_chat_record);
    }

    #[test]
    fn test_query_chat_record() {
        let a = ChatRecords::find()
            .filter(
                Condition::all()
                    .add(entity::chat_records::Column::SpaceType.eq("type"))
                    .add(entity::chat_records::Column::SpaceId.eq("id"))
                    .add(entity::chat_records::Column::MessageId.eq("id")),
            )
            //:=DEL: .one(db)
            .limit(Some(100))
            .build(DbBackend::Postgres)
            .to_string();

        dbg!(a);
    }

    #[test]
    fn test_parse_openai_agent_response() {
        let testcase = r#"
{"content": "Hello! How can I assist you today? If you have any questions or need help with something feel free to ask."}
"#;

        let result: OpenAIAgentResponse = serde_json::from_str(testcase).unwrap();
        //dbg!(&result);
        assert_eq!(
            result,
            OpenAIAgentResponse {
                content: Some(r#"Hello! How can I assist you today? If you have any questions or need help with something feel free to ask."#.to_string()),
                tool_calls: None,
            }
        );

        let testcase = r#"{"content": null, "tool_calls": {"name": "make_reminder", "arguments": {"content": "check soup", "timestamp": "3h"}}}
"#;

        let result: OpenAIAgentResponse = serde_json::from_str(testcase).unwrap();
        //dbg!(&result);
        assert_eq!(
            result,
            OpenAIAgentResponse {
                content: None,
                tool_calls: Some(ToolCalls {
                    name: "make_reminder".to_string(),
                    arguments: serde_json::from_str(
                        r#"{"content": "check soup", "timestamp": "3h"}"#
                    )
                    .unwrap(),
                }),
            }
        );
    }
}
