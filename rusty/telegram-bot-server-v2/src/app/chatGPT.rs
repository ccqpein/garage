use super::util::*;
use super::*;
use base64::encode;
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
    ChatId, GroupId, MessageChat, MessageId, MessageKind, MessageText, SupergroupId, ToFileRef,
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

fn chat_record_to_json(cr: &entity::chat_records::Model) -> Result<Value, String> {
    let cc = match serde_json::from_str::<Value>(cr.content.as_ref().map(|s| s.as_str()).unwrap()) {
        Ok(cc) => cc,
        Err(_) => {
            // pure string cannot from_str to value
            // use json! can handle the pure string
            json!(cr.content.as_ref().unwrap_or(&String::new()))
        }
    };
    Ok(json!({"role": cr.role, "content": cc}))
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

// Function to encode an image to base64
fn encode_image(image_path: &str) -> Result<String, Box<dyn std::error::Error>> {
    // Open the file
    let mut image_file = File::open(image_path)?;

    // Read the file's contents into a vector
    let mut buffer = Vec::new();
    image_file.read_to_end(&mut buffer)?;

    // Encode the contents of the file
    let encoded_image = encode(&buffer);

    Ok(encoded_image)
}

async fn audio_to_text_call(
    client: reqwest::Client,
    audio_file_path: &str,
) -> Result<String, String> {
    //:= copy from call_py_openai_client
    let response = match client
        .post(format!("http://127.0.0.1:8080{}", "/audio_transcribe"))
        .header("Content-Type", "application/json")
        .body(json!({"file_path": audio_file_path}).to_string())
        .send()
        .await
    {
        Ok(v) => {
            //debug!("recieve response {:?} from openai agent",v);
            v
                .json::<OpenAIAgentResponse>()
                .await
                .map_err(|e| e.to_string())                
        }
        
        Err(e) => Err(e.to_string()),
    }?;

    response.content.ok_or("no response".to_string())
}

/// The wrapper of the message data
/// used inside reply and chatgpt input
#[derive(Debug)]
pub enum DataWrapper {
    Text(String),
    Image(String),
    AudioText(String),
}

impl DataWrapper {
    async fn from_msg(
        value: &Message,
        downloader: &Option<FileDownloader>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        match &value.kind {
            MessageKind::Text { data, .. } => Ok(DataWrapper::Text(data.to_string())),
            MessageKind::Photo { data, .. } => {
                if let Some(dl) = downloader {
                    Ok(Self::Image(encode_image(
                        &dl.download_file(data.last().unwrap(), None).await?,
                    )?))
                } else {
                    Err("need downloader for image".into())
                }
            }
            MessageKind::Voice { data, .. } => {
                let file_path = if let Some(dl) = downloader {
                    dl.download_file(data,Some(".ogg")).await?
                } else {
                    return Err("need downloader for image".into());
                };

                //:= maybe need the global request rather than make here...
                //:= or in chatgpt struct. Need to make sure client pool will reconnect
                let client = reqwest::Client::builder()
                    .connect_timeout(Duration::from_secs(60))
                    .build()
                    .map_err(|e| e.to_string())?;

                let audio_text = audio_to_text_call(client, &file_path).await?;
                info!("audio_text from openai: {}",audio_text);

                Ok(Self::AudioText(audio_text))
            }
            _ => Err("data wrapper cannot from this message yet".into()),
        }
    }
}

pub async fn insert_new_reply(
    msg: &Message,
    role: &str,
    replace_content: Option<&DataWrapper>,
    db: &DatabaseConnection,
    downloader: &Option<FileDownloader>,
) -> Result<(), Box<dyn std::error::Error>> {
    let (mut reply_to_id, reply_msg_data) = match &msg.reply_to_message {
        Some(m) => match m.as_ref() {
            telegram_bot::MessageOrChannelPost::Message(m) => {
                (Some(m.id), DataWrapper::from_msg(m, downloader).await?)
            }
            telegram_bot::MessageOrChannelPost::ChannelPost(_) => {
                return Err("ChannelPost isn't support yet".into())
            }
        },
        None => (None, DataWrapper::Text(String::new())),
    };

    let (space_type, space_id) = get_space_info(msg);

    let content = match replace_content {
        Some(a) => a,
        None => &match &msg.kind {
            MessageKind::Text { data, .. } => DataWrapper::Text(data.clone()),
            _ => return Err("only support the text".into()),
        },
    };

    // reply some post
    let content = match reply_to_id {
        Some(rid) => {
            // the message is replied is not inside chain
            if !if_reply_chat_in_the_chain2(&space_type, &space_id, rid, db).await {
                reply_to_id = None; // merge this two meg together
                match (reply_msg_data, content) {
                    (DataWrapper::Text(rply), DataWrapper::Text(this)) | (DataWrapper::AudioText(rply), DataWrapper::Text(this))=> {
                        let cc = vec![r#"Original Post: \n""#, &rply, r#""\n"#, &this].concat();
                        json!([{"type":"text", "text": cc}]).to_string()
                    }

                    (DataWrapper::Image(img), DataWrapper::Text(this)) => {
                        json!([{"type":"text", "text": this},
                               {"type": "image_url","image_url":{"url":format!("data:image/jpeg;base64,{}",img)}}]).to_string()
                    },
                    
                    _ => {return Err("can only reply free message with text".into())}
                }
            } else {
                match content{
                    DataWrapper::Text(this) | DataWrapper::AudioText(this) => json!([{"type":"text", "text": this}]).to_string(),
                    DataWrapper::Image(img) => json!([{"type": "image_url","image_url":{"url":format!("data:image/jpeg;base64,{}",img)}}]).to_string(),
                }
            }
        }
        None => match content {
            DataWrapper::Text(this)|DataWrapper::AudioText(this) => json!([{"type":"text", "text": this}]).to_string(),
            DataWrapper::Image(_) => return Err("start talk with text".into()),
        },
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

    reqwest_client: reqwest::Client,
    file_downloader: Option<FileDownloader>,

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
        downloader: Option<FileDownloader>,
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
            file_downloader: downloader,
            waken_groups: stored_groups,
            waken_usernames: stored_usernames,
            reqwest_client: reqwest::Client::builder()
                .connect_timeout(Duration::from_secs(60))
                .build()
                .map_err(|e| e.to_string())?,
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

            msgs.push(chat_record_to_json(&x)?);
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

    async fn call_py_openai_client(
        &self,
        endpoint: &str,
        body: &str,
    ) -> Result<OpenAIAgentResponse, String> {
        match self
            .reqwest_client
            .post(format!("http://127.0.0.1:8080{}", endpoint))
            .header("Content-Type", "application/json")
            .body(body.to_string())
            .send()
            .await
        {
            Ok(v) => v
                .json::<OpenAIAgentResponse>()
                .await
                .map_err(|e| e.to_string()),
            Err(e) => Err(e.to_string()),
        }
    }

    //:= TODO: do I need to clean the table? Text isn't that large
    /// clean the message older than 3600 * 24 * 5
    async fn clean_table() {}

    async fn handle_chat(&mut self, msg: ChatGPTInput) -> Result<(), String> {
        // check if chat legal or not
        match (msg.group_id, msg.user_name.as_str(), &msg.data) {
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
            (Some(ref g_id), name, DataWrapper::Text(cont)) if cont == "wake_up" => {
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

        // add to reply here
        insert_new_reply(
            &msg.this_message,
            "user",
            Some(&msg.data),
            &self.db,
            &self.file_downloader,
        )
        .await
        .map_err(|e| e.to_string())?;

        // start to call open ai
        let body = self
            .make_chat_message_to_openai_agent(&msg.this_message)
            .await
            .map_err(|e| e.to_string())?;

        info!("chat_message_to_openai_agent: {}", body.to_string());

        let result: Result<_, _> =
            match self.call_py_openai_client("/chat", &body.to_string()).await {
                Ok(s) => {
                    let a_response = s;

                    info!("response from agent: {:?}", a_response);

                    match (a_response.content, a_response.tool_calls) {
                        (Some(deliver_msg), None) => {
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
                        (None, None) => Err("openai agent response the empty result".into()),
                        (None, Some(tc)) => {
                            let deliver_msg = tc.run(self, msg.chat_id).await?;
                            if let Some(dm) = deliver_msg {
                                match self
                                    .deliver_sender
                                    .send(Msg2Deliver::new(
                                        "reply_to".to_string(),
                                        msg.chat_id,
                                        dm,
                                        Some(msg.this_message.id),
                                    ))
                                    .await
                                {
                                    Err(e) => Err(e.to_string()),
                                    _ => Ok(()),
                                }
                            } else {
                                Ok(())
                            }
                        }
                        (Some(deliver_msg), Some(tc)) => {
                            let deliver_msg = format!(
                                "{} (with funcall response {})",
                                deliver_msg,
                                tc.run(self, msg.chat_id).await?.unwrap_or("".to_string())
                            );
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
                    }
                }
                Err(e) => Err(e.to_string()),
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
            downloader: self.file_downloader.clone(),
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
    data: DataWrapper,
    user_name: String,

    first_name: String,
    last_name: Option<String>,

    chat_id: ChatId,
    group_id: Option<String>,

    this_message: Message,
}

impl ChatGPTInput {
    async fn check_msg_comm(msg: &Message, downloader: &Option<FileDownloader>) -> Option<Self> {
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
                                        data: DataWrapper::Text(words.into()),
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
                                    data: DataWrapper::Text("wake_up".into()),
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
                                        data: DataWrapper::Text(words.into()),
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

            // super group
            (MessageChat::Supergroup(group), MessageKind::Text { data, entities }) => {
                group_id = Some(group.id.to_string());
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data.starts_with("/wake_up") {
                                info!("receive command {}", data);
                                return Some(Self {
                                    data: DataWrapper::Text("wake_up".into()),
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
                                        data: DataWrapper::Text(words.into()),
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
            let data = match DataWrapper::from_msg(msg, downloader).await {
                Ok(d) => d,
                Err(e) => {
                    error!("error in chatgpt input: {}", e.to_string());
                    return None;
                }
            };
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

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct OpenAIAgentResponse {
    content: Option<String>,
    tool_calls: Option<ToolCalls>,
}

impl OpenAIAgentResponse {
    fn has_tool_calls(&self) -> bool {
        self.tool_calls.is_some()
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq)]
struct ToolCalls {
    name: String,
    arguments: Value,
}

impl ToolCalls {
    fn is_empty(&self) -> bool {
        self.name.is_empty()
    }

    fn name_and_arguments(&self) -> (&String, &Value) {
        (&self.name, &self.arguments)
    }

    async fn run(&self, x: &ChatGPT, chatid: ChatId) -> Result<Option<String>, String> {
        match self.name.as_str() {
            "make_reminder" => {
                let content = self
                    .arguments
                    .get("content")
                    .ok_or("cannot get content in args".to_string())?
                    .as_str()
                    .ok_or("cannot get as string".to_string())?
                    .to_string();
                let time = ReminderTime::parse(
                    self.arguments
                        .get("timestamp")
                        .ok_or("cannot get timestamp in args".to_string())?
                        .as_str()
                        .ok_or("cannot get as string".to_string())?,
                )?;
                add_reminder(chatid, time, content, x.deliver_sender.clone())
                    .await
                    .map_err(|e| e.to_string())?;
                Ok(None)
            }
            n @ _ => Ok(Some(format!("no {} function on bot side", n))),
        }
    }
}

// ChatGPTInputConsumer type
pub struct ChatGPTInputConsumer {
    sender: Sender<ChatGPTInput>,
    downloader: Option<FileDownloader>,
}

#[async_trait]
impl AppConsumer for ChatGPTInputConsumer {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match ChatGPTInput::check_msg_comm(msg, &self.downloader).await {
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

        let testcase = r#"{"content": "Can you tell me a joke?"}"#;
        let result: OpenAIAgentResponse = serde_json::from_str(testcase).unwrap();
        assert_eq!(
            result,
            OpenAIAgentResponse {
                content: Some(r#"Can you tell me a joke?"#.to_string()),
                tool_calls: None,
            }
        );
        

        // dbg!(result
        //     .tool_calls
        //     .unwrap()
        //     .arguments
        //     .get("timestamp")
        //     .unwrap()
        //     .as_str()
        //     .unwrap());
    }

    #[test]
    fn test_json_list_parser() {
        let a_data = r#"{"type":"text", "text": "Who won the world series in 2020?"}"#;
        let a: Value = serde_json::from_str(a_data).unwrap();

        let re = json!({"role": "user", "content": "content"});
        dbg!(&a);
        dbg!(&re);

        let b = r#"hello"#;
        //let bb = serde_json::from_str::<Value>(&b).unwrap();
        let bb = json!(b);
        dbg!(&bb);

        let re2 = json!({"role": "user", "content": a});
        dbg!(&re2);
        dbg!(&re2.to_string());

        let re2 = json!({"role": "user", "content": a_data});
        dbg!(&re2);

        // assert_eq!(
        //     a_data,
        //     json!({"type":"text", "text": "Who won the world series in 2020?"}).to_string()
        // )

        dbg!(json!({"a": "a"}).to_string());
        //let bb = "a".to_string();
        dbg!(json!({"a": bb}).to_string());

        let content = "content";
        let img = "img";
        dbg!(json!([{"type":"text", "text": content},
                    {"type": "image_url","image_url":{"url":format!("data:image/jpeg;base64,{}",img)}}]).to_string());
    }
}
