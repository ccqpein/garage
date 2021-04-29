use std::collections::{HashMap, HashSet};

//use async_std::sync::Mutex;
use lazy_static::*;
use std::sync::{Arc, Mutex};
use telegram_bot::{
    Api, ChatId, Message, MessageChat, MessageId, MessageKind, SendMessage, ToMessageId,
};
use tokio::{
    sync::mpsc::{Receiver, Sender},
    time::sleep,
    time::Duration,
};
use tracing::info;

lazy_static! {
    static ref REMIND_PENDING_TABLE: Mutex<HashMap<ChatId, bool>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
    //static ref API: Arc<Mutex<Api>> = { Arc::new(Mutex::new(Api::new(""))) };

    static ref REMIND_TABLE: Mutex<HashMap<(ChatId,MessageId), bool>> = {
        Mutex::new(HashMap::new())
    };
}

enum Status {
    RemindPending,
    Nil,
}

pub struct Msg2Deliver {
    command: String,
    chatid: ChatId,
    msg: String,
}

pub struct Watcher {
    api: Api,

    ch: Receiver<Message>,

    send: Sender<Msg2Deliver>,
}

impl Watcher {
    pub fn new(api: Api, ch: Receiver<Message>, send: Sender<Msg2Deliver>) -> Self {
        //*API = Arc::new(Mutex::new(api.clone()));
        Self { api, ch, send }
    }

    async fn run(&mut self) {
        while let Some(msg) = self.ch.recv().await {
            let status = status_checker(&msg);
            match (&msg.kind, status) {
                (MessageKind::Text { ref data, .. }, Status::Nil) => {
                    match data.to_lowercase().as_str() {
                        "remind" => {
                            //:= guard private chat

                            let id = msg.chat.id().clone();
                            REMIND_PENDING_TABLE.lock().unwrap().insert(id, true);

                            let sendchannel = self.send.clone();
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(3)).await;
                                sendchannel.send(Msg2Deliver {
                                    command: "send".into(),
                                    chatid: id,
                                    msg: "Go ahead, I am listenning".into(),
                                });

                                sleep(Duration::from_secs(5)).await;
                                sendchannel.send(Msg2Deliver {
                                    command: "send".into(),
                                    chatid: id,
                                    msg: "Run out remind waiting time".into(),
                                });

                                REMIND_PENDING_TABLE.lock().unwrap().remove(&id);
                            });
                        }
                        _ => {}
                    }
                }
                (MessageKind::Text { ref data, .. }, Status::RemindPending) => {
                    //REMIND_TABLE.lock().insert
                    tokio::spawn(async move {});
                    REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id());
                }
                _ => {}
            }
        }
    }
}

fn status_checker(msg: &Message) -> Status {
    if REMIND_PENDING_TABLE
        .lock()
        .unwrap()
        .get(&msg.chat.id())
        .is_some()
    {
        Status::RemindPending
    } else {
        Status::Nil
    }
}

struct Deliver {
    api: Api,
    ch: Receiver<Msg2Deliver>,
}

impl Deliver {
    async fn run(&mut self) {
        while let Some(ref d) = self.ch.recv().await {
            match d.command.as_ref() {
                "send" => {
                    if let Err(s) = self.send_message(&d.chatid, &d.msg).await {
                        info!("{}", s);
                    }
                }
                _ => {}
            }
        }
    }

    async fn send_message(&self, id: &ChatId, msg: &str) -> Result<(), String> {
        self.api
            .send(SendMessage::new(id, msg))
            .await
            .map_err(|e| e.to_string())?;

        Ok(())
    }
}
