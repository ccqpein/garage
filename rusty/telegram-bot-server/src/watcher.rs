use std::collections::HashMap;

use lazy_static::*;
use std::sync::Mutex;
use telegram_bot::{Api, ChatId, Message, MessageKind, ToMessageId};
use tokio::{
    sync::{
        mpsc::{Receiver, Sender},
        oneshot,
    },
    time::sleep,
    time::Duration,
};

use tracing::{debug, info};

use crate::{
    deliver::Msg2Deliver,
    reminder::{Msg2Reminder, ReminderComm},
};

lazy_static! {
    static ref REMIND_PENDING_TABLE: Mutex<HashMap<ChatId, (Status, oneshot::Sender<bool>)>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

#[derive(Clone)]
enum Status {
    RemindPending,
    RemindCancelPending,
    Nil,
}

pub struct Watcher {
    api: Api,

    ch: Receiver<Message>,

    send: Sender<Msg2Deliver>,

    reminder: Sender<Msg2Reminder>,
}

impl Watcher {
    pub fn new(
        api: Api,
        ch: Receiver<Message>,
        send: Sender<Msg2Deliver>,
        reminder: Sender<Msg2Reminder>,
    ) -> Self {
        Self {
            api,
            ch,
            send,
            reminder,
        }
    }

    pub async fn run(&mut self) {
        while let Some(msg) = self.ch.recv().await {
            let status = status_checker(&msg);
            match (&msg.kind, status) {
                (MessageKind::Text { ref data, .. }, Status::Nil) => {
                    match data.to_lowercase().as_str() {
                        "remind" => {
                            //:= guard private chat

                            let id = msg.chat.id().clone();
                            let (snd, mut rec) = oneshot::channel();

                            // register sender to global status
                            REMIND_PENDING_TABLE
                                .lock()
                                .unwrap()
                                .insert(id, (Status::RemindPending, snd));

                            // send to deliver channel
                            let deliver_send = self.send.clone();

                            // bye bye async
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(3)).await;
                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Go ahead, I am listenning".into(),
                                    ))
                                    .await;

                                sleep(Duration::from_secs(5)).await;

                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Run out remind waiting time".into(),
                                    ))
                                    .await;

                                REMIND_PENDING_TABLE.lock().unwrap().remove(&id);
                            });
                        }
                        "cancelremind" => {
                            let id = msg.chat.id().clone();
                            let (snd, mut rec) = oneshot::channel();

                            // register sender to global status
                            REMIND_PENDING_TABLE
                                .lock()
                                .unwrap()
                                .insert(id, (Status::RemindCancelPending, snd));

                            // send to deliver channel
                            let deliver_send = self.send.clone();

                            // bye bye async
                            tokio::spawn(async move {
                                sleep(Duration::from_secs(3)).await;
                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Go ahead, I am listenning".into(),
                                    ))
                                    .await;

                                sleep(Duration::from_secs(5)).await;

                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Run out remind cancel waiting time".into(),
                                    ))
                                    .await;

                                REMIND_PENDING_TABLE.lock().unwrap().remove(&id);
                            });
                        }
                        _ => {}
                    }
                }
                //:= need give timer config
                (MessageKind::Text { ref data, .. }, Status::RemindPending) => {
                    if let Some((_, snd)) =
                        REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id())
                    {
                        let _ = snd.send(true);

                        self.reminder.send(Msg2Reminder::new(
                            ReminderComm::New,
                            (msg.chat.id(), msg.to_message_id().to_string()),
                            String::from(data),
                            Duration::from_secs(1800), //:= half hours, maybe change in futrue
                        ));
                    } else {
                        debug!(
                            "Cannot find this chat_id {} in pending table with data {}",
                            msg.chat.id(),
                            data
                        )
                    };
                }
                (MessageKind::Text { ref data, .. }, Status::RemindCancelPending) => {
                    if let Some((_, snd)) =
                        REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id())
                    {
                        let _ = snd.send(true); // tell timer stop

                        self.reminder.send(Msg2Reminder::new(
                            ReminderComm::Cancel,
                            (msg.chat.id(), data.clone()),
                            String::new(),
                            Duration::from_secs(1800), //:= half hours, maybe change in futrue
                        ));
                    } else {
                        debug!(
                            "Cannot find this chat_id {} in pending table with data {}",
                            msg.chat.id(),
                            data
                        )
                    };
                }
                _ => {}
            }
        }
    }
}

fn status_checker(msg: &Message) -> Status {
    if let Some((status, _)) = REMIND_PENDING_TABLE.lock().unwrap().get(&msg.chat.id()) {
        status.clone()
    } else {
        Status::Nil
    }
}
