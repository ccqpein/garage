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
    RemindPending(u64),
    RemindCancelPending,
    Nil,
}

enum SpecialMsg {
    Reminder(u64), // time of reminder
    CancelReminder(String),
    CancelReminderPending,
    UnSpport,
}

impl From<&String> for SpecialMsg {
    fn from(command: &String) -> Self {
        let mut a = command.split_whitespace();
        let pre_comm = if let Some(comm) = a.next() {
            match comm.to_lowercase().as_str() {
                "remind" => {
                    Self::Reminder(30) // default is 30 mins
                }
                "cancelremind" => Self::CancelReminder(String::new()),
                _ => Self::UnSpport,
            }
        } else {
            Self::UnSpport
        };

        match (pre_comm, a.next()) {
            (Self::Reminder(i), Some(n)) => Self::Reminder(n.parse::<u64>().unwrap_or(i)),

            (re @ Self::Reminder(_), None) => re,

            (Self::CancelReminder(_), Some(msgid)) => Self::CancelReminder(msgid.into()),

            (Self::CancelReminder(_), None) => Self::CancelReminderPending,

            (re @ Self::UnSpport, _) => re,

            _ => Self::UnSpport,
        }
    }
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
        info!("Watcher is running");
        while let Some(msg) = self.ch.recv().await {
            info!("Watcher receive message: {:?}", msg);
            // check this chat window status
            let status = status_checker(&msg);

            match (&msg.kind, status) {
                (MessageKind::Text { ref data, .. }, Status::Nil) => {
                    match SpecialMsg::from(data) {
                        SpecialMsg::Reminder(time) => {
                            //:= guard private chat

                            let id = msg.chat.id().clone();
                            let (snd, mut rec) = oneshot::channel();

                            // register sender to global status
                            REMIND_PENDING_TABLE
                                .lock()
                                .unwrap()
                                .insert(id, (Status::RemindPending(time), snd));

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

                        SpecialMsg::CancelReminder(msgid) => {
                            self.reminder
                                .send(Msg2Reminder::new(
                                    ReminderComm::Cancel,
                                    (msg.chat.id(), msgid),
                                    String::new(),
                                    Duration::from_secs(0),
                                ))
                                .await;
                        }

                        SpecialMsg::CancelReminderPending => {
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
                        SpecialMsg::UnSpport => {
                            debug!("unsupport {}", data)
                        }
                    }
                }

                (MessageKind::Text { ref data, .. }, Status::RemindPending(time)) => {
                    if let Some((_, snd)) =
                        REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id())
                    {
                        let _ = snd.send(true);

                        self.reminder
                            .send(Msg2Reminder::new(
                                ReminderComm::New,
                                (msg.chat.id(), msg.to_message_id().to_string()),
                                String::from(data),
                                Duration::from_secs(60 * time),
                            ))
                            .await;
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

                        self.reminder
                            .send(Msg2Reminder::new(
                                ReminderComm::Cancel,
                                (msg.chat.id(), data.clone()),
                                String::new(),
                                Duration::from_secs(0),
                            ))
                            .await;
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
