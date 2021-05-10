use std::collections::HashMap;

use lazy_static::*;
use std::sync::Mutex;
use telegram_bot::{Api, ChatId, Message, MessageChat, MessageKind, ToMessageId};
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
    Check(CheckMsg),
    UnSpport,
    UnSpportMsg(String), // reason inside
}

enum CheckMsg {
    Nil,
    Reminder,
    //:= TODO: All
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
                "check" => Self::Check(CheckMsg::Nil),
                a @ _ => Self::UnSpportMsg(String::from(a)),
            }
        } else {
            Self::UnSpport
        };

        match (pre_comm, a.next().map(|s| s.to_lowercase())) {
            (Self::Reminder(i), Some(n)) => Self::Reminder(n.parse::<u64>().unwrap_or(i)),

            (re @ Self::Reminder(_), None) => re,

            (Self::CancelReminder(_), Some(msgid)) => Self::CancelReminder(msgid.into()),

            (Self::CancelReminder(_), None) => Self::CancelReminderPending,

            (Self::Check(_), a) => match a.as_ref().map(String::as_str) {
                Some("reminder") => Self::Check(CheckMsg::Reminder),
                _ => Self::UnSpport,
            },

            (re @ Self::UnSpport, _) => re,

            (re @ Self::UnSpportMsg(_), _) => re,

            _ => Self::UnSpport,
        }
    }
}

pub struct Watcher {
    ch: Receiver<Message>,

    send: Sender<Msg2Deliver>,

    reminder: Sender<Msg2Reminder>,
}

impl Watcher {
    pub fn new(
        _api: Api,
        ch: Receiver<Message>,
        send: Sender<Msg2Deliver>,
        reminder: Sender<Msg2Reminder>,
    ) -> Self {
        Self { ch, send, reminder }
    }

    pub async fn run(&mut self) {
        info!("Watcher is running");
        while let Some(msg) = self.ch.recv().await {
            info!("Watcher receive message: {:?}", msg);

            // check this chat window status
            let status = status_checker(&msg);

            match (&msg.kind, &msg.chat, status) {
                (MessageKind::Text { ref data, .. }, MessageChat::Private(_), Status::Nil) => {
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
                                sleep(Duration::from_secs(5)).await;
                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                match deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Go ahead, I am listenning".into(),
                                    ))
                                    .await
                                {
                                    Ok(_) => {}
                                    Err(e) => {
                                        debug!(
                                            "Error {} happens in pending watcher",
                                            e.to_string()
                                        );
                                        return;
                                    }
                                }

                                sleep(Duration::from_secs(10)).await;

                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                match deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Run out remind waiting time".into(),
                                    ))
                                    .await
                                {
                                    Ok(_) => {}
                                    Err(e) => {
                                        debug!(
                                            "Error {} happens in pending watcher",
                                            e.to_string()
                                        );
                                        return;
                                    }
                                }

                                REMIND_PENDING_TABLE.lock().unwrap().remove(&id);
                            });
                        }

                        SpecialMsg::CancelReminder(msgid) => {
                            match self
                                .reminder
                                .send(Msg2Reminder::new(
                                    ReminderComm::Cancel,
                                    (msg.chat.id(), msgid),
                                    String::new(),
                                    Duration::from_secs(0),
                                ))
                                .await
                            {
                                Ok(_) => {}
                                Err(e) => {
                                    debug!("Error {} happens in CancelReminder", e.to_string());
                                    return;
                                }
                            }
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
                                sleep(Duration::from_secs(5)).await;
                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                match deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Go ahead, I am listenning".into(),
                                    ))
                                    .await
                                {
                                    Ok(_) => {}
                                    Err(e) => {
                                        debug!(
                                            "Error {} happens in cancel pending watcher",
                                            e.to_string()
                                        );
                                        return;
                                    }
                                }

                                sleep(Duration::from_secs(10)).await;

                                if rec.try_recv().is_ok() {
                                    return;
                                }

                                match deliver_send
                                    .send(Msg2Deliver::new(
                                        "send".into(),
                                        id,
                                        "Run out remind cancel waiting time".into(),
                                    ))
                                    .await
                                {
                                    Ok(_) => {}
                                    Err(e) => {
                                        debug!(
                                            "Error {} happens in pending watcher",
                                            e.to_string()
                                        );
                                        return;
                                    }
                                }

                                REMIND_PENDING_TABLE.lock().unwrap().remove(&id);
                            });
                        }

                        SpecialMsg::Check(cm) => match cm {
                            CheckMsg::Nil => unreachable!(),
                            CheckMsg::Reminder => {
                                match self
                                    .reminder
                                    .send(Msg2Reminder::new(
                                        ReminderComm::Check,
                                        (msg.chat.id(), String::new()),
                                        String::new(),
                                        Duration::from_secs(0),
                                    ))
                                    .await
                                {
                                    Ok(_) => {}
                                    Err(e) => {
                                        debug!(
                                            "Error {} happens in reminder checking watcher",
                                            e.to_string()
                                        );
                                        return;
                                    }
                                }
                            }
                        },

                        SpecialMsg::UnSpportMsg(m) => {
                            debug!("unsupport {}", data);
                            match self
                                .send
                                .send(Msg2Deliver::new("send".into(), msg.chat.id(), m))
                                .await
                            {
                                Ok(_) => {}
                                Err(e) => {
                                    debug!("Error {} happens in pending watcher", e.to_string());
                                    return;
                                }
                            }
                        }

                        SpecialMsg::UnSpport => {
                            debug!("unsupport {}", data)
                        }
                    }
                }

                (
                    MessageKind::Text { ref data, .. },
                    MessageChat::Private(_),
                    Status::RemindPending(time),
                ) => {
                    if let Some((_, snd)) =
                        REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id())
                    {
                        let _ = snd.send(true);

                        match self
                            .reminder
                            .send(Msg2Reminder::new(
                                ReminderComm::New,
                                (msg.chat.id(), msg.to_message_id().to_string()),
                                String::from(data),
                                Duration::from_secs(60 * time),
                            ))
                            .await
                        {
                            Ok(_) => {}
                            Err(e) => {
                                debug!("Error {} happens in remind pending watcher", e.to_string());
                                return;
                            }
                        }
                    } else {
                        debug!(
                            "Cannot find this chat_id {} in pending table with data {}",
                            msg.chat.id(),
                            data
                        )
                    };
                }

                (
                    MessageKind::Text { ref data, .. },
                    MessageChat::Private(_),
                    Status::RemindCancelPending,
                ) => {
                    if let Some((_, snd)) =
                        REMIND_PENDING_TABLE.lock().unwrap().remove(&msg.chat.id())
                    {
                        let _ = snd.send(true); // tell timer stop

                        match self
                            .reminder
                            .send(Msg2Reminder::new(
                                ReminderComm::Cancel,
                                (msg.chat.id(), data.clone()),
                                String::new(),
                                Duration::from_secs(0),
                            ))
                            .await
                        {
                            Ok(_) => {}
                            Err(e) => {
                                debug!(
                                    "Error {} happens in RemindCancelPending watcher",
                                    e.to_string()
                                );
                                return;
                            }
                        }
                    } else {
                        debug!(
                            "Cannot find this chat_id {} in pending table with data {}",
                            msg.chat.id(),
                            data,
                        )
                    };
                }

                _ => {
                    match self
                        .send
                        .send(Msg2Deliver::new(
                            "send".into(),
                            msg.chat.id(),
                            "Nothing for you".into(),
                        ))
                        .await
                    {
                        Ok(_) => {}
                        Err(e) => {
                            debug!("Error {} happens in send to unspport match", e.to_string());
                            return;
                        }
                    }
                }
            }
        }
    }
}

fn status_checker(msg: &Message) -> Status {
    match REMIND_PENDING_TABLE.lock().unwrap().get(&msg.chat.id()) {
        Some((status, _)) => status.clone(),
        None => Status::Nil,
    }
}
