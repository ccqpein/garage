use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    sync::Mutex,
};

use super::*;
use lazy_static::*;
use serde_json::to_string;
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot,
};
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

lazy_static! {
    static ref REMINDERS_TABLE: Mutex<HashMap<ChatId, HashMap<usize, String>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

fn add_reminder(chatid: &ChatId, content: &String) {
    let mut table = REMINDERS_TABLE.lock().unwrap();
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    let mut keys = reminders.keys().collect::<Vec<_>>();
    keys.sort();
    let largest = keys.last().cloned().unwrap_or(&0);

    reminders.insert(largest + 1, content.to_string());
    //:= need to return something
}

fn delete_reminder(chatid: &ChatId, ind: usize) {
    let mut table = REMINDERS_TABLE.lock().unwrap();
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    reminders.remove(&ind);
    //:= need to return something
}

type ReminderTime = u64;

#[derive(Clone)]
pub enum ReminderStatus {
    Reminder(ReminderTime),
    ReminderPending,

    /// cancel the reminder
    CancelReminder(ChatId),
    CancelReminderPending,
}

impl From<&'_ ReminderInput> for ReminderStatus {
    fn from(input: &'_ ReminderInput) -> Self {
        match input.msg[0].as_str() {
            "reminder" => {
                if let Some(Ok(minutes)) = input.msg.get(1).map(|s| s.parse::<u64>()) {
                    ReminderStatus::Reminder(minutes)
                } else {
                    ReminderStatus::Reminder(30)
                }
            }
            "cancelreminder" => ReminderStatus::CancelReminder(input.chat_id),
            _ => unreachable!(),
        }
    }
}

/// input generated from message
struct ReminderInput {
    chat_id: ChatId,
    msg: Vec<String>,
}

impl ReminderInput {
    fn from_msg(msg: &Message) -> Option<Self> {
        let data = match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(data),
            _ => None,
        };

        let data: Vec<_> = if let Some(line) = data {
            line.split_whitespace().map(|s| s.to_lowercase()).collect()
        } else {
            return None;
        };

        match data.get(0).map(|s| s.as_str()) {
            Some("reminder") | Some("cancelreminder") => Some(Self {
                chat_id: msg.chat.id(),
                msg: data,
            }),
            _ => None,
        }
    }
}

/// Reminder app
struct Reminder {
    sender: Sender<ReminderInput>,
    receiver: Receiver<ReminderInput>,

    status_checker_sender: Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl Reminder {
    pub async fn run(&mut self) {
        while let Some(ref rem_input) = self.receiver.recv().await {
            let re_status: ReminderStatus = rem_input.into(); //:= need chat id status keeping
            match re_status {
                ReminderStatus::Reminder(time) => {
                    // let status of this chat updated
                    self.status_checker_sender
                        .send((
                            StatusCheckerInput::new(
                                rem_input.chat_id,
                                ChatStatus::ReminderApp(ReminderStatus::ReminderPending),
                                Operate::Update,
                            ),
                            None,
                        ))
                        .await;

                    // make something put to tokio
                    let status_snd = self.status_checker_sender.clone();
                    let chat_id = rem_input.chat_id;
                    let deliver_sender = self.deliver_sender.clone();
                    tokio::spawn(async move {
                        sleep(Duration::from_secs(5)).await;
                        let (snd, mut rev) = oneshot::channel();
                        // get the status of this chat
                        status_snd.send((
                            StatusCheckerInput::new(
                                chat_id,
                                ChatStatus::None, // just for placeholder
                                Operate::Query,
                            ),
                            Some(snd),
                        )); //:= check result maybe

                        match rev.await {
                            Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending)) => {
                                deliver_sender.send(Msg2Deliver::new(
                                    "send".to_string(),
                                    chat_id,
                                    "Go ahead, I am listening".to_string(),
                                ));
                            }
                            Ok(ChatStatus::None) => {
                                //:= after reminder created, status is none
                                ()
                            }
                            Err(_) => todo!(),
                            _ => todo!(),
                        };

                        sleep(Duration::from_secs(10)).await;

                        let (snd, mut rev) = oneshot::channel();
                        status_snd.send((
                            StatusCheckerInput::new(
                                chat_id,
                                ChatStatus::None, // just for placeholder
                                Operate::Query,
                            ),
                            Some(snd),
                        )); //:= check result maybe

                        match rev.await {
                            Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending)) => {
                                deliver_sender.send(Msg2Deliver::new(
                                    "send".to_string(),
                                    chat_id,
                                    "Run out remind waiting time".to_string(),
                                ));
                            }
                            Ok(ChatStatus::None) => {
                                //:= after reminder created, status is none
                            }
                            Err(_) => todo!(),
                            _ => todo!(),
                        };

                        status_snd.send((
                            StatusCheckerInput::new(
                                chat_id,
                                ChatStatus::None, // just for placeholder
                                Operate::Delete,
                            ),
                            None,
                        ));
                    });
                }
                // should check in inside status checker
                //ReminderStatus::ReminderPending => todo!(),
                ReminderStatus::CancelReminder(_) => todo!(),
                // should check in inside status checker
                ReminderStatus::CancelReminderPending => todo!(),
                _ => unreachable!(),
            }
        }
    }
}
