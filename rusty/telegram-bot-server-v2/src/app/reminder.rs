use super::*;
use lazy_static::*;
use serde_json::to_string;
use std::{
    collections::{BinaryHeap, HashMap, HashSet},
    sync::Mutex,
};
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot::{self, error::TryRecvError},
};
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

lazy_static! {
    static ref REMINDERS_TABLE: Mutex<HashMap<ChatId, HashMap<usize, oneshot::Sender<bool>>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

async fn add_reminder(
    chatid: ChatId,
    time: u64,
    content: String,
    deliver_sender: Sender<Msg2Deliver>,
) {
    let mut table = REMINDERS_TABLE.lock().unwrap();
    let reminders = table.entry(chatid).or_insert(HashMap::new());

    let mut keys = reminders.keys().collect::<Vec<_>>();
    keys.sort();
    let largest = keys.last().cloned().unwrap_or(&0);

    let (snd, mut rev) = oneshot::channel();
    reminders.insert(largest + 1, snd);

    tokio::spawn(async move {
        let dlvr_msg = Msg2Deliver::new("send".to_string(), chatid, content.to_string());
        loop {
            sleep(Duration::from_secs(time)).await;
            match rev.try_recv() {
                Ok(_) => return,
                Err(TryRecvError::Empty) => continue,
                Err(TryRecvError::Closed) => return,
            }
            deliver_sender.send(dlvr_msg.clone()).await;
        }
    });
}

fn delete_reminder(chatid: &ChatId, ind: &usize) {
    let mut table = REMINDERS_TABLE.lock().unwrap();
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    match reminders.remove(ind) {
        Some(sig) => {
            sig.send(true);
        }
        None => {
            //:= tracing here maybe
        }
    };
}

type ReminderTime = u64;

#[derive(Clone)]
pub enum ReminderStatus {
    ReminderPending(ReminderTime),
    //CancelReminderPending,
}

#[derive(Clone)]
pub enum ReminderComm {
    InitReminder(ReminderTime),
    MakeReminder(String, ReminderTime),
    /// cancel the reminder
    CancelReminder(usize),
}

/// input generated from message
pub struct ReminderInput {
    chat_id: ChatId,
    command: ReminderComm,
}

impl ReminderInput {
    pub(super) fn new(chat_id: ChatId, command: ReminderComm) -> Self {
        Self { chat_id, command }
    }

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
            Some("reminder") => Some(Self {
                chat_id: msg.chat.id(),
                command: if let Some(Ok(minutes)) = data.get(1).map(|s| s.parse::<u64>()) {
                    ReminderComm::InitReminder(minutes)
                } else {
                    ReminderComm::InitReminder(30)
                },
            }),
            Some("cancelreminder") => Some(Self {
                chat_id: msg.chat.id(),
                command: if let Some(Ok(ind)) = data.get(1).map(|s| s.parse::<usize>()) {
                    ReminderComm::CancelReminder(ind)
                } else {
                    return None;
                },
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
    pub fn sender(&self) -> Sender<ReminderInput> {
        self.sender.clone()
    }

    pub async fn run(&mut self) {
        while let Some(ref rem_input) = self.receiver.recv().await {
            match &rem_input.command {
                ReminderComm::InitReminder(time) => {
                    // let status of this chat updated
                    self.status_checker_sender
                        .send((
                            StatusCheckerInput::new(
                                rem_input.chat_id,
                                ChatStatus::ReminderApp(ReminderStatus::ReminderPending(*time)),
                                Operate::Update,
                            ),
                            None,
                        ))
                        .await;

                    // make something put to tokio
                    let status_snd = self.status_checker_sender.clone();
                    let chat_id = rem_input.chat_id;
                    let deliver_sender = self.deliver_sender.clone();
                    // awaiting guard
                    tokio::spawn(awaiting_reminder(status_snd, chat_id, deliver_sender));
                }
                ReminderComm::MakeReminder(msg, time) => {
                    add_reminder(
                        rem_input.chat_id,
                        *time,
                        msg.to_string(),
                        self.deliver_sender.clone(),
                    )
                    .await
                }
                ReminderComm::CancelReminder(ind) => delete_reminder(&rem_input.chat_id, ind),
            }
        }
    }
}

async fn awaiting_reminder(
    status_snd: Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
    chat_id: ChatId,
    deliver_sender: Sender<Msg2Deliver>,
) {
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
        Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending(_))) => {
            deliver_sender.send(Msg2Deliver::new(
                "send".to_string(),
                chat_id,
                "Go ahead, I am listening".to_string(),
            ));
        }
        Ok(ChatStatus::None) => {
            //:= after reminder created, status is none
            return;
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
        Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending(_))) => {
            deliver_sender.send(Msg2Deliver::new(
                "send".to_string(),
                chat_id,
                "Run out remind waiting time".to_string(),
            ));
        }
        Ok(ChatStatus::None) => {
            //:= after reminder created, status is none
            return;
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
}
