use super::*;
use lazy_static::*;
use serde_json::to_string;
use std::collections::{BinaryHeap, HashMap, HashSet};
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot::{self, error::TryRecvError},
    Mutex,
};
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

lazy_static! {
    static ref REMINDERS_TABLE: Mutex<HashMap<ChatId, HashMap<usize, oneshot::Sender<bool>>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

/// add reminder to this chat window
async fn add_reminder(
    chatid: ChatId,
    time: u64,
    content: String,
    deliver_sender: Sender<Msg2Deliver>,
) {
    let mut table = REMINDERS_TABLE.lock().await;
    let reminders = table.entry(chatid).or_insert(HashMap::new());

    let mut keys = reminders.keys().cloned().collect::<Vec<_>>();
    keys.sort();
    let largest = keys.last().cloned().unwrap_or(0);

    let (snd, mut rev) = oneshot::channel();
    reminders.insert(largest + 1, snd);

    deliver_sender
        .send(Msg2Deliver::new(
            "send".to_string(),
            chatid,
            format!(
                "reminder {} created, remind you every {} seconds",
                largest + 1,
                time
            ),
        ))
        .await;

    tokio::spawn(async move {
        let dlvr_msg = Msg2Deliver::new("send".to_string(), chatid, content.to_string());
        loop {
            sleep(Duration::from_secs(time)).await;
            match rev.try_recv() {
                Err(TryRecvError::Empty) => {
                    deliver_sender.send(dlvr_msg.clone()).await;
                }
                _ => return,
            }
        }
    });
}

async fn delete_reminder(chatid: &ChatId, ind: &usize, deliver_sender: Sender<Msg2Deliver>) {
    let mut table = REMINDERS_TABLE.lock().await;
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    let dlvr_msg = match reminders.remove(ind) {
        Some(sig) => {
            sig.send(true);
            Msg2Deliver::new(
                "send".to_string(),
                *chatid,
                format!("reminder {} cancelled", ind),
            )
        }
        None => Msg2Deliver::new(
            "send".to_string(),
            *chatid,
            format!("cannot find number {} reminder", ind),
        ),
    };

    deliver_sender.send(dlvr_msg).await;
}

type ReminderTime = u64;

#[derive(Clone, Debug)]
pub enum ReminderStatus {
    /// awaiting reminder content
    ReminderPending(ReminderTime),
}

#[derive(Clone)]
pub enum ReminderComm {
    InitReminder(ReminderTime),
    MakeReminder(String, ReminderTime),
    /// cancel the reminder
    CancelReminder(usize), //:= need finish this
}

pub struct ReminderInputConsumer {
    snd: Sender<ReminderInput>,
}

impl ReminderInputConsumer {
    pub fn new(snd: Sender<ReminderInput>) -> Self {
        Self { snd }
    }
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
pub struct Reminder {
    sender: Sender<ReminderInput>,
    receiver: Receiver<ReminderInput>,

    status_checker_sender: Sender<StatusCheckerInput>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl Reminder {
    pub fn new(
        deliver_sender: Sender<Msg2Deliver>,
        status_checker_sender: Sender<StatusCheckerInput>,
    ) -> Self {
        let (snd, rev) = mpsc::channel(10);
        Self {
            sender: snd,
            receiver: rev,
            status_checker_sender,
            deliver_sender,
        }
    }

    pub fn sender(&self) -> Sender<ReminderInput> {
        self.sender.clone()
    }

    pub async fn run(&mut self) {
        while let Some(ref rem_input) = self.receiver.recv().await {
            match &rem_input.command {
                ReminderComm::InitReminder(time) => {
                    // let status of this chat updated
                    self.status_checker_sender
                        .send(
                            StatusCheckerInput::new(
                                rem_input.chat_id,
                                ChatStatus::ReminderApp(ReminderStatus::ReminderPending(*time)),
                                Operate::Update,
                                None,
                            )
                            .unwrap(),
                        )
                        .await;

                    // make something put to tokio
                    let status_snd = self.status_checker_sender.clone();
                    let chat_id = rem_input.chat_id;
                    let deliver_sender = self.deliver_sender.clone();
                    // awaiting guard
                    tokio::spawn(awaiting_reminder(status_snd, chat_id, deliver_sender));
                }
                ReminderComm::MakeReminder(msg, time) => {
                    // make reminder
                    let new_reminder_id = add_reminder(
                        rem_input.chat_id,
                        *time,
                        msg.to_string(),
                        self.deliver_sender.clone(),
                    )
                    .await;
                }
                ReminderComm::CancelReminder(ind) => {
                    delete_reminder(&rem_input.chat_id, ind, self.deliver_sender.clone()).await
                }
            }
        }
    }
}

/// this part is the guard for checking if reminder content has sent or not
async fn awaiting_reminder(
    status_snd: Sender<StatusCheckerInput>,
    chat_id: ChatId,
    deliver_sender: Sender<Msg2Deliver>,
) {
    sleep(Duration::from_secs(5)).await;
    let (snd, mut rev) = oneshot::channel();
    // get the status of this chat
    if let Err(e) = status_snd
        .send(
            StatusCheckerInput::new(
                chat_id,
                ChatStatus::None, // just for placeholder
                Operate::Query,
                Some(snd),
            )
            .unwrap(),
        )
        .await
    {
        debug!("error {} happens when query status for chat {}", e, chat_id);
        return;
    }

    match rev.await {
        Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending(_))) => {
            deliver_sender
                .send(Msg2Deliver::new(
                    "send".to_string(),
                    chat_id,
                    "Go ahead, I am listening".to_string(),
                ))
                .await;
        }
        Ok(ChatStatus::None) => {
            // after reminder created, status is none
            return;
        }
        Err(e) => {
            debug!(
                "awaiting reminder checking status has issue {}",
                e.to_string()
            );
            return;
        }
        _ => {
            debug!("awaiting reminder checking status has issue: unsupport reply");
            return;
        }
    };

    sleep(Duration::from_secs(10)).await;

    let (snd, mut rev) = oneshot::channel();
    if let Err(e) = status_snd
        .send(
            StatusCheckerInput::new(
                chat_id,
                ChatStatus::None, // just for placeholder
                Operate::Query,
                Some(snd),
            )
            .unwrap(),
        )
        .await
    {
        debug!("error {} happens when query status for chat {}", e, chat_id);
        return;
    }

    match rev.await {
        Ok(ChatStatus::ReminderApp(ReminderStatus::ReminderPending(_))) => {
            deliver_sender
                .send(Msg2Deliver::new(
                    "send".to_string(),
                    chat_id,
                    "Run out remind waiting time".to_string(),
                ))
                .await;

            status_snd
                .send(
                    StatusCheckerInput::new(
                        chat_id,
                        ChatStatus::None, // just for placeholder
                        Operate::Delete,
                        None,
                    )
                    .unwrap(),
                )
                .await;
        }
        Ok(ChatStatus::None) => {
            //after reminder created, status is none
            return;
        }
        Err(e) => {
            debug!("awaiting reminder checking status has issue {:?}", e);
            return;
        }
        _ => {
            debug!("awaiting reminder checking status has issue: unsupport reply");
            return;
        }
    };
}

/// impl app below

#[async_trait]
impl AppConsumer for ReminderInputConsumer {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match ReminderInput::from_msg(msg) {
            Some(input) => {
                self.snd.send(input).await.map_err(|e| e.to_string())?;
                Ok(ConsumeStatus::Taken)
            }
            None => Ok(ConsumeStatus::NotMine),
        }
    }
}

#[async_trait]
impl App for Reminder {
    type Consumer = ReminderInputConsumer;

    fn consumer(&self) -> Self::Consumer {
        ReminderInputConsumer::new(self.sender.clone())
    }

    async fn run(mut self) -> Result<(), String> {
        Reminder::run(&mut self).await;
        Ok(())
    }
}
