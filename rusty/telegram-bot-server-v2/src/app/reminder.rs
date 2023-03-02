use super::*;
use chrono::{Timelike, Utc};
use chrono_tz::America::New_York;
use lazy_static::*;
use std::collections::HashMap;
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::{
    sync::{
        mpsc::{self, error::SendError, Receiver, Sender},
        oneshot::{self, error::TryRecvError},
        Mutex,
    },
    time::{sleep, Duration},
};
use tracing::debug;

lazy_static! {
    static ref REMINDERS_TABLE: Mutex<HashMap<ChatId, HashMap<usize, oneshot::Sender<bool>>>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

const REMINDER_APP_NAME: AppName = AppName("Reminder");

/// add reminder to this chat window
async fn add_reminder(
    chatid: ChatId,
    time: ReminderTime,
    content: String,
    deliver_sender: Sender<Msg2Deliver>,
) -> Result<(), SendError<Msg2Deliver>> {
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
                "reminder {} created, remind you {}",
                largest + 1,
                time.reminder_notification()
            ),
            None,
        ))
        .await?;

    tokio::spawn(async move {
        let reminder_detail = Msg2Deliver::new(
            "send".to_string(),
            chatid,
            format!("reminder {}:", largest + 1),
            None,
        );
        let dlvr_msg = Msg2Deliver::new("send".to_string(), chatid, content.to_string(), None);
        let mut brk;

        loop {
            brk = wait_until(&time).await.unwrap();

            match rev.try_recv() {
                Err(TryRecvError::Empty) => {
                    deliver_sender
                        .send(reminder_detail.clone())
                        .await
                        .map_err(|e| debug!("error in sending reminder {}", e))
                        .unwrap();

                    deliver_sender
                        .send(dlvr_msg.clone())
                        .await
                        .map_err(|e| debug!("error in sending reminder {}", e))
                        .unwrap();
                }
                _ => return,
            }

            if brk {
                delete_reminder(&chatid, &(largest + 1), deliver_sender).await;
                return;
            }
        }
    });
    Ok(())
}

async fn wait_until(rt: &ReminderTime) -> Result<bool, String> {
    match rt {
        a @ ReminderTime::D { .. } => sleep(a.to_duration()?).await,
        ReminderTime::T((hh, mm)) => {
            loop {
                let now_utc = Utc::now();
                let now_eastern = now_utc.with_timezone(&New_York);

                let (h, m) = (now_eastern.hour(), now_eastern.minute());

                if *hh == h && *mm == m {
                    break;
                }

                sleep(tokio::time::Duration::from_secs(60)).await;
            }
            return Ok(true);
        }
    }
    Ok(false)
}

async fn delete_reminder(chatid: &ChatId, ind: &usize, deliver_sender: Sender<Msg2Deliver>) {
    let mut table = REMINDERS_TABLE.lock().await;
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    let dlvr_msg = match reminders.remove(ind) {
        Some(sig) => {
            match sig.send(true) {
                Ok(_) => {
                    info!("send back for deleting reminder successfully")
                }
                Err(_e) => {
                    debug!("send back for deleting reminder {} has error", ind)
                }
            }

            Msg2Deliver::new(
                "send".to_string(),
                *chatid,
                format!("reminder {} cancelled", ind),
                None,
            )
        }
        None => Msg2Deliver::new(
            "send".to_string(),
            *chatid,
            format!("cannot find number {} reminder", ind),
            None,
        ),
    };

    deliver_sender
        .send(dlvr_msg)
        .await
        .map_err(|e| debug!("error in sending reminder {}", e))
        .unwrap();
}

async fn clean_reminder(chatid: &ChatId, deliver_sender: Sender<Msg2Deliver>) {
    let mut table = REMINDERS_TABLE.lock().await;
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    for (ind, sig) in reminders.drain() {
        match sig.send(true) {
            Ok(_) => {
                info!("send back for deleting reminder successfully")
            }
            Err(_e) => {
                debug!("send back for deleting reminder {} has error", ind)
            }
        }
    }

    deliver_sender
        .send(Msg2Deliver::new(
            "send".to_string(),
            *chatid,
            format!("reminder cleaned"),
            None,
        ))
        .await
        .map_err(|e| debug!("error in sending reminder {}", e))
        .unwrap();
}

async fn list_reminder(chatid: &ChatId, deliver_sender: Sender<Msg2Deliver>) {
    let mut table = REMINDERS_TABLE.lock().await;
    let reminders = table.entry(*chatid).or_insert(HashMap::new());

    //:= TODO: list summary? details? or just numbers
}

#[derive(Clone, Debug)]
pub enum ReminderTime {
    D { unit: String, num: u64 },
    T((u32, u32)),
}

impl ReminderTime {
    fn from_sec(s: u64) -> Self {
        Self::D {
            num: s,
            unit: String::from("seconds"),
        }
    }

    fn to_duration(&self) -> Result<Duration, String> {
        match self {
            ReminderTime::D { unit, num } => match unit.as_str() {
                "minutes" => {
                    let (s, flag) = num.overflowing_mul(60);
                    if !flag {
                        Ok(Duration::from_secs(s))
                    } else {
                        Err(String::from("to duration overflow"))
                    }
                }
                "seconds" => Ok(Duration::from_secs(*num)),
                "hours" => {
                    let (s, flag) = num.overflowing_mul(3600);
                    if !flag {
                        Ok(Duration::from_secs(s))
                    } else {
                        Err(String::from("to duration overflow"))
                    }
                }
                "days" => {
                    let (s, flag) = num.overflowing_mul(3600 * 24);
                    if !flag {
                        Ok(Duration::from_secs(s))
                    } else {
                        Err(String::from("to duration overflow"))
                    }
                }
                _ => unreachable!(),
            },
            ReminderTime::T(_) => Err(String::from("specifically time cannot make duration")),
        }
    }

    /// parse reminder string like 11m/23s/1h/2d
    /// only accept s, m, h, d
    /// or specifically time like 12:45
    fn parse(s: &str) -> Result<Self, String> {
        // if give number directly, it should be seconds
        if let Ok(ss) = s.parse::<u64>() {
            return Ok(Self::from_sec(ss));
        }

        match s
            .split(':')
            .filter_map(|a| a.parse::<u32>().ok())
            .collect::<Vec<u32>>()[..]
        {
            [h, m] => return Ok(Self::T((h, m))),
            _ => (),
        }

        let (time, unit) = s.split_at(s.len() - 1);
        match time.parse::<u64>() {
            Ok(tt) => match unit {
                "m" => Ok(Self::D {
                    unit: String::from("minutes"),
                    num: tt,
                }),
                "s" => Ok(Self::D {
                    unit: String::from("seconds"),
                    num: tt,
                }),
                "h" => Ok(Self::D {
                    unit: String::from("hours"),
                    num: tt,
                }),
                "d" => Ok(Self::D {
                    unit: String::from("days"),
                    num: tt,
                }),
                _ => return Err("unsupported unit, has to be s, m, h, d".to_string()),
            },
            Err(_e) => Err(format!("this time {} parsed failed", time)),
        }
    }

    fn reminder_notification(&self) -> String {
        match self {
            ReminderTime::D { unit, num } => format!("every {} {}", num, unit),
            ReminderTime::T((h, m)) => format!("on {}:{:02}", h, m),
        }
    }
}

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
    CancelReminder(usize),

    /// clean all reminders
    CleanReminders,

    /// List all reminders
    ListReminders,

    /// for from_msg using when parsing message to command has
    /// error
    ErrorCommand(String),
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

    /// for command
    fn from_msg_v2(msg: &Message) -> Option<Self> {
        match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { ref data, entities }) => {
                match entities.get(0) {
                    Some(en) => match en.kind {
                        telegram_bot::MessageEntityKind::BotCommand => {
                            if data == "/clean_reminder" {
                                info!("receive command {}", data);
                                Some(Self {
                                    chat_id: msg.chat.id(),
                                    command: ReminderComm::CleanReminders,
                                })
                            } else if data.starts_with("/list_reminders") {
                                info!("receive command {}", data);
                                //:= TODO: new command
                                None
                            } else if data.starts_with("/cancel_reminder") {
                                info!("receive command {}", data);
                                if let Some(ind) = data.get(en.length as usize + 1..) {
                                    if let Ok(d) = ind.parse::<usize>() {
                                        return Some(Self {
                                            chat_id: msg.chat.id(),
                                            command: ReminderComm::CancelReminder(d),
                                        });
                                    }
                                }

                                None
                            } else {
                                None
                            }
                        }
                        _ => None,
                    },
                    None => None,
                }
            }
            _ => None,
        }
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
            Some("reminder") => {
                let comm = data.get(1).map_or(
                    ReminderComm::InitReminder(ReminderTime::parse("30m").unwrap()),
                    |time| {
                        ReminderTime::parse(time).map_or_else(
                            |e| ReminderComm::ErrorCommand(e.to_string()),
                            |r_time| ReminderComm::InitReminder(r_time),
                        )
                    },
                );
                Some(Self {
                    chat_id: msg.chat.id(),
                    command: comm,
                })
            }
            Some("cancelreminder") => {
                let comm = data.get(1).map_or(
                    ReminderComm::ErrorCommand(
                        "cancelreminder command should has followed reminder id".into(),
                    ),
                    |s| {
                        s.parse::<usize>()
                            .map_or(ReminderComm::ErrorCommand("id parse failed".into()), |id| {
                                ReminderComm::CancelReminder(id)
                            })
                    },
                );
                Some(Self {
                    chat_id: msg.chat.id(),
                    command: comm,
                })
            }
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
                                REMINDER_APP_NAME,
                                rem_input.chat_id,
                                ChatStatus::ReminderApp(ReminderStatus::ReminderPending(
                                    time.clone(),
                                )),
                                Operate::Update,
                                None,
                            )
                            .unwrap(),
                        )
                        .await
                        .map_err(|e| debug!("status_checker_sender send error {}", e))
                        .unwrap();

                    // make something put to tokio
                    let status_snd = self.status_checker_sender.clone();
                    let chat_id = rem_input.chat_id;
                    let deliver_sender = self.deliver_sender.clone();
                    // awaiting guard
                    tokio::spawn(awaiting_reminder(status_snd, chat_id, deliver_sender));
                }
                ReminderComm::MakeReminder(msg, time) => {
                    // make reminder
                    add_reminder(
                        rem_input.chat_id,
                        time.clone(),
                        msg.to_string(),
                        self.deliver_sender.clone(),
                    )
                    .await
                    .map_err(|e| {
                        debug!("{}", e);
                    })
                    .unwrap();
                }
                ReminderComm::CancelReminder(ind) => {
                    delete_reminder(&rem_input.chat_id, &ind, self.deliver_sender.clone()).await
                }
                ReminderComm::ErrorCommand(err_msg) => {
                    self.deliver_sender
                        .send(Msg2Deliver::new(
                            "send".to_string(),
                            rem_input.chat_id,
                            err_msg.to_owned(),
                            None,
                        ))
                        .await
                        .map_err(|e| {
                            debug!("{}", e);
                        })
                        .unwrap();
                }
                ReminderComm::CleanReminders => {
                    clean_reminder(&rem_input.chat_id, self.deliver_sender.clone()).await
                }
                ReminderComm::ListReminders => {
                    list_reminder(&rem_input.chat_id, self.deliver_sender.clone()).await
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
    let (snd, rev) = oneshot::channel();
    // get the status of this chat
    if let Err(e) = status_snd
        .send(
            StatusCheckerInput::new(
                REMINDER_APP_NAME,
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
                    None,
                ))
                .await
                .map_err(|e| {
                    debug!("{}", e);
                })
                .unwrap();
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
    };

    sleep(Duration::from_secs(10)).await;

    let (snd, rev) = oneshot::channel();
    if let Err(e) = status_snd
        .send(
            StatusCheckerInput::new(
                REMINDER_APP_NAME,
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
                    None,
                ))
                .await
                .map_err(|e| {
                    debug!("{}", e);
                })
                .unwrap();

            status_snd
                .send(
                    StatusCheckerInput::new(
                        REMINDER_APP_NAME,
                        chat_id,
                        ChatStatus::None, // just for placeholder
                        Operate::Delete,
                        None,
                    )
                    .unwrap(),
                )
                .await
                .map_err(|e| {
                    debug!("{}", e);
                })
                .unwrap();
        }
        Ok(ChatStatus::None) => {
            //after reminder created, status is none
            return;
        }
        Err(e) => {
            debug!("awaiting reminder checking status has issue {:?}", e);
            return;
        }
    };
}

/// impl app below

#[async_trait]
impl AppConsumer for ReminderInputConsumer {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match ReminderInput::from_msg_v2(msg) {
            Some(input) => {
                self.snd.send(input).await.map_err(|e| e.to_string())?;
                return Ok(ConsumeStatus::Taken);
            }
            None => (),
        };

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
