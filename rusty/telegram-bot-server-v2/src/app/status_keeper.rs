use super::*;
use async_trait::async_trait;
use lazy_static::*;
use std::collections::HashMap;
use telegram_bot::{ChatId, MessageChat, MessageKind};
use tokio::sync::{oneshot, Mutex};

lazy_static! {
    static ref CHAT_STATUS_TABLE: Mutex<HashMap<ChatId, ChatStatus>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

#[derive(Clone)]
pub enum ChatStatus {
    None,
    ReminderApp(ReminderStatus),
}

pub enum Operate {
    Update,
    Query,
    Delete,
}

/// catch message first check if this chat has
/// status inside or not
#[derive(Clone)]
pub struct StatusCheckerCatcher {
    reminder_sender: Sender<ReminderInput>,
}

impl StatusCheckerCatcher {
    pub fn new(reminder_sender: Sender<ReminderInput>) -> Self {
        Self { reminder_sender }
    }

    /// check msg if there is status of this chat
    /// return Some(_) means it truly has, None is not
    async fn check_from_msg(&self, msg: &Message) -> Option<()> {
        let data = match (&msg.chat, &msg.kind) {
            (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(data),
            _ => None,
        };

        let mut tb = CHAT_STATUS_TABLE.lock().await;
        match tb.get(&msg.chat.id()) {
            Some(status) => match status {
                ChatStatus::None => None,
                ChatStatus::ReminderApp(reminder_status) => {
                    match (reminder_status, data) {
                        (ReminderStatus::ReminderPending(time), Some(dd)) => {
                            self.reminder_sender
                                .send(ReminderInput::new(
                                    msg.chat.id(),
                                    ReminderComm::MakeReminder(dd.to_string(), *time),
                                ))
                                .await;
                        }
                        (ReminderStatus::ReminderPending(_), None) => (), //:= TODO: need to tell chat empty isn't accept
                    }
                    //:= send reminder_status
                    tb.remove(&msg.chat.id());
                    Some(())
                }
            },
            None => None,
        }
    }
}

/// The input for status checker use for recording the status
/// sent from other app
pub struct StatusCheckerInput {
    /// this chatid
    chat_id: ChatId,
    /// the status want to update
    update_status: ChatStatus,
    /// operation
    ops: Operate,
}

impl StatusCheckerInput {
    pub fn new(chat_id: ChatId, update_status: ChatStatus, ops: Operate) -> Self {
        Self {
            chat_id,
            update_status,
            ops,
        }
    }
}

struct StatusChecker {
    // keep this clone inside for AppConsumer trait
    checker_catcher_clone: StatusCheckerCatcher,

    sender: Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
    receiver: Receiver<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
}

impl StatusChecker {
    pub fn new(catcher: StatusCheckerCatcher) -> Self {
        let (snd, rev) = mpsc::channel(10);
        Self {
            checker_catcher_clone: catcher,
            sender: snd,
            receiver: rev,
        }
    }

    /// give clone of sender
    /// used by other app which want to check the status
    pub fn sender(&self) -> Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)> {
        self.sender.clone()
    }

    pub async fn run(&mut self) {
        while let Some((check_input, send_back)) = self.receiver.recv().await {
            match check_input.ops {
                //:= Update and Delete can return msg to send_back if up stream support
                Operate::Update => {
                    let mut tb = CHAT_STATUS_TABLE.lock().await;
                    let en = tb.entry(check_input.chat_id).or_insert(ChatStatus::None);
                    *en = check_input.update_status;
                }
                Operate::Query => {
                    // Query has to send back something or awaiting_reminder will block forever
                    let send_back_result = if let Some(record) =
                        CHAT_STATUS_TABLE.lock().await.get(&check_input.chat_id)
                    {
                        record.clone()
                    } else {
                        ChatStatus::None
                    };

                    if let Some(snd) = send_back {
                        match snd.send(send_back_result.clone()) {
                            //:= fix todo!()
                            Ok(_) => todo!(),
                            Err(_) => todo!(),
                        }
                    } else {
                        //:= TODO: finish this
                    }
                }
                Operate::Delete => {
                    //:= maybe check this option
                    CHAT_STATUS_TABLE.lock().await.remove(&check_input.chat_id);
                }
            }
        }
    }
}

/// Apply app traits below

#[async_trait]
impl AppConsumer for StatusCheckerCatcher {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        match self.check_from_msg(msg).await {
            Some(_) => Ok(ConsumeStatus::Taken),
            None => Ok(ConsumeStatus::NotMine),
        }
    }
}

#[async_trait]
impl App for StatusChecker {
    type Consumer = StatusCheckerCatcher;

    fn consumer(&self) -> Self::Consumer {
        self.checker_catcher_clone.clone()
    }

    async fn run(mut self) -> Result<(), String> {
        info!("app echo is running");
        self.run().await;
        Ok(())
    }
}
