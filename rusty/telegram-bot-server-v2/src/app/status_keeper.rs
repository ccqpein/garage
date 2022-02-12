use std::collections::HashMap;

use super::*;
use lazy_static::*;
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
pub struct StatusCheckerCatcher {
    reminder_sender: Sender<ReminderInput>,
}

impl StatusCheckerCatcher {
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

// The input for status checker use for recording the status
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
    sender: Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
    receiver: Receiver<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
}

impl StatusChecker {
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
