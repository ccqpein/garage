use std::collections::HashMap;

use super::*;
use lazy_static::*;
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot, Mutex,
};
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

lazy_static! {
    static ref REMIND_PENDING_TABLE: Mutex<HashMap<ChatId, (ReminderStatus, oneshot::Sender<bool>)>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

type ReminderTime = u64;

enum ReminderStatus {
    Reminder(ReminderTime),
    ReminderPending,

    /// cancel the reminder
    CancelReminder(ChatId),
    CancelReminderPending,
}

struct ReminderInput {
    chat_id: ChatId,
    msg: Vec<String>,
}

impl ReminderInput {
    //:= From here
    // fn from_msg(msg: &Message) -> Option<Self> {
    //     let data = match (&msg.chat, &msg.kind) {
    //         (MessageChat::Private(_), MessageKind::Text { ref data, .. }) => Some(data),
    //         _ => None,
    //     };

    //     match data.map(|s| s.to_lowercase().as_str()) {
    //         Some("reminder") => {}
    //         _ => None,
    //     }
    // }
}

/// Reminder app
struct Reminder {
    sender: Sender<ReminderInput>,
    receiver: Receiver<ReminderInput>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl Reminder {
    pub async fn run(&mut self) {
        while let Some(rem_input) = self.receiver.recv().await {}
    }
}
