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
    //:= TODO: input from message
}

/// Reminder app
struct Reminder {}
