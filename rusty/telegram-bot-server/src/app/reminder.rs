use std::collections::HashMap;

use async_trait::async_trait;
use telegram_bot::ChatId;
use tokio::sync::oneshot;
use tracing::{debug, info};

use super::App;

enum ReminderStatus {
    /// make new reminder
    Reminder(u64), // time of reminder
    ReminderPending,

    /// cancel the reminder
    CancelReminder(String),
    CancelReminderPending,
}

struct Reminder {
    status_table: HashMap<ChatId, (ReminderStatus, oneshot::Sender<bool>)>,
}

impl Reminder {
    fn new() -> Self {
        todo!()
    }

    /// check this chat window's status
    pub fn status_checker() -> Option<ReminderStatus> {
        todo!()
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_slice_pattern() {
        let testcase = ["a", "b"];
        let [a, b, ..] = testcase;
        assert_eq!(a, "a");
        assert_eq!(b, "b");

        let testcase = ["a", ""];
        let [a, b, ..] = testcase;
        assert_eq!(a, "a");
        assert_eq!(b, "");

        let testcase = "a".split_whitespace();
        match testcase.clone().collect::<Vec<_>>()[..] {
            [_a, _b, ..] => {
                panic!();
            }
            [_a, _b] => {
                panic!();
            }
            [a] => {
                assert_eq!(a, "a");
            }
            _ => panic!("wrong"),
        }
    }
}
