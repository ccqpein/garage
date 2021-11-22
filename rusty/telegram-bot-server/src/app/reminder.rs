use std::collections::HashMap;

use telegram_bot::ChatId;
use tokio::sync::oneshot;

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

// #[async_trait]
// impl<'a> App<'a> for Reminder {
//     type Input;

//     type Output;

//     fn match_str(&self, msg: &'a str) -> Option<Vec<&'a str>> {
//         // let mut pre_msg = msg.to_lowercase().split_whitespace();
//         // match (pre_msg.next(), pre_msg.next()) {
//         //     (Some(), Some()) => {}
//         //     (Some(comm), None) => if comm = "remind" {},
//         // }

//         // None

//         todo!()
//     }

//     async fn run(&self, input: Self::Input) -> Self::Output
//     where
//         'a: 'async_trait,
//     {
//         todo!()
//     }
// }
