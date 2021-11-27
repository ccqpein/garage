use std::collections::HashMap;

use async_trait::async_trait;
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot,
};

use tracing::{debug, info};

use crate::deliver::Msg2Deliver;

use super::{App, AppInput, Register};

enum ReminderStatus {
    /// make new reminder
    Reminder(u64), // time of reminder
    ReminderPending,

    /// cancel the reminder
    CancelReminder(String),
    CancelReminderPending,
}

// impl TryFrom<&[String]> for ReminderStatus {
//     type Error = String;

//     fn try_from(msgs: &[String]) -> Result<Self, Self::Error> {
//         match msgs {
//             ["reminder"] => Ok(Self::Reminder(30)),
//             ["reminder", t] => match t.parse::<u64>() {
//                 Ok(u) => Ok(Self::Reminder(u)),
//                 Err(e) => Err(e.to_string()),
//             },
//             ["cancelreminder"] => Ok(Self::CancelReminderPending),
//             ["cancelreminder", msgid] => Ok(Self::CancelReminder(msgid)),
//             _ => Err("pattern match has issue in reminder try from".to_string()),
//         }
//     }
// }

struct ReminderInput {
    chat_id: ChatId,
    msg: Vec<String>,
}

impl ReminderInput {
    fn new() -> Self {
        Self {
            chat_id: ChatId::new(0),
            msg: vec![],
        }
    }
}

impl AppInput for ReminderInput {
    fn from_msg(&mut self, m: &Message) -> Result<(), String> {
        match (&m.kind, &m.chat) {
            (MessageKind::Text { ref data, .. }, cc @ MessageChat::Private(_)) => {
                let data = data
                    .to_lowercase()
                    .split_whitespace()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>();
                match &data[..] {
                    [comm, _time] => {
                        if comm == "reminder" {
                            self.msg = data;
                            self.chat_id = cc.id();
                        } else {
                            return Err("ReminderInput maker failed".to_string());
                        }
                    }
                    [comm] => {
                        if comm == "reminder" {
                            self.msg = data;
                            self.chat_id = cc.id();
                        } else {
                            return Err("ReminderInput maker failed".to_string());
                        }
                    }
                    _ => return Err("ReminderInput maker failed".to_string()),
                }
            }
            _ => {
                debug!("message pattern matching failed");
                return Err("ReminderInput maker failed".to_string());
            }
        }
        Ok(())
    }
}

struct Reminder {
    // message_rec: Receiver<Vec<String>>,
    // message_snd: Sender<Vec<String>>,
    message_rec: Receiver<Box<dyn AppInput>>,
    message_snd: Sender<Box<dyn AppInput>>,
    status_table: HashMap<ChatId, (ReminderStatus, oneshot::Sender<bool>)>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl Reminder {
    fn new(deliver_sender: Sender<Msg2Deliver>) -> Self {
        let (snd, rev) = mpsc::channel(10);
        Self {
            message_rec: rev,
            message_snd: snd,
            status_table: HashMap::new(),
            deliver_sender,
        }
    }

    /// check this chat window's status
    pub fn status_checker() -> Option<ReminderStatus> {
        todo!()
    }

    pub async fn run(&mut self) {
        while let Some(msg) = self.message_rec.recv().await {
            todo!()
            // match ReminderStatus::try_from(msg) {
            //     Ok(_) => todo!(),
            //     Err(e) => {
            // self.deliver_sender.send(

            //     Msg2Deliver::new("send", chatid, msg)
            // )
        }
    }
}

impl Register for Reminder {
    fn message_match(&self) -> Box<dyn Fn(&telegram_bot::Message) -> Option<Box<dyn AppInput>>>
    where
        Self: Sized,
    {
        box |m: &telegram_bot::Message| -> Option<Box<dyn AppInput>> {
            let mut input = ReminderInput::new();
            match input.from_msg(m) {
                Ok(_) => Some(box input),
                Err(e) => {
                    debug!("error {} happen", e.to_string());
                    None
                }
            }
        }
    }
}

impl App for Reminder {
    fn name(&self) -> String {
        "reminder".into()
    }

    fn sender(&self) -> Sender<Box<dyn AppInput>> {
        self.message_snd.clone()
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
