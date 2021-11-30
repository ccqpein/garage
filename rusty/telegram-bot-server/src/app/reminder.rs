use std::{any::Any, collections::HashMap};

use async_trait::async_trait;
use telegram_bot::{ChatId, Message, MessageChat, MessageKind};
use tokio::sync::{
    mpsc::{self, Receiver, Sender},
    oneshot, Mutex,
};

use tracing::{debug, info};

use crate::deliver::Msg2Deliver;

use super::{App, AppInput, Register};
use lazy_static::*;

lazy_static! {
    static ref REMIND_PENDING_TABLE: Mutex<HashMap<ChatId, (ReminderStatus, oneshot::Sender<bool>)>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

enum ReminderStatus {
    /// make new reminder
    Reminder(u64), // time of reminder
    ReminderPending,

    /// cancel the reminder
    CancelReminder(ChatId),
    CancelReminderPending,
}

impl TryFrom<&ReminderInput> for ReminderStatus {
    type Error = String;

    fn try_from(msgs: &ReminderInput) -> Result<Self, Self::Error> {
        match &msgs.msg.iter().map(|s| s.as_str()).collect::<Vec<_>>()[..] {
            ["reminder"] => Ok(Self::Reminder(30)),
            ["reminder", t] => match t.parse::<u64>() {
                Ok(u) => Ok(Self::Reminder(u)),
                Err(e) => Err(e.to_string()),
            },
            ["cancelreminder"] => Ok(Self::CancelReminderPending),
            ["cancelreminder", msgid] => Ok(Self::CancelReminder(msgs.chat_id)),
            _ => Err("pattern match has issue in reminder try from".to_string()),
        }
    }
}

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

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

struct Reminder {
    message_rec: Receiver<Box<dyn AppInput>>,
    message_snd: Sender<Box<dyn AppInput>>,

    deliver_sender: Sender<Msg2Deliver>,
}

impl Reminder {
    fn new(deliver_sender: Sender<Msg2Deliver>) -> Self {
        let (snd, rev) = mpsc::channel(10);
        Self {
            message_rec: rev,
            message_snd: snd,

            deliver_sender,
        }
    }

    /// check this chat window's status
    pub fn status_checker() -> Option<ReminderStatus> {
        todo!()
    }

    pub async fn run(&mut self) {
        while let Some(msg) = self.message_rec.recv().await {
            // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=288a134fecaa31f1576e74d5f0316812
            let remind_input = msg.into_any();
            if let Ok(input) = remind_input.downcast::<ReminderInput>() {
                let cid = input.chat_id.clone();
                if let Ok(status) = ReminderStatus::try_from(&Box::into_inner(input)) {
                    match status {
                        ReminderStatus::Reminder(_) => todo!(),
                        ReminderStatus::ReminderPending => todo!(),
                        ReminderStatus::CancelReminder(_) => todo!(),
                        ReminderStatus::CancelReminderPending => todo!(),
                    }
                } else {
                    debug!("status parsing failed in reminder");
                    self.deliver_sender
                        .send(Msg2Deliver::new(
                            "send".into(),
                            cid,
                            "inner service problem".into(),
                        ))
                        .await;
                }
            } else {
                debug!("downcast failed in reminder");
            };
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
