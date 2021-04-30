use std::{collections::HashMap, future::Future};

use telegram_bot::{ChatId, MessageId};

use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::oneshot;

use crate::deliver::Msg2Deliver;

pub enum ReminderComm {
    New,
    Cancel,
}

pub struct Msg2Reminder {
    comm: ReminderComm,

    reminder_id_pair: (ChatId, MessageId),

    content: String,
}

impl Msg2Reminder {
    pub fn new(comm: ReminderComm, reminder_id_pair: (ChatId, MessageId), content: String) -> Self {
        Self {
            comm,
            reminder_id_pair,
            content,
        }
    }
}

struct Msg2Timer;

struct Reminder {
    rec: Receiver<Msg2Reminder>,

    to_deliver: Sender<Msg2Deliver>,

    to_timer: HashMap<(ChatId, MessageId), oneshot::Sender<Msg2Timer>>,
}

impl Reminder {
    fn new(rec: Receiver<Msg2Reminder>, to_deliver: Sender<Msg2Deliver>) -> Self {
        Self {
            rec,
            to_deliver,
            to_timer: HashMap::new(),
        }
    }

    async fn run(&mut self) {
        while let Some(msg) = self.rec.recv().await {
            match (msg.comm, msg.reminder_id_pair, msg.content) {
                (ReminderComm::New, id_pair, content) => {
                    tokio::spawn(self.make_new_timer(id_pair, content));
                }
                (ReminderComm::Cancel, id_pair, ..) => {
                    if let Some(ch) = self.to_timer.remove(&id_pair) {
                        ch.send(Msg2Timer); //:= here
                    }
                }
                _ => {}
            }
        }
    }

    fn make_new_timer(
        &mut self,
        id_pair: (ChatId, MessageId),
        content: String,
    ) -> impl Future<Output = Result<(), String>> {
        // make channel
        let (snd, mut rec) = oneshot::channel();

        self.to_timer.insert(id_pair, snd);

        let to_deliver = self.to_deliver.clone();

        async move {
            loop {
                match rec.try_recv() {
                    Ok(_) => break,
                    _ => (),
                }
                tokio::time::sleep(tokio::time::Duration::from_secs(10)); //:= test here
                to_deliver.send(Msg2Deliver::new(String::new(), id_pair.0, String::new()));
            }
            Ok(())
        }
    }
}
