use std::{collections::HashMap, future::Future};

use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::oneshot;

use crate::watcher::Msg2Deliver;

enum ReminderComm {
    New,
}

struct Msg2Reminder {
    comm: ReminderComm,

    reminder_id: usize,

    content: String,
}

struct Msg2Timer {}

struct Reminder {
    rec: Receiver<Msg2Reminder>,

    to_deliver: Sender<Msg2Deliver>,

    to_timer: HashMap<usize, oneshot::Sender<Msg2Timer>>,
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
            match (msg.comm, msg.reminder_id, msg.content) {
                (ReminderComm::New, id, content) => {
                    tokio::spawn(self.make_new_timer(id, content));
                }
                _ => {}
            }
        }
    }

    fn make_new_timer(
        &mut self,
        id: usize,
        content: String,
    ) -> impl Future<Output = Result<(), String>> {
        let (snd, rec) = oneshot::channel();
        //:= need check duplications
        self.to_timer.insert(id, snd);
        let to_deliver = self.to_deliver.clone();
        async move {
            to_deliver;
            rec;
            Ok(())
        }
    }
}
