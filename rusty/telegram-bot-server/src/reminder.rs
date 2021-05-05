use std::{collections::HashMap, future::Future};

use telegram_bot::ChatId;

use tokio::sync::mpsc::{Receiver, Sender};
use tokio::sync::oneshot;
use tokio::time::{sleep, Duration};
use tracing::{debug, info};

use crate::deliver::Msg2Deliver;

pub enum ReminderComm {
    New,
    Cancel,
}

pub struct Msg2Reminder {
    comm: ReminderComm,

    reminder_id_pair: (ChatId, String),

    content: String,

    time_duration: Duration,
}

impl Msg2Reminder {
    pub fn new(
        comm: ReminderComm,
        reminder_id_pair: (ChatId, String),
        content: String,
        duration: Duration,
    ) -> Self {
        Self {
            comm,
            reminder_id_pair,
            content,
            time_duration: duration,
        }
    }
}

struct Msg2Timer;

pub struct Reminder {
    rec: Receiver<Msg2Reminder>,

    to_deliver: Sender<Msg2Deliver>,

    to_timer: HashMap<(ChatId, String), oneshot::Sender<Msg2Timer>>,
}

impl Reminder {
    pub fn new(rec: Receiver<Msg2Reminder>, to_deliver: Sender<Msg2Deliver>) -> Self {
        Self {
            rec,
            to_deliver,
            to_timer: HashMap::new(),
        }
    }

    pub async fn run(&mut self) {
        info!("Reminder is running");
        while let Some(msg) = self.rec.recv().await {
            match (
                msg.comm,
                msg.reminder_id_pair,
                msg.content,
                msg.time_duration,
            ) {
                (ReminderComm::New, id_pair, content, dur) => {
                    tokio::spawn(self.make_new_timer(id_pair, content, dur));
                }
                (ReminderComm::Cancel, id_pair, ..) => {
                    if let Some(ch) = self.to_timer.remove(&id_pair) {
                        let _ = ch.send(Msg2Timer);
                    } else {
                        self.to_deliver
                            .send(Msg2Deliver::new(
                                "send".to_string(),
                                id_pair.0,
                                format!("Cannot found {} reminder", id_pair.1),
                            ))
                            .await;
                    }
                }
            }
        }
    }

    fn make_new_timer(
        &mut self,
        id_pair: (ChatId, String),
        content: String,
        duration: Duration,
    ) -> impl Future<Output = Result<(), String>> {
        // make channel
        let (snd, mut rec) = oneshot::channel();

        self.to_timer.insert(id_pair.clone(), snd);

        let to_deliver = self.to_deliver.clone();

        async move {
            to_deliver
                .send(Msg2Deliver::new(
                    String::from("send"),
                    id_pair.0,
                    String::from(format!(
                        "New reminder {} made, remind you every {} minutes",
                        id_pair.1,
                        duration.as_secs() / 60
                    )),
                ))
                .await;

            loop {
                match rec.try_recv() {
                    Ok(_) => break,
                    _ => (),
                }
                sleep(duration).await;
                match to_deliver
                    .send(Msg2Deliver::new(
                        String::from("send"),
                        id_pair.0,
                        content.clone(),
                    ))
                    .await
                {
                    Ok(_) => {}
                    Err(e) => {
                        debug!("Error {} happens in {:?}", e.to_string(), id_pair);
                    }
                }
            }

            to_deliver
                .send(Msg2Deliver::new(
                    String::from("send"),
                    id_pair.0,
                    String::from(format!("Reminder {} cancelled", id_pair.1)),
                ))
                .await;
            info!("Remind {:?} cancelled", id_pair);
            Ok(())
        }
    }
}
