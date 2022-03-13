use super::*;
use async_trait::async_trait;

use std::fmt::Debug;
use telegram_bot::Message;
use tokio::sync::mpsc::{self, Receiver, Sender};
use tracing::{debug, error, info};

mod echo;
pub use echo::*;

mod github_commit_check;
pub use github_commit_check::*;

mod reminder;
pub use reminder::*;

mod status_keeper;
pub use status_keeper::*;

mod github_commit_check_active;
pub use github_commit_check_active::*;

/// App name
pub struct AppName(&'static str);

impl std::fmt::Display for AppName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// consume this message or not
pub enum ConsumeStatus {
    Taken,
    NotMine,
}

/// App async trait
#[async_trait]
pub trait App: Send {
    type Consumer: AppConsumer;
    fn consumer(&self) -> Self::Consumer;
    async fn run(mut self) -> Result<(), String>;
}

#[async_trait]
pub trait AppConsumer: Send + Sync {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String>;
}

impl AppConsumer for () {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String> {
        Ok(ConsumeStatus::NotMine)
    }
}

pub struct AppLayer {
    rev: Receiver<Message>,
    app_queue: Vec<Box<dyn AppConsumer + 'static>>,
}

impl AppLayer {
    pub fn new() -> (Self, Sender<Message>) {
        let (snd, rev) = mpsc::channel(10);
        (
            Self {
                rev,
                app_queue: vec![],
            },
            snd,
        )
    }

    pub fn register_app<A, C>(&mut self, a: &A)
    where
        C: AppConsumer + 'static,
        A: App<Consumer = C> + 'static,
    {
        self.app_queue.push(box a.consumer());
        //tokio::spawn(async { a.run().await });
    }

    async fn consume_msg(&mut self, msg: Message) -> Result<(), String> {
        for ap in &mut self.app_queue {
            match ap.as_mut().consume_msg(&msg).await {
                Ok(ConsumeStatus::NotMine) => continue,
                Ok(ConsumeStatus::Taken) => return Ok(()),
                Err(_) => return Err("AppConsumer consume message wrong".into()),
            }
        }
        Err("no app can consume this msg".into())
    }

    pub async fn run(mut self) {
        info!("applayer is running");
        while let Some(msg) = self.rev.recv().await {
            debug!("applayer receive message: {:?}", msg);
            match self.consume_msg(msg).await {
                Ok(_) => continue,
                Err(e) => error!("error: {}", e),
            }
        }
    }
}
