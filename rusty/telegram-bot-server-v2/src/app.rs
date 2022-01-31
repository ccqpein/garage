use super::*;
use async_trait::async_trait;
use std::any::Any;
use std::fmt::Debug;
use telegram_bot::Message;
use tokio::sync::mpsc::{self, Receiver, Sender};
use tracing::{debug, error, info};

mod echo;
pub use echo::*;

//mod github_commit_check;
//pub use github_commit_check::*;

// trait AppInput {
//     /// no status
//     fn parse_input(input: &Message) -> Option<Self>
//     where
//         Self: Sized;
//     //fn into_any(self: Box<Self>) -> Box<dyn Any>;
// }

// pub trait AppInput: Debug {}

// pub trait AppInputBuilder {
//     //type AppInput;
//     fn parse_input<'a>(&self, msg: &'a Message) -> Option<Box<dyn AppInput>>;
// }

pub enum ConsumeStatus {
    Taken,
    NotMine,
}

pub trait App {
    type Consumer: AppConsumer;
    fn consumer(&self) -> Self::Consumer;
    //fn run(self)
}

#[async_trait]
pub trait AppConsumer: Send + Sync {
    async fn consume_msg<'a>(&mut self, msg: &'a Message) -> Result<ConsumeStatus, String>;
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

    pub fn register_app<A, C>(&mut self, a: A)
    where
        C: AppConsumer + 'static,
        A: App<Consumer = C> + 'static,
    {
        self.app_queue.push(box a.consumer());
        tokio::spawn(async { a.run().await })
    }

    pub async fn consume_msg(&mut self, msg: Message) -> Result<(), String> {
        for ap in &mut self.app_queue {
            match ap.as_mut().consume_msg(&msg).await {
                Ok(_) => todo!(),
                Err(_) => todo!(),
            }
        }
        Err("no app can consume this msg".into())
    }

    pub async fn run(mut self) {
        info!("applayer is running");
        while let Some(msg) = self.rev.recv().await {
            info!("applayer receive message: {:?}", msg);
            match self.consume_msg(msg).await {
                Ok(_) => continue,
                Err(e) => error!("error: {}", e),
            }
        }
    }
}
