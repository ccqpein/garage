use super::*;
use async_trait::async_trait;
use std::any::Any;
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

#[async_trait]
pub trait App {
    //type Input: AppInput;
    // async fn run<'m, 's: 'm>(
    //     &'s mut self,
    //     input: Box<dyn AppInput + Send + 'm>,
    // ) -> Result<(), String>;

    //fn sender(&self) -> Sender<Self::Input>;
    async fn consume(&mut self, msg: &Message) -> Option<()>;
}

pub struct AppLayer {
    rev: Receiver<Message>,
    app_queue: Vec<Box<dyn App + 'static>>,
}

impl AppLayer {
    pub fn new() -> (Self, Sender<Message>) {
        let (snd, rev) = mpsc::channel(10);
        (
            Self {
                app_queue: vec![],
                rev,
            },
            snd,
        )
    }

    pub fn register_app(&mut self, a: impl App + Send + 'static) {
        self.app_queue.push(Box::new(a));
    }

    pub async fn consume_msg(&mut self, msg: Message) -> Result<(), String> {
        for a in &mut self.app_queue {
            if let Some(_) = a.consume(&msg).await {
                return Ok(());
            }
        }
        Err("there are no app can consume this message".into())
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
