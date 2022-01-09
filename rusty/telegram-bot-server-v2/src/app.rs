use super::*;
use async_trait::async_trait;
use std::any::Any;
use telegram_bot::Message;
use tokio::sync::mpsc::Sender;
use tracing::debug;

mod echo;
use echo::*;

//mod github_commit_check;
//use github_commit_check::*;

trait AppInput<'m>: Any {
    /// no status
    fn parse_input(input: &'m Message) -> Option<Self>
    where
        Self: Sized;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

#[async_trait]
trait App {
    //type Input: AppInput;
    async fn run<'m, 's: 'm>(
        &'s mut self,
        input: Box<dyn AppInput + Send + 'm>,
    ) -> Result<(), String>;
}

// struct AppLayer {
//     //app_queue: Vec<App>,
// }

// impl AppLayer {
//     async fn check_msg<A: App>(msg: &Message, app: &mut A) -> Option<A::Input> {
//         A::Input::parse_input(msg)
//     }

//     async fn run_app<A: App>(input: A::Input, app: &mut A) {
//         match app.run(input).await {
//             Ok(_) => (),
//             Err(e) => debug!("{}", e),
//         }
//     }
// }
