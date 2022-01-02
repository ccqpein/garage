use clap::Clap;
use lazy_static::*;
use std::{collections::HashMap, sync::Arc};
use telegram_bot::Message;
use tokio::sync::{
    mpsc::{Receiver, Sender},
    oneshot, Mutex,
};

mod github_commit_check;

trait App<Input> {
    fn parse_msg(&self, msg: &Message) -> Input;
}

// lazy_static! {
//     static ref APP_MSG_PARSER: Arc<Mutex<HashMap<Box<dyn Fn(&Message) -> Option<()> + Send + Sync>, Sender<String>>>> = {
//         let m = HashMap::new();
//         Arc::new(Mutex::new(m))
//     };
// }

// pub async fn parse_message(msg: &Message) {
//     //let parse_map = APP_MSG_PARSER.lock().await;
//     for (f, ch) in APP_MSG_PARSER.lock().await.iter() {
//         if let Some(_) = f(msg) {}
//     }
// }

/// app layer opts
#[derive(Default, Clap, Clone)]
pub struct Opts {
    #[clap(short, long)]
    pub vault: String,
}
