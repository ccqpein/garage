use std::collections::HashMap;

use super::*;
use lazy_static::*;
use telegram_bot::ChatId;
use tokio::sync::{oneshot, Mutex};

lazy_static! {
    static ref CHAT_STATUS_TABLE: Mutex<HashMap<ChatId, ChatStatus>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

#[derive(Clone)]
pub enum ChatStatus {
    None,
    ReminderApp(ReminderStatus),
}

pub enum Operate {
    Update,
    Query,
    Delete,
}

// The input for status checker use for recording the status
pub struct StatusCheckerInput {
    /// this chatid
    chat_id: ChatId,
    /// the status want to update
    update_status: ChatStatus,
    /// operation
    ops: Operate,
}

impl StatusCheckerInput {
    pub fn new(chat_id: ChatId, update_status: ChatStatus, ops: Operate) -> Self {
        Self {
            chat_id,
            update_status,
            ops,
        }
    }
}

struct StatusChecker {
    sender: Sender<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
    receiver: Receiver<(StatusCheckerInput, Option<oneshot::Sender<ChatStatus>>)>,
}

impl StatusChecker {
    pub async fn run(&mut self) {
        while let Some((check_input, send_back)) = self.receiver.recv().await {
            match check_input.ops {
                //:= Update and Delete can return msg to send_back if up stream support
                Operate::Update => {
                    let mut tb = CHAT_STATUS_TABLE.lock().await;
                    let en = tb.entry(check_input.chat_id).or_insert(ChatStatus::None);
                    *en = check_input.update_status;
                }
                Operate::Query => {
                    let send_back_result = if let Some(record) =
                        CHAT_STATUS_TABLE.lock().await.get(&check_input.chat_id)
                    {
                        record.clone()
                    } else {
                        ChatStatus::None
                    };

                    if let Some(snd) = send_back {
                        match snd.send(send_back_result.clone()) {
                            //:= fix todo!()
                            Ok(_) => todo!(),
                            Err(_) => todo!(),
                        }
                    } else {
                        //:= TODO: finish this
                    }
                }
                Operate::Delete => {
                    //:= maybe check this option
                    CHAT_STATUS_TABLE.lock().await.remove(&check_input.chat_id);
                }
            }
        }
    }
}
