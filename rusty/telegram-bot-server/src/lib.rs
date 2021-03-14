mod app;

use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Update {
    update_id: usize,
    message: Option<Message>,
    //edited_message:Option<String>,
    //channel_post:Option<String>,
    //edited_channel_post:Option<String>,
    //inline_query:
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Message {
    message_id: usize,
    from: Option<User>,
    sender_chat: Option<Chat>,
    date: usize,
    chat: Chat,
    forward_from: Option<User>,
    forward_from_chat: Option<Chat>,
    forward_from_message_id: Option<usize>,
    forward_signature: Option<String>,
    forward_sender_name: Option<String>,
    forward_date: Option<usize>,
    //reply_to_message: Rc<Self>,
    //:= TODO: not finish all
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct User {}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Chat {}
