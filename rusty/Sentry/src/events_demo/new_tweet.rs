use crate::event::*;

struct NewTweet {
    id: String,
    time: u64,
    content: String,
}

impl Event for NewTweet {}
