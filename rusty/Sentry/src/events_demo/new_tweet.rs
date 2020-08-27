use crate::event::*;

struct NewTweet {
    id: String,
    time: u64,
    content: String,
}

impl HappenEvent for NewTweet {}
