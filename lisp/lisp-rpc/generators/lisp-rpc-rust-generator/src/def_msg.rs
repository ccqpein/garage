struct DefMsg {
    msg_name: String,
}

impl DefMsg {
    /// make new def msg
    fn new() -> Self {
        todo!()
    }

    /// all keys of this def msg
    fn keys(&self) -> impl Iterator<Item = (&str, &DefMsg)> {
        todo!()
    }

    fn gen_code(&self) {}
}
