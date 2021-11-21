use super::App;

struct Reminder {}

impl App for Reminder {
    type Input;

    type Output;

    fn match_str(&self, msg: &'a str) -> Option<Vec<&'a str>> {
        let mut pre_msg = msg.to_lowercase().split_whitespace();
        match (pre_msg.next(), pre_msg.next()) {
            (Some(), Some()) => {}
            (Some(comm), None) => if comm = "remind" {},
        }

        None
    }

    async fn run(&self, input: Self::Input) -> Self::Output
    where
        'a: 'async_trait,
    {
        todo!()
    }
}
