use async_trait::async_trait;
use lazy_static::*;
use std::{collections::HashMap, sync::Arc};
use telegram_bot::Message;

mod github_commit_check;

trait AppInput {
    /// no status
    fn parse_input(input: Message) -> Option<Self>
    where
        Self: Sized;
}

impl<A> AppInput for A
where
    A: TryFrom<Message>,
{
    fn parse_input(input: Message) -> Option<Self>
    where
        Self: Sized,
    {
        Self::try_from(input).ok()
    }
}

#[async_trait]
trait App {
    type Input: AppInput;
    async fn run(&mut self, input: Self::Input) -> Result<(), String>;
}
