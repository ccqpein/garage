use super::*;
use async_trait::async_trait;
use lazy_static::*;
use std::{collections::HashMap, sync::Arc};
use telegram_bot::Message;

mod github_commit_check;

trait AppInput {
    /// no status
    fn parse_input(input: &Message) -> Result<Option<Self>, String>
    where
        Self: Sized;
}

#[async_trait]
trait App {
    type Input: AppInput;
    async fn run(&mut self, input: Self::Input) -> Result<(), String>;
}
