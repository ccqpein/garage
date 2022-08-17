use super::*;
use chrono::{Timelike, Utc};
use chrono_tz::America::New_York;
use telegram_bot::ChatId;
use tokio::sync::Mutex;
use tracing::debug;

pub static MY_CHATID: Mutex<Option<ChatId>> = Mutex::const_new(None);

pub struct GithubCommitCheckActive {
    vault_path: String,
    deliver_sender: Sender<Msg2Deliver>,
}

impl GithubCommitCheckActive {
    pub fn new(deliver_sender: Sender<Msg2Deliver>, vault_path: String) -> Self {
        Self {
            deliver_sender,
            vault_path,
        }
    }

    async fn check(self) -> Result<(), String> {
        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;

            let saved_id = MY_CHATID.lock().await;
            if let Some(chatid) = *saved_id {
                info!("find chatid in record, make daily checker");
                let vault = self.vault_path.to_string();
                tokio::spawn(async move {
                    loop {
                        // wait
                        wait_until().await;

                        let reply = match does_this_user_have_commit_today(
                            "ccqpein",
                            Some(vault.clone() + "/githubtoken"),
                        )
                        .await
                        {
                            Ok(true) => {
                                format!("You have commit today")
                            }
                            Ok(false) => {
                                format!("You haven't commit today yet")
                            }
                            Err(e) => format!("check error {}", e.to_string()),
                        };

                        match self
                            .deliver_sender
                            .send(Msg2Deliver::new("send".to_string(), chatid, reply))
                            .await
                        {
                            Ok(_) => (),
                            Err(e) => debug!("error in sending reminder {}", e),
                        };

                        // wait until next minute
                        tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
                    }
                });
                break;
            }
        }
        Ok(())
    }
}

async fn wait_until() {
    loop {
        let now_utc = Utc::now();
        let now_eastern = now_utc.with_timezone(&New_York);

        let (h, m) = (now_eastern.hour(), now_eastern.minute());

        if h == 22 && m == 40 {
            info!("timeup, break waiting");
            break;
        }

        // check until next minute
        tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
    }
}

#[async_trait]
impl App for GithubCommitCheckActive {
    type Consumer = ();

    fn consumer(&self) -> Self::Consumer {
        ()
    }

    async fn run(mut self) -> Result<(), String> {
        self.check().await
    }
}
