use super::*;

pub struct GithubCommitCheckActive {
    deliver_sender: Sender<Msg2Deliver>,
}

impl GithubCommitCheckActive {
    async fn check() -> Result<(), String> {
        let f =
            BufReader::new(File::open(vault.to_string() + "/myname").map_err(|e| e.to_string())?);

        let myname = f
            .lines()
            .next()
            .ok_or("Read 'myname' failed".to_string())?
            .map_err(|e| e.to_string())?;

        let reply = if does_this_user_have_commit_today(
            "ccqpein",
            Some(vault.to_string() + "/githubtoken"),
        )
        .await?
        {
            format!("You have commit today")
        } else {
            format!("You haven't commit today yet")
        };

        match self
            .deliver_sender
            .send(Msg2Deliver::new("send".to_string(), chatid, reply))
            .await
        {
            Ok(_) => Ok(()),
            Err(e) => Err(e.to_string()),
        }
    }
}

#[async_trait]
impl App for GithubCommitCheckActive {
    type Consumer = ();

    fn consumer(&self) -> Self::Consumer {
        ()
    }

    async fn run(mut self) -> Result<(), String> {
        todo!()
    }
}
