use std::{
    io::{self, BufRead},
    process::Stdio,
};

use tokio::{
    io::{AsyncBufReadExt, AsyncWriteExt, BufReader},
    process::{Child, ChildStdin, ChildStdout, Command},
    time,
};

async fn helper(c: &mut Child) -> (ChildStdin, ChildStdout) {
    (c.stdin.take().unwrap(), c.stdout.take().unwrap())
}

// should run a python and kill it after special time
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut command = Command::new("python3");
    command.arg("-m").arg("revChatGPT");
    command.stdin(Stdio::piped());
    command.stdout(Stdio::piped());

    let mut child = command.spawn().expect("failed to spawn command");

    let (mut stdin, stdout) = helper(&mut child).await;

    tokio::spawn(async move { child.wait().await });

    let mut reader = BufReader::new(stdout).lines();

    tokio::spawn(async move {
        println!("output: \n");
        while let Some(line) = reader.next_line().await.unwrap() {
            println!("{}", line)
        }
    });

    // time::sleep(time::Duration::from_secs(5)).await;

    // stdin.write_all(b"!exit").await?;
    let mut input_reader = io::BufReader::new(io::stdin());
    let mut buffer = vec![];
    while let Ok(_) = input_reader.read_until(b'\n', &mut buffer) {
        stdin.write_all(&buffer).await?;
        stdin.write(b"\n").await?;
        buffer.clear();
    }

    Ok(())
}
