use core::time;
use std::pin::Pin;

use async_timer::timer::{Timer, new_timer};
use tokio::select;

async fn test1() {
    let mut timer = new_timer(time::Duration::from_secs(2));
    assert!(!timer.is_expired());
    //timer.await;
    Pin::new(&mut timer).await;
    assert!(!timer.is_expired());
}

async fn test2() {
    let mut timer = new_timer(time::Duration::from_secs(2));
    select! {
        _ = timer => {
            println!("hello")
        }
        else => panic!()
    }
}

#[tokio::main]
async fn main() {
    //test1().await;
    test2().await;
}
