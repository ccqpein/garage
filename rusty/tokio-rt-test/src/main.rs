use std::future::Future;
use std::thread;

use tokio::runtime;
use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};

struct MyFuture {}

impl MyFuture {
    async fn run() -> Self {
        sleep(Duration::from_secs(2)).await;
        Self {}
    }
}

impl Future for MyFuture {
    type Output = ();

    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let ww = cx.waker().clone();
        thread::spawn(move || {
            thread::sleep(Duration::from_secs(2));

            ww.wake();
        });

        println!("my future every 2 seconds");
        std::task::Poll::Pending
    }
}

fn main() {
    //let mut rt = tokio::runtime::Runtime::new().unwrap();
    let rt = runtime::Builder::new_multi_thread()
        .enable_time()
        .worker_threads(1) // just one thread
        .build()
        .unwrap();

    // test channel
    let (tx, mut rx) = mpsc::channel(100);

    rt.spawn(async move {
        tx.send("message").await.unwrap();
    });

    // diff than Golang, close on rev
    thread::sleep(Duration::from_secs(1));
    rx.close();

    rt.spawn(async move {
        while let Some(s) = rx.recv().await {
            println!("receive!");
            println!("{}", s)
        }
        println!("end")
    });

    //

    rt.spawn(async {
        // this await for run() function
        let a: MyFuture = MyFuture::run().await;
        println!("make struct");
        // this await for struct MyFuture
        a.await
    });

    rt.spawn(async {
        loop {
            sleep(Duration::from_secs(1)).await;
            println!("one sec");
        }
    });

    rt.block_on(async {
        loop {
            sleep(Duration::from_secs(5)).await;
            println!("hello");
        }
    })
}
