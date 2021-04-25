use tokio::runtime;
use tokio::time::{sleep, Duration};

fn main() {
    //let mut rt = tokio::runtime::Runtime::new().unwrap();
    let rt = runtime::Builder::new_multi_thread()
        .enable_time()
        .worker_threads(1) // just one thread
        .build()
        .unwrap();

    rt.spawn(async {
        loop {
            sleep(Duration::from_secs(1)).await;
            println!("one sec");
        }
    });

    rt.block_on(async {
        loop {
            sleep(Duration::from_secs(5)).await;
            println!("hello")
        }
    })
}
