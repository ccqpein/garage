use lazy_static::lazy_static;
use tokio::sync::Mutex;

lazy_static! {
    static ref A: Mutex<String> = Mutex::new(String::new());
    static ref B: Mutex<String> = Mutex::new(String::new());

    //

    static ref As: std::sync::Mutex<String> = std::sync::Mutex::new(String::new());
    static ref Bs: std::sync::Mutex<String> = std::sync::Mutex::new(String::new());
}

/// no deadlock
async fn work1() {
    *A.lock().await = "work1".to_string();
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    println!("done the work1 sleep");
    *B.lock().await = "work1".to_string();
}

/// no deadlock
async fn work2() {
    *B.lock().await = "work1".to_string();
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    println!("done the work2 sleep");
    *A.lock().await = "work1".to_string();
}

/// deadlock
async fn work1_v1() {
    let mut a = A.lock().await;
    *a = "work1".to_string();
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    println!("done the work1 sleep");
    let mut b = B.lock().await;
    *b = "work1".to_string();
}

/// deadlock
async fn work2_v1() {
    let mut b = B.lock().await;
    *b = "work1".to_string();
    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
    println!("done the work2 sleep");
    let mut a = A.lock().await;
    *a = "work1".to_string();
}

fn work1s() {
    *As.lock().unwrap() = "work1".to_string();
    std::thread::sleep(std::time::Duration::from_secs(1));
    println!("done the work1 sleep");
    *Bs.lock().unwrap() = "work1".to_string();
}

fn work2s() {
    *Bs.lock().unwrap() = "work1".to_string();
    std::thread::sleep(std::time::Duration::from_secs(1));
    println!("done the work2 sleep");
    *As.lock().unwrap() = "work1".to_string();
}

#[tokio::main]
async fn main() {
    let h1 = tokio::spawn(work1());
    let h2 = tokio::spawn(work2());

    h1.await;
    h2.await;

    println!("done the first version");

    let h1 = tokio::spawn(work1_v1());
    let h2 = tokio::spawn(work2_v1());

    h1.await;
    h2.await;

    // done successful means the locker release after await
    // instead of the function
    println!("done");
}

// fn main() {
//     std::thread::spawn(work1s);
//     std::thread::spawn(work2s);

//     println!("done sync version")
// }
