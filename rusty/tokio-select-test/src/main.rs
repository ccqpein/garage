use tokio::{select, sync::mpsc, time};

/// infinity loop because the `a = receiver1.recv()` will always match (Some/None) even the senders are droped
/// else branch will never match. and it is truly infinity loop, will keep consuming cpu
async fn test1() {
    let (sender1, mut receiver1) = mpsc::channel::<i32>(5);
    let (sender2, mut receiver2) = mpsc::channel::<i32>(5);
    let (sender3, mut receiver3) = mpsc::channel::<i32>(5);

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender1.send(i as i32).await;
        }
    });

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender2.send(i as i32).await;
        }
    });
    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender3.send(i as i32).await;
        }
    });

    // infinity loop
    loop {
        tokio::select! {
            a = receiver1.recv() => {
                match a {
                    Some(a) => println!("1: {:?}",a),
                    None => receiver1.close(),
                }
            }
            b = receiver2.recv() => {
                match b {
                    Some(b) => println!("2: {:?}",b),
                    None => receiver2.close(),
                }

            }
            c = receiver3.recv() => {
                match c {
                    Some(c) => println!("3: {:?}",c),
                    None => receiver3.close(),
                }
            }
            else =>break,
        }
    }
}

/// good one, the else branch will match when all other receivers are None (senders dropped)
async fn test2() {
    let (sender1, mut receiver1) = mpsc::channel::<i32>(5);
    let (sender2, mut receiver2) = mpsc::channel::<i32>(5);
    let (sender3, mut receiver3) = mpsc::channel::<i32>(5);

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender1.send(i as i32).await;
        }
    });

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender2.send(i as i32).await;
        }
    });
    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender3.send(i as i32).await;
        }
    });

    // good one: if sender don't send anyting and don't drop, it will sleep in loop await the recv()
    loop {
        tokio::select! {
            Some(a) = receiver1.recv() => {
                println!("1: {:?}",a);
            }
            Some(b) = receiver2.recv() => {
                println!("2: {:?}",b)
            }
            Some(c) = receiver3.recv() => {
                 println!("3: {:?}",c)
            }
            else => break,
        }
    }
}

/// receiver2/3 are closed, else branch will match when sender1 dropped. sender2 and sender3 are still alive, but it doesn't matter anymore.
async fn test3() {
    let (sender1, mut receiver1) = mpsc::channel::<i32>(5);
    let (sender2, mut receiver2) = mpsc::channel::<i32>(5);
    let (sender3, mut receiver3) = mpsc::channel::<i32>(5);

    tokio::spawn(async move {
        for i in 0..5 {
            // sleep need await too
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //
            sender1.send(i as i32).await;
        }
        // sender 1 drop
    });

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //sender2.send(i as i32).await;
        }
    });
    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //sender3.send(i as i32).await;
        }
    });

    loop {
        println!("in loop");
        tokio::select! {
            Some(a) = receiver1.recv() => {
                println!("1: {:?}",a);
                // for test if dont close all receivers here, and sender 2&3 don't send anything
                // it will sleep until new message from 2&3.
                receiver2.close();
                receiver3.close();
            }
            Some(b) = receiver2.recv() => {
                println!("2: {:?}",b)
            }
            Some(c) = receiver3.recv() => {
                 println!("3: {:?}",c)
            }
            else => break,
        }
    }
}

/// infinity await in loop because the sender3 don't have any messages to send.
/// so the receiver3.recv() will sleep/await forever (neither Some or None), else branch won't match.
async fn test4() {
    let (sender1, mut receiver1) = mpsc::channel::<i32>(5);
    let (sender2, mut receiver2) = mpsc::channel::<i32>(5);
    let (sender3, mut receiver3) = mpsc::channel::<i32>(5);

    tokio::spawn(async move {
        for i in 0..5 {
            // sleep need await too
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //
            sender1.send(i as i32).await;
        }
        // sender 1 drop
    });

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender2.send(i as i32).await;
        }
    });
    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //sender3.send(i as i32).await;
        }
    });

    loop {
        println!("in loop");
        tokio::select! {
            Some(a) = receiver1.recv() => {
                println!("1: {:?}",a);
            }
            Some(b) = receiver2.recv() => {
                println!("2: {:?}",b)
            }
            Some(c) = receiver3.recv() => {
                 println!("3: {:?}",c)
            }
            else => break,
        }
    }
}

/// sender3 drop inside sender2 block, so the receiver3.recv() will return None. Fix the infinity await as test4
async fn test5() {
    let (sender1, mut receiver1) = mpsc::channel::<i32>(5);
    let (sender2, mut receiver2) = mpsc::channel::<i32>(5);
    let (sender3, mut receiver3) = mpsc::channel::<i32>(5);

    tokio::spawn(async move {
        for i in 0..5 {
            // sleep need await too
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //
            sender1.send(i as i32).await;
        }
        // sender 1 drop
    });

    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            sender2.send(i as i32).await;
        }
        // drop here, so the receiver3.recv() will return None. break the infinity loop
        drop(sender3);
    });
    tokio::spawn(async move {
        for i in 0..5 {
            tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            //sender3.send(i as i32).await;
        }
    });

    loop {
        println!("in loop");
        tokio::select! {
            Some(a) = receiver1.recv() => {
                println!("1: {:?}",a);
            }
            Some(b) = receiver2.recv() => {
                println!("2: {:?}",b)
            }
            Some(c) = receiver3.recv() => {
                 println!("3: {:?}",c)
            }
            else => break,
        }
    }
}

/// tick test in select!
async fn tick_test() {
    let mut interval = time::interval(time::Duration::from_secs(1));
    loop {
        println!("in loop");
        tokio::select! {
            a = interval.tick() => {
                println!("tick {:?}",a);
            }
            else => break,
        }
    }
}

#[tokio::main]
async fn main() {
    test1().await;
    //test2().await;
    //test3().await;
    //test4().await;
    //test5().await;
    //tick_test().await;
}
