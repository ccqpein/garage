use tokio::{select, sync::mpsc};

#[tokio::main]
async fn main() {
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
            sender3.send(i as i32).await;
        }
    });

    // infinity loop
    // loop {
    //     tokio::select! {
    //         a = receiver1.recv() => {
    //             match a {
    //                 Some(a) => println!("1: {:?}",a),
    //                 None => receiver1.close(),
    //             }
    //         }
    //         b = receiver2.recv() => {
    //             match b {
    //                 Some(b) => println!("2: {:?}",b),
    //                 None => receiver2.close(),
    //             }

    //         }
    //         c = receiver3.recv() => {
    //             match c {
    //                 Some(c) => println!("3: {:?}",c),
    //                 None => receiver3.close(),
    //             }
    //         }
    //         else =>break,
    //     }
    // }

    loop {
        println!("in loop");
        tokio::select! {
            Some(a) = receiver1.recv() => {
                 println!("1: {:?}",a)
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
