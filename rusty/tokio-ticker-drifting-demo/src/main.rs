use std::time::{Duration, SystemTime};
use tokio::{
    select,
    time::{Instant, interval, sleep},
};

/// tokio time instant to nano number
async fn time_to_nano(i: Instant) -> u128 {
    let now = Instant::now();
    let system_now = SystemTime::now();

    let target_system_time = if i > now {
        system_now + (i - now)
    } else {
        system_now - (now - i)
    };

    target_system_time
        .duration_since(SystemTime::UNIX_EPOCH)
        .expect("Time went backwards")
        .as_nanos()
}

#[tokio::main]
async fn main() {
    // let now = Instant::now();
    // println!("Current time in nanos: {}", time_to_nano(now).await)
    //let mut xx = Instant::now();
    //let mut interval = tokio::time::interval(Duration::from_millis(1000));

    // let start_time = Instant::now();
    // let mut ideal_next_tick = start_time;
    // let mut interval = interval(Duration::from_millis(1000));

    // loop {
    //     select! {
    //         tt = interval.tick() => {
    //             //let a = tt.duration_since(ideal_next_tick);

    //             let drift = if tt > ideal_next_tick {
    //                 tt.duration_since(ideal_next_tick)
    //             } else {
    //                 ideal_next_tick.duration_since(tt)
    //             };

    //             println!("Tick at: {:?}, Drift: {} ns", tt, drift.as_nanos());

    //             // if a.as_millis() > 1000 {
    //             //     println!("get one");
    //             //     return
    //             // }else {
    //             //     println!("tt is {:?}, a is {:?}",tt,a);
    //             //     ideal_next_tick += Duration::from_millis(1000);
    //             // }
    //             ideal_next_tick += Duration::from_millis(1000);
    //         }
    //     }
    // }

    let start_time = Instant::now();
    let mut interval = interval(Duration::from_millis(1000));

    // 1. We skip the first tick because it's always "0" drift
    interval.tick().await;
    println!("Ticker started...");

    for i in 1..=5 {
        // 2. Simulate the program doing some "work" that takes 200ms
        // This should not affect the ticker because 200ms < 1000ms
        sleep(Duration::from_millis(200)).await;

        let tt = interval.tick().await;

        // Calculate drift from the start_time + (i * 1 second)
        let ideal_time = start_time + Duration::from_secs(i);

        let drift = if tt > ideal_time {
            tt.duration_since(ideal_time)
        } else {
            ideal_time.duration_since(tt)
        };

        println!("Tick #{} | Drift: {} ns", i, drift.as_nanos());
    }
}
