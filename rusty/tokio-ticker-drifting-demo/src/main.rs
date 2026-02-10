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

    let mut interval = interval(Duration::from_millis(1000));

    // First tick happens immediately, so we consume it to "start the clock"
    interval.tick().await;

    println!("Ticker started... (Waiting for the first 1s tick)");

    for i in 1..=5 {
        // 1. Wait for the tick
        let scheduled_time = interval.tick().await;

        // 2. IMMEDIATELY check the real world time
        let actual_time = Instant::now();

        // 3. The "Jitter" is the difference between Reality and the Schedule
        let jitter = actual_time.duration_since(scheduled_time);

        println!(
            "Tick #{}: Scheduled for {:?}, Woke up {:?} late (Jitter)",
            i,
            scheduled_time,    // This stays perfect on the grid
            jitter.as_nanos()  // This is the "shaking" you are looking for
        );
    }
}
