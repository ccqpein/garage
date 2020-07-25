use snake_game::*;
use std::{thread, time};

fn main() {
    let mut a = init_stdout(10, 40);
    back_to_origin(&mut a);
    loop {
        thread::sleep(time::Duration::from_secs(3));
    }
}
