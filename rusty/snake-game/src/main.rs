use snake_game::*;
use std::{thread, time};

fn main() {
    let mut a = Frame::new(10, 20);
    //let mut col = 0;
    //a.write(0, 0, String::from("◼")).unwrap();
    //a.write(0, 1, String::from("◼")).unwrap();
    //a.write(0, 2, String::from("◼")).unwrap();
    for col in 0..11 {
        a.write(col, col, String::from("◼")).unwrap();
        thread::sleep(time::Duration::from_secs(1));
    }

    a.write(0, 0, String::from("a")).unwrap();
}
