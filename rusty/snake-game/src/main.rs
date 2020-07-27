use snake_game::*;
use std::io::Result;
use std::{thread::sleep, time::Duration};

fn main() -> Result<()> {
    let mut a = Frame::new(10, 20);
    //let mut col = 0;
    //a.write(0, 0, String::from("◼")).unwrap();
    //a.write(0, 1, String::from("◼")).unwrap();
    //a.write(0, 2, String::from("◼")).unwrap();
    // for col in 0..11 {
    //     a.write(col, col, String::from("◼")).unwrap();
    //     thread::sleep(time::Duration::from_secs(1));
    // }

    //a.write(0, 0, String::from("a")).unwrap();

    let mut snake = Snake::new((5, 5), Direction::Right, 2);
    println!("{:?}", snake);
    loop {
        snake.show(&mut a).unwrap();
        sleep(Duration::from_secs(1));
        match snake.move_one_step()? {
            Some(tt) => a.write(tt.0, tt.1, String::from("."))?,
            None => (),
        };
    }
}
