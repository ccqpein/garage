use std::io::{stdout, Result, Write};
use std::{thread, time};

fn test() -> Result<()> {
    let msgs = vec!["hello", "I am ", "robot"];
    let mut std_out = stdout();
    loop {
        for msg in msgs.iter() {
            std_out.write_all(msg.as_bytes())?;
            std_out.flush()?;
            thread::sleep(time::Duration::from_millis(500));
            print!("\n{}\x1b[1A", msg); // https://notes.burke.libbey.me/ansi-escape-codes/
            print!("\r");
            std_out.flush()?;
            thread::sleep(time::Duration::from_millis(500));
        }
    }
}

fn main() {
    test().unwrap()
}
