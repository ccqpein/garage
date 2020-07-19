use std::io::{stdout, Result, Write};
use std::{thread, time};

/// Carriage_return(\r or 0xD): To take control at starting of same line.
///
/// Line_Feed(\n or 0xA): To Take control at starting of next line.
///
/// form_feed(\f or 0xC): To take control at starting of next page.
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
