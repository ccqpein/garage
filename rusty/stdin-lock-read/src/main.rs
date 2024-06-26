use std::io;

fn main() {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        // truly block
        Ok(n) => {
            println!("{n} bytes read");
            println!("{input}");
        }
        Err(error) => println!("error: {error}"),
    }
}
