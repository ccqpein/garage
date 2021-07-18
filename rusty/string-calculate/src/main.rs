use string_calculate::*;

fn main() {
    let a = QStr::new("123");
    let b = QStr::new("456");

    println!("{}", a + b);
}
