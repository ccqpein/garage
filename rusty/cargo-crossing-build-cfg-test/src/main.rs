fn main() {
    let s = if cfg!(feature = "test") {
        "test"
    } else {
        "default"
    };

    println!("Hello, {}!", s);
}
