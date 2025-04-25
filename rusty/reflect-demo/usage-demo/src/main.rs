use macro_demo::MyDebug;

#[derive(MyDebug)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    println!("{:?}", Point { x: 1, y: 1 })
}
