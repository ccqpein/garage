trait Test {
    fn a() -> String {
        String::from("default")
    }
}

struct A;
struct B;

impl Test for A {
    fn a() -> String {
        String::from("in a")
    }
}

impl Test for B {}

fn main() {
    dbg!(<A as Test>::a());
    dbg!(<B as Test>::a());
}
