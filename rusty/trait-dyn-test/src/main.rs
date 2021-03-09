struct A {}

impl A {
    fn aa(&self) -> u32 {
        0
    }
}

trait SomeT {
    fn aa(&self) -> u32;
}

impl SomeT for A {
    fn aa(&self) -> u32 {
        100
    }
}

fn main() {
    let a = A {};
    println!("{}", a.aa());
    println!("{}", SomeT::aa(&a));
}
