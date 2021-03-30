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

//////////////////////////////////////////
trait SomeTB {
    fn bb(_: usize) -> usize;
}

impl SomeTB for A {
    fn bb(a: usize) -> usize {
        a
    }
}

fn main() {
    let a = A {};
    println!("{}", a.aa());
    println!("{}", SomeT::aa(&a));

    println!("{}", A::bb(1));
}
