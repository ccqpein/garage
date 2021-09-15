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

/////////////////////////////
trait First {
    // have to add &self inside
    // because rustc --explain E0038
    // fn first() -> i32;
    fn first(&self) -> i32;
    // or like this:
    // fn first() -> i32
    // where
    //     Self: Sized;
}

trait Second {
    fn second(&self) -> i32;
}

impl First for A {
    fn first(&self) -> i32 {
        1
    }
}

impl Second for A {
    fn second(&self) -> i32 {
        2
    }
}

// Cannot write like this because Second isn't the auto trait
// rustc --explain E0225
// fn entrance<T>(x: dyn First + Second) -> i32 {
//     1
// }

fn run_first(a: &dyn First) -> i32 {
    a.first()
}

fn run_both<T>(a: T) -> i32
where
    T: First + Second,
{
    a.first() + a.second()
}

fn main() {
    let a = A {};
    println!("{}", a.aa());
    // same name function but inside trait
    println!("{}", SomeT::aa(&a));

    // when there isn't &self in function arguments
    println!("{}", A::bb(1));
}
