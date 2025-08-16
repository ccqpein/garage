use async_trait::async_trait;
use tokio::runtime;

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

// async trait test below
pub trait AppInput {}
pub trait AppOutput {}

impl AppInput for str {}
impl AppInput for String {}
impl AppOutput for Result<(), String> {}

#[async_trait]
pub trait App {
    async fn run(&self, input: &(dyn AppInput + Sync)) -> Result<(), String>;
}

#[async_trait]
impl App for A {
    async fn run(&self, input: &(dyn AppInput + Sync)) -> Result<(), String> {
        Ok(())
    }
}

struct B;

#[async_trait]
pub trait Bpp {
    async fn run(&self, input: &(dyn AppInput + Sync)) -> Box<dyn AppOutput + Sync>;
}

#[async_trait]
impl Bpp for B {
    async fn run(&self, input: &(dyn AppInput + Sync)) -> Box<dyn AppOutput + Sync> {
        println!("yoyoyo");
        Box::new(Ok(()))
    }
}

fn main() {
    let a = A {};
    println!("{}", a.aa());
    // same name function but inside trait
    println!("{}", SomeT::aa(&a));

    // when there isn't &self in function arguments
    println!("{}", A::bb(1));

    // async trait test
    let rt = runtime::Builder::new_multi_thread()
        .enable_time()
        .enable_io()
        .build()
        .unwrap();

    rt.block_on(async {
        let b = B;
        b.run(&"hello".to_string()).await;
    });
}
