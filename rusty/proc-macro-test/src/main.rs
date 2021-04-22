use proc_macro_test::*;

#[derive(DeriveTestYo)]
struct A;

#[derive(GiveMeFields, Debug)]
struct B {
    #[this]
    a: String,

    #[this]
    b: i64,
}

fn main() {
    //println!("{}", A::hello_macros());
    println!("{:?}", B::give_me_fields());
}
