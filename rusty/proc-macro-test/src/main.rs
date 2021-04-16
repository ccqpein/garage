use proc_macro_test::DeriveTestYo;

#[derive(DeriveTestYo)]
struct A;

fn main() {
    println!("{}", A::hello_macros());
}
