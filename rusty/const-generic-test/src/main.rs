#![feature(const_generics)]
#![feature(const_evaluatable_checked)]

struct SliceN<const N: usize> {
    a: [i32; N],
}

impl<const N: usize> SliceN<N> {
    fn add_one(s: Self) -> SliceN<N> {
        let a = [0; N + 1];
        SliceN { a }
    }
}

fn main() {
    println!("Hello, world!");
}
