use std::{fmt::Display, ops::Add};

#[derive(Debug)]
struct WeirdOutput<W> {
    v: W,
}

// O: <Self as Add>::Output`, indicates that I am trying to use a concrete type (`<Self as Add>::Output`) as a trait bound, which is not allowed
// trait WeirdAdd: Add + Sized {
//     fn wadd<O>(self, rhs: Self) -> WeirdOutput<O>
//     where
//         O: <Self as Add>::Output,
//     {
//         WeirdOutput { v: self + rhs }
//     }
// }

trait WeirdAdd: Add + Sized {
    fn wadd(self, rhs: Self) -> WeirdOutput<<Self as Add>::Output> {
        WeirdOutput { v: self + rhs }
    }
}

impl WeirdAdd for i32 {}

fn main() {
    dbg!(1.add(1));

    dbg!(1_i32.wadd(1)); // this one need the empty impl 
}
