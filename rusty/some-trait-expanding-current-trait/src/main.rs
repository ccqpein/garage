use std::{fmt::Display, ops::Add};

#[derive(Debug)]
struct WeirdOutput<W> {
    v: W,
}

trait WeirdAdd: Add + Sized {
    fn wadd<O>(self, rhs: Self) -> WeirdOutput<O>
    where
        O: Self::Output,
    {
        WeirdOutput { v: self + rhs }
    }
}

fn main() {
    dbg!(1.add(1));

    dbg!(1_i32.wadd(1));
}
