use std::any::Any;

use cl_format_test::{v2_test, TildeAble2, TildeKind};

fn any_test(a: Box<dyn Any>) {}

fn main() {
    let a = 'a';
    let mut buf = String::new();
    v2_test(a, &TildeKind::Char, &mut buf);
}
