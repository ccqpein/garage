#![feature(associated_type_defaults)]

use std::any::Any;
enum Enum {
    E1,
    E2,
}

trait En: Any {
    //fn downcast(&self) ->
}

trait A1: En {
    fn check_a1(&self) -> bool {
        true
    }
}
trait A2: En {
    fn check_a2(&self) -> bool {
        true
    }
}

impl En for char {}
impl A1 for char {}
impl A2 for char {}

struct A1S<T: A1> {
    inner: T,
}

fn entry1(a: Box<dyn Any>) {
    let cc = a.downcast::<char>().unwrap();
    dbg!(cc.check_a1());
}

fn entry2(a: Box<(dyn En + 'static)>) {
    // let cc = a.downcast::<char>().unwrap();
    // dbg!(cc.check_a1());
}

fn main() {
    entry1(Box::new('v'));
}
