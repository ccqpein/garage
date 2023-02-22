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

//////////////////////////
//////////////////////////
//////////////////////////

trait A {
    fn into_e1(&self) -> bool;
    fn into_e2(&self) -> bool;
}

impl A for char {
    fn into_e1(&self) -> bool {
        true
    }
    fn into_e2(&self) -> bool {
        true
    }
}

impl A for i32 {
    fn into_e1(&self) -> bool {
        true
    }
    fn into_e2(&self) -> bool {
        false
    }
}

fn entry2(a: &Vec<&dyn A>) {
    for aa in a {
        dbg!(aa.into_e1());
        dbg!(aa.into_e2());
    }
}

fn main() {
    entry1(Box::new('v'));

    entry2(&vec![&'v', &32]);
}
