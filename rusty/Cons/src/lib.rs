//use std::sync::Arc;
use std::rc::Rc;

#[derive(Debug)]
pub enum Value {
    Nil,
    Integer(Rc<i64>),
    Cons(Rc<Cons>),
}

#[derive(Debug)]
pub struct Cons {
    car: Value,
    cdr: Value,
}

pub fn cons(a: Value, b: Value) -> Value {
    Value::Cons(Rc::new(Cons { car: a, cdr: b }))
}
