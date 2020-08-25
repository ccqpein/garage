use ::Cons::*;
use std::rc::Rc;

fn main() {
    // make ((1 2) . (3 4)
    let a = Value::Integer(Rc::new(1));
    let b = Value::Integer(Rc::new(2));
    let c = Value::Integer(Rc::new(3));
    let d = Value::Integer(Rc::new(4));

    let result = cons(cons(a, cons(b, Value::Nil)), cons(c, cons(d, Value::Nil)));
    dbg!(result);
}
