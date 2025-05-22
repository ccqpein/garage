use crate::expression::*;

pub fn add(ctx: &mut Context, v: Value) -> Value {
    dbg!("hello");

    Value {
        ty: Types::Number(2),
    }
}
