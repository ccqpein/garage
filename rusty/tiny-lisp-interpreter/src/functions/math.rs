use crate::expression::*;

pub fn add(ctx: &mut Context, a: &Args) -> Result<Value, ExpressionError> {
    if !a.pre_check_all_types(Value::is_number) {
        return Err(ExpressionError::FuncCallErr);
    }

    match a
        .args
        .into_iter()
        .map(|v| v.into_number())
        .reduce(|acc, n| acc + n)
    {
        Some(v) => Ok(Value::Number(v)),
        None => Err(ExpressionError::FuncCallErr),
    }
}

// pub fn mul(ctx: &mut Context, v: Value) -> Value {
//     dbg!("hello");

//     Value {
//         ty: Types::Number(2),
//     }
// }
