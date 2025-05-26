use crate::expression::*;
pub mod math;

pub fn lambda(ctx: &mut Context, a: &Args) -> Result<Value, ExpressionError> {
    todo!()
}

///:= need to make this,...
///:= should return the Value::Func
pub fn defun(ctx: &mut Context, a: &Args) -> Result<Value, ExpressionError> {
    todo!()
}
