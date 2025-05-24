use std::collections::HashMap;

use crate::parser::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Types {
    Number(i64),
    String(String),
    Func(String),
}

/// the arg of function call
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Value {
    pub ty: Types,
}

type Lmda = fn(&mut Context, Value) -> Value;

/// Expr context when we running expression
pub struct Context {
    pub fnTable: HashMap<String, Lmda>,
}

struct Expr<'a, 'c: 'a> {
    a: &'a Atom,
    c: &'c mut Context,
}

impl<'a, 'c: 'a> Expr<'a, 'c> {
    pub fn eval(&self) {
        match self.a {
            Atom::Sym(sym) => todo!(),
            Atom::List(atoms) => todo!(),
        }
    }

    fn eval_sym(&self, s: &Sym) {
        match s.vt {
            ValueType::Number(_a) => todo!(),
            ValueType::String(_) => todo!(),
            ValueType::Symbol(_) => todo!(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::functions;

    use super::*;

    #[test]
    fn test_context() {
        let mut ctx = Context {
            fnTable: HashMap::new(),
        };

        ctx.fnTable.insert("add".to_string(), functions::math::add);

        dbg!(ctx.fnTable.get("add").unwrap()(
            &mut ctx,
            Value {
                ty: Types::Number(1),
            },
        ));

        ctx.fnTable.insert("mul".to_string(), functions::math::mul);

        dbg!(ctx.fnTable.get("mul").unwrap()(
            &mut ctx,
            Value {
                ty: Types::Number(1),
            },
        ));
    }
}
