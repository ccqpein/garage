use std::collections::HashMap;

use crate::parser::*;

enum Types {
    Number(i64),
    String(String),
    Func(String),
}

/// the arg of function call
struct Value {
    ty: Types,
}

/// Expr context when we running expression
struct Context {
    //fnTable: HashMap<>,
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
            ValueType::Number(a) => todo!(),
            ValueType::String(_) => todo!(),
            ValueType::Symbol(_) => todo!(),
        }
    }
}
