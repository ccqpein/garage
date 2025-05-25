use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{self, Rc},
};

use crate::parser::*;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpressionError {
    NumberParsingError,
    Undefined(String),
    Inevalable,

    FuncCallErr,
}

pub struct Args<'arg> {
    pub args: &'arg [Value],
}

impl<'arg> Args<'arg> {
    /// check all args type before call function
    pub fn pre_check_all_types(&self, f: fn(&Value) -> bool) -> bool {
        self.args.into_iter().all(|n| f(n))
    }
}

type Lmda = fn(&mut Context, &Args) -> Result<Value, ExpressionError>;

/// the arg of function call
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Number(i64),
    String(String),
    Func(Lmda),
}

impl Value {
    pub fn is_number(&self) -> bool {
        matches!(self, Self::Number(_))
    }

    pub fn into_number(&self) -> i64 {
        if let Value::Number(nn) = self {
            return *nn;
        } else {
            panic!("cannot into number")
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::String(_))
    }

    pub fn is_func(&self) -> bool {
        matches!(self, Self::Func(_))
    }
}

/// Expr context when we running expression
#[derive(Default)]
pub struct Context {
    pub fnTable: HashMap<String, Lmda>,
    pub vTable: HashMap<String, Value>,
}

impl Context {
    fn get_symbol(&self, sym: &Sym) -> Result<Option<Value>, ExpressionError> {
        match self.vTable.get(&sym.name) {
            Some(vv) => Ok(Some(vv.clone())),
            None => Ok(None),
        }
    }

    fn get_func(&self, sym: &Sym) -> Result<Option<Value>, ExpressionError> {
        match self.fnTable.get(&sym.name) {
            Some(vv) => Ok(Some(Value::Func(*vv))),
            None => Ok(None),
        }
    }
}

struct Expr<'a> {
    a: &'a Atom,
    c: Rc<RefCell<Context>>,
}

impl<'a> Expr<'a> {
    pub fn from_atom(a: &'a Atom, c: Rc<RefCell<Context>>) -> Self {
        Self { a: a, c: c }
    }

    pub fn get_context(&self) -> Rc<RefCell<Context>> {
        self.c.clone()
    }

    pub fn eval(&self) -> Result<Value, ExpressionError> {
        match self.a {
            Atom::Sym(sym) => self.eval_sym(sym),
            Atom::List(atoms) => {
                let Value::Func(f) = self.eval_func(
                    atoms
                        .first()
                        .ok_or(ExpressionError::Inevalable)?
                        .into_sym()
                        .ok_or(ExpressionError::Inevalable)?,
                )?
                else {
                    panic!()
                };

                // let Value::Func(f) = Self::from_atom(
                //     atoms.first().ok_or(ExpressionError::Inevalable)?,
                //     self.c.clone(),
                // )
                // .eval_func()?
                // else {
                //     panic!()
                // };

                let mut args = Vec::with_capacity(atoms.len() - 1);
                for a in &atoms[1..] {
                    args.push(Self::from_atom(a, self.get_context()).eval()?);
                }

                let rc_ = &self.get_context();
                let mut context = RefCell::borrow_mut(rc_);
                f(&mut context, &Args { args: &args })
            }
        }
    }

    fn eval_func(&self, s: &Sym) -> Result<Value, ExpressionError> {
        if let Some(vv) = self.c.borrow().get_func(s)? {
            return Ok(vv);
        } else {
            return Err(ExpressionError::Undefined(s.name.clone()));
        }
    }

    fn eval_sym(&self, s: &Sym) -> Result<Value, ExpressionError> {
        if let Ok(number) = s.name.parse::<i64>() {
            return Ok(Value::Number(number));
        }

        if s.is_string() {
            return Ok(Value::String(s.name.to_string()));
        }

        if let Some(vv) = self.c.borrow().get_symbol(s)? {
            return Ok(vv);
        } else {
            return Err(ExpressionError::Undefined(s.name.clone()));
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::functions;

    use super::*;

    #[test]
    fn test_context() {
        let mut ctx = Context {
            fnTable: HashMap::new(),
            ..Default::default()
        };

        ctx.fnTable.insert("add".to_string(), functions::math::add);

        let args = vec![Value::Number(1), Value::Number(1), Value::Number(2)];
        assert_eq!(
            ctx.fnTable.get("add").unwrap()(&mut ctx, &Args { args: &args },),
            Ok(Value::Number(4))
        );

        // ctx.fnTable.insert("mul".to_string(), functions::math::mul);

        // dbg!(ctx.fnTable.get("mul").unwrap()(
        //     &mut ctx,
        //     Value {
        //         ty: Types::Number(1),
        //     },
        // ));
    }

    #[test]
    fn test_eval() {
        let mut t = tokenize(Cursor::new(r#"(add 1 2 3)"#.as_bytes()));
        let atom = &read_root(&mut t).unwrap()[0];

        //dbg!(atom);

        let mut ctx = Context {
            ..Default::default()
        };

        ctx.fnTable.insert("add".to_string(), functions::math::add);

        let exp = Expr::from_atom(atom, Rc::new(RefCell::new(ctx)));

        assert_eq!(exp.eval(), Ok(Value::Number(6)));
    }
}
