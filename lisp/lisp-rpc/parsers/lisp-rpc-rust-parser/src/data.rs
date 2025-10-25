//! The pure rpc data like (get-book :title "hello world" :version "1984").
//!
//! The first symbol is the name of data, and everything else are the "arguments"

use std::{collections::HashMap, error::Error, io::Cursor};

use itertools::Itertools;
use tracing::debug;

use crate::{Atom, Expr, Parser};

#[derive(Debug)]
enum DataErrorType {
    InvalidInput,
}

#[derive(Debug)]
struct DataError {
    msg: &'static str,
    err_type: DataErrorType,
}

impl std::fmt::Display for DataError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "data operation error {:?}", self)
    }
}

impl Error for DataError {}

#[derive(Debug, PartialEq, Eq)]
pub struct Data {
    name: String,
    inner_atom: Expr,
}

impl Data {
    /// generate the Data from string directly
    fn from_str(p: &Parser, s: &str) -> Result<Self, Box<dyn Error>> {
        let c = Cursor::new(s);
        let exp = p.read_exp(&mut p.tokenize(c))?;

        let name = if let Some(first_atom) = exp.nth(0) {
            match first_atom {
                Expr::Atom(Atom {
                    literal,
                    value: crate::TypeValue::Symbol(_),
                }) => Some(literal.to_string()),
                _ => None,
            }
        } else {
            None
        }
        .ok_or(DataError {
            msg: "the first isn't the name of data (symbol)",
            err_type: DataErrorType::InvalidInput,
        })?;

        Ok(Self {
            name: name,
            inner_atom: exp,
        })
    }

    /// to the data map struct
    fn to_map<'d>(&'d self) -> Result<DataMap<'d>, Box<dyn Error>> {
        let table = self
            .inner_atom
            .iter()
            .ok_or(DataError {
                msg: "inner data isn't the list",
                err_type: DataErrorType::InvalidInput,
            })?
            .get(1..)
            .array_chunks()
            .filter_map(|[k, v]| match (k, v) {
                (
                    Expr::Atom(
                        s @ Atom {
                            value: crate::TypeValue::Keyword(_),
                            ..
                        },
                    ),
                    _,
                ) => Some((s, v)),
                _ => None,
            })
            .collect();

        Ok(DataMap {
            expr: self,
            hash_map: table,
        })
    }

    /// the name of the expr, always the first element depending on the spec
    fn get_name(&self) -> &str {
        &self.name
    }

    /// generate the data
    fn new(name: &str, rest_datas: &[Expr]) -> Result<Self, Box<dyn Error>> {
        let mut d = vec![];

        d.push(Expr::Atom(Atom::read(name)));

        // check

        if rest_datas.len() % 2 != 0 {
            return Err(Box::new(DataError {
                msg: "rest data has to be odd length elements",
                err_type: DataErrorType::InvalidInput,
            }));
        }

        for [k, _] in rest_datas.iter().array_chunks() {
            match k {
                Expr::Atom(Atom {
                    value: crate::TypeValue::Keyword(_),
                    ..
                }) => (),
                _ => {
                    return Err(Box::new(DataError {
                        msg: "data has to be keyword-value pair",
                        err_type: DataErrorType::InvalidInput,
                    }));
                }
            }
        }

        d.append(&mut rest_datas.iter().map(|s| s.clone()).collect());

        Ok(Self {
            name: name.to_string(),
            inner_atom: Expr::List(d),
        })
    }

    /// generate the data
    fn to_str(&self) -> String {
        self.inner_atom.into_tokens()
    }
}

#[derive(Debug)]
struct DataMap<'d> {
    expr: &'d Data,

    /// sym has to be the keyword type
    hash_map: HashMap<&'d Atom, &'d Expr>,
}

impl<'d> DataMap<'d> {
    fn get_name(&self) -> &str {
        self.expr.get_name()
    }

    /// get the value of the keyword
    /// the value has to be the Expr::Atom by now
    fn get(&self, k: &'_ str) -> Option<&Expr> {
        match self.hash_map.get(&Atom::read_keyword(k)) {
            Some(vv) => Some(*vv),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_data_from_str() {
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        let p = Parser::new();
        let d = Data::from_str(&p, s);
        //dbg!(&d);
        assert!(d.is_ok());

        let dd = d.unwrap();
        assert_eq!(dd.get_name(), "get-book");

        let dd_map = dd.to_map().unwrap();
        assert_eq!(dd_map.get_name(), "get-book");
        assert_eq!(
            dd_map.get("title"),
            Some(&Expr::Atom(Atom::read_string("hello world")))
        );

        //

        let s = r#"(get-book :title "hello world" :version 1984)"#;

        let d = Data::from_str(&Parser::new().config_read_number(true), s).unwrap();
        let d = d.to_map().unwrap();

        assert_eq!(d.get_name(), "get-book");
        assert_eq!(
            d.get("version"),
            Some(&Expr::Atom(Atom::read_number("1984", 1984)))
        );
    }

    #[test]
    fn test_new_data() {
        let p = Parser::new().config_read_number(true);
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        let d = Data::from_str(&p, s).unwrap();

        assert_eq!(
            d,
            Data::new(
                "get-book",
                &vec![
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Atom(Atom::read_string("hello world")),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Atom(Atom::read_string("1984"))
                ]
            )
            .unwrap()
        )
    }

    #[test]
    fn test_data_to_str() {
        let p = Parser::new();
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        let d = Data::from_str(&p, s).unwrap();

        assert_eq!(s, d.to_str());
    }
}
