//! The pure rpc data like (get-book :title "hello world" :version "1984").
//!
//! The first symbol is the name of data, and everything else are the "arguments"

use std::{collections::HashMap, error::Error, io::Cursor};

use itertools::Itertools;

use crate::{Atom, Sym, read_exp, tokenize};

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

pub struct Data {
    name: String,
    inner_atom: Atom,
}

impl Data {
    /// generate the Data from string directly
    fn from_string(s: &str) -> Result<Self, Box<dyn Error>> {
        let c = Cursor::new(s);
        let exp = read_exp(&mut tokenize(c))?;
        let name = if let Some(first_atom) = exp.nth(0) {
            match first_atom {
                Atom::Sym(Sym {
                    name,
                    read_type: crate::ParserType::String,
                }) => Some(name.to_string()),
                _ => None,
            }
        } else {
            None
        }
        .ok_or(DataError {
            msg: "the first isn't the name of data (string)",
            err_type: DataErrorType::InvalidInput,
        })?;

        Ok(Self {
            name: name,
            inner_atom: exp,
        })
    }

    /// to the data map struct
    fn to_hash_map<'d>(&'d self) -> Result<DataMap<'d>, Box<dyn Error>> {
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
                    Atom::Sym(
                        s @ Sym {
                            read_type: crate::ParserType::Keyword,
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
}

struct DataMap<'d> {
    expr: &'d Data,
    hash_map: HashMap<&'d Sym, &'d Atom>,
}

impl<'d> DataMap<'d> {
    fn get_name(&self) -> &str {
        self.expr.get_name()
    }
}
