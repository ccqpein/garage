//! The pure rpc data like (get-book :title "hello world" :version "1984").
//!
//! The first symbol is the name of data, and everything else are the "arguments"

use std::{collections::HashMap, env, error::Error, io::Cursor};

use itertools::Itertools;
use tracing::{debug, error};

use crate::{Atom, Expr, Parser, TypeValue};

#[derive(Debug, PartialEq, Eq)]
enum DataErrorType {
    InvalidInput,
    CorruptedData,
}

#[derive(Debug, PartialEq, Eq)]
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

trait FromExpr {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized;
}

trait FromStr: FromExpr {
    fn from_str(p: &Parser, s: &str) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        let c = Cursor::new(s);
        let mut tkn = p.tokenize(c);

        let exp = p.read_router(tkn.get(0).ok_or(DataError {
            msg: "empty str",
            err_type: DataErrorType::InvalidInput,
        })?)?(p, &mut tkn)?;

        Self::from_expr(&exp)
    }
}

/// define all the data, list, and map type that can be treat as Data
#[derive(Debug, PartialEq, Eq)]
pub enum Data {
    /// Data is (data-name keyword-data-pairs...)
    Data(ExprData),

    /// List is '(1 2 3 4 "d")
    List(ListData),

    /// Map is '(:a 1 :b 3)
    Map(MapData),

    /// Everything else is value
    Value(TypeValue),

    /// error if something happen
    Error(DataError),
}

impl FromExpr for Data {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        Self::from_expr(expr)
    }
}

impl FromStr for Data {}

impl std::fmt::Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Data {
    fn from_expr(e: &Expr) -> Result<Self, Box<dyn Error>> {
        match e {
            Expr::List(_) => Ok(Self::Data(ExprData::from_expr(e)?)),
            Expr::Quote(expr) => {
                // list or map
                match expr.as_ref() {
                    Expr::List(exprs) => match &exprs[0] {
                        // Map data
                        Expr::Atom(Atom {
                            value: crate::TypeValue::Keyword(_),
                            ..
                        }) => Ok(Self::Map(MapData::from_expr(e)?)),

                        // List data
                        Expr::Atom(Atom { .. }) => Ok(Self::List(ListData::from_expr(e)?)),

                        _ => Err(Box::new(DataError {
                            msg: "cannot generate Data from the expr",
                            err_type: DataErrorType::InvalidInput,
                        })),
                    },
                    _ => Err(Box::new(DataError {
                        msg: "cannot generate Data from the expr",
                        err_type: DataErrorType::InvalidInput,
                    })),
                }
            }
            Expr::Atom(a) => match &a.value {
                TypeValue::Symbol(_) => {
                    error!("symbol cannot be data");
                    Err(Box::new(DataError {
                        msg: "cannot generate Data from the symbol",
                        err_type: DataErrorType::InvalidInput,
                    }))
                }
                vv @ _ => Ok(Self::Value(vv.clone())),
            },
            _ => {
                error!("cannot generate Data from the expr: {e}");
                Err(Box::new(DataError {
                    msg: "cannot generate Data from the expr",
                    err_type: DataErrorType::InvalidInput,
                }))
            }
        }
    }

    fn to_string(&self) -> String {
        match self {
            Data::Data(value_data) => value_data.to_string(),
            Data::List(list_data) => list_data.to_string(),
            Data::Map(map_data) => map_data.to_string(),
            Data::Value(type_value) => type_value.to_string(),
            Data::Error(data_error) => format!("{:?}", data_error),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprData {
    name: String,
    args: Vec<String>,
    inner_map: DataMap,
}

impl FromExpr for ExprData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        Self::from_expr(expr)
    }
}

impl FromStr for ExprData {}

impl ExprData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>> {
        let exprs = match expr {
            Expr::List(ee) => ee,
            _ => {
                return Err(Box::new(DataError {
                    msg: "cannot generate ExprData from this expr",
                    err_type: DataErrorType::InvalidInput,
                }));
            }
        };

        if exprs.len() < 1 {
            return Err(Box::new(DataError {
                msg: "empty data",
                err_type: DataErrorType::InvalidInput,
            }));
        }

        if exprs.len() % 2 != 1 {
            return Err(Box::new(DataError {
                msg: "rest data has to be odd length elements",
                err_type: DataErrorType::InvalidInput,
            }));
        }

        let name = match &exprs[0] {
            Expr::Atom(Atom {
                value: crate::TypeValue::Symbol(s),
            }) => s,
            _ => {
                return Err(Box::new(DataError {
                    msg: "data's first element has to be symbol",
                    err_type: DataErrorType::InvalidInput,
                }));
            }
        };

        let mut kwrds = vec![];
        for [k, v] in exprs[1..].into_iter().array_chunks() {
            match (k, v) {
                (
                    Expr::Atom(Atom {
                        value: crate::TypeValue::Keyword(k),
                    }),
                    _,
                ) => kwrds.push(k.to_string()),
                _ => {
                    return Err(Box::new(DataError {
                        msg: "",
                        err_type: DataErrorType::InvalidInput,
                    }));
                }
            };
        }

        Ok(Self {
            name: name.to_string(),
            args: kwrds,
            inner_map: DataMap::new(&exprs[1..])?,
        })
    }

    /// the name of the expr, always the first element depending on the spec
    fn get_name(&self) -> &str {
        &self.name
    }

    /// generate the data
    fn to_string(&self) -> String {
        format!(
            "({} {})",
            self.name,
            self.args
                .iter()
                .map(|k| [
                    format!(":{}", k.to_string()),
                    self.inner_map
                        .get(k)
                        .unwrap_or(&Data::Error(DataError {
                            msg: "corrupted data",
                            err_type: DataErrorType::CorruptedData
                        }))
                        .to_string()
                ])
                .flatten()
                .join(" ")
        )
    }

    fn get(&self, k: &str) -> Option<&Data> {
        self.inner_map.get(k)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ListData {
    inner_data: Vec<Data>,
}

impl FromExpr for ListData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        Self::from_expr(expr)
    }
}

impl FromStr for ListData {}

impl ListData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>> {
        match expr {
            Expr::Quote(expr) => match expr.as_ref() {
                Expr::List(exprs) => {
                    let mut res = vec![];
                    for e in exprs {
                        res.push(Data::from_expr(e)?);
                    }

                    Ok(Self { inner_data: res })
                }
                _ => Err(Box::new(DataError {
                    msg: "cannot generate ListData from this expr, not list after quote",
                    err_type: DataErrorType::InvalidInput,
                })),
            },
            _ => Err(Box::new(DataError {
                msg: "cannot generate ListData from this expr, need quoted",
                err_type: DataErrorType::InvalidInput,
            })),
        }
    }

    fn to_string(&self) -> String {
        format!(
            "'({})",
            self.inner_data.iter().map(|d| d.to_string()).join(" ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
struct MapData {
    kwrds: Vec<String>,
    map: DataMap,
}

impl FromExpr for MapData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>>
    where
        Self: Sized,
    {
        Self::from_expr(expr)
    }
}

impl FromStr for MapData {}

impl MapData {
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>> {
        let mut kwrds = vec![];
        let map = match expr {
            Expr::Quote(e2) => match e2.as_ref() {
                Expr::List(ee) => {
                    for [k, _] in ee.iter().array_chunks() {
                        match k {
                            Expr::Atom(Atom {
                                value: crate::TypeValue::Keyword(k),
                            }) => {
                                kwrds.push(k.to_string());
                            }
                            _ => {
                                return Err(Box::new(DataError {
                                    msg: "MapData has to be keyword pairs like '(:a 1 :b 2)",
                                    err_type: DataErrorType::InvalidInput,
                                }));
                            }
                        }
                    }

                    DataMap::new(&ee)?
                }
                _ => {
                    return Err(Box::new(DataError {
                        msg: "MapData has to be quoted like '(:a 1 :b 2)",
                        err_type: DataErrorType::InvalidInput,
                    }));
                }
            },
            _ => {
                return Err(Box::new(DataError {
                    msg: "MapData has to be quoted like '(:a 1 :b 2)",
                    err_type: DataErrorType::InvalidInput,
                }));
            }
        };

        Ok(Self { kwrds, map })
    }

    fn to_string(&self) -> String {
        format!(
            "'({})",
            self.kwrds
                .iter()
                .map(|k| [
                    format!(":{}", k.to_string()),
                    self.map
                        .get(k)
                        .unwrap_or(&Data::Error(DataError {
                            msg: "corrupted data",
                            err_type: DataErrorType::CorruptedData
                        }))
                        .to_string()
                ])
                .flatten()
                .join(" ")
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
struct DataMap {
    hash_map: HashMap<String, Data>,
}

impl DataMap {
    fn new(exprs: &[Expr]) -> Result<Self, Box<dyn Error>> {
        let mut table = HashMap::new();
        for [k, v] in exprs.iter().array_chunks() {
            match (k, v) {
                (
                    Expr::Atom(Atom {
                        value: crate::TypeValue::Keyword(k),
                    }),
                    _,
                ) => {
                    table.insert(k.to_string(), Data::from_expr(v)?);
                }
                _ => (),
            };
        }

        Ok(Self { hash_map: table })
    }

    pub fn get(&self, k: &'_ str) -> Option<&Data> {
        match self.hash_map.get(k) {
            Some(vv) => Some(vv),
            None => None,
        }
    }

    pub fn to_string(&self) -> String {
        self.hash_map
            .iter()
            .map(|(k, v)| format!(":{} {}", k, v.to_string()))
            .join(" ")
    }
}

impl FromIterator<(String, Data)> for DataMap {
    fn from_iter<T: IntoIterator<Item = (String, Data)>>(iter: T) -> Self {
        Self {
            hash_map: iter.into_iter().collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::assert_matches::assert_matches;

    use super::*;

    #[test]
    fn test_read_data_from_str() {
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        let p = Parser::new();
        let d = ExprData::from_str(&p, s);
        //dbg!(&d);
        assert!(d.is_ok());

        let dd = d.unwrap();
        assert_eq!(dd.get_name(), "get-book");

        assert_eq!(dd.get_name(), "get-book");
        assert_eq!(
            dd.get("title"),
            Some(&Data::from_str(&p, r#""hello world""#).unwrap())
        );

        //

        let s = r#"(get-book :title "hello world" :version 1984)"#;

        let d = ExprData::from_str(&Parser::new().config_read_number(true), s).unwrap();

        assert_eq!(d.get_name(), "get-book");
        assert_eq!(
            d.get("version"),
            Some(&Data::Value(TypeValue::Number(1984)))
        );
    }

    #[test]
    fn test_read_data_from_str_nesty() {
        let s = r#"(get-book :title "hello world" :version '(1 2 3 4) :map '(:a 2 :r 4))"#;
        let p = Parser::new().config_read_number(true);

        //dbg!(p.parse_root(Cursor::new(s)));

        let d = Data::from_str(&p, s).unwrap();

        dbg!(&d);
        assert_matches!(d, Data::Data(ExprData { .. }));

        assert_eq!(
            d.to_string(),
            r#"(get-book :title "hello world" :version '(1 2 3 4) :map '(:a 2 :r 4))"#
        );

        let Data::Data(d) = d else { panic!() };

        assert_eq!(
            d.get("version"),
            Some(&Data::List(
                ListData::from_str(&p, r#"'(1 2 3 4)"#).unwrap()
            ))
        );

        assert_eq!(
            d.get("map"),
            Some(&Data::Map(
                MapData::from_str(&p, r#"'(:a 2 :r 4)"#).unwrap()
            ))
        );

        assert_eq!(
            d.to_string(),
            r#"(get-book :title "hello world" :version '(1 2 3 4) :map '(:a 2 :r 4))"#
        )
    }

    #[test]
    fn test_data_to_str() {
        let p = Parser::new();
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        let d = ExprData::from_str(&p, s).unwrap();

        assert_eq!(s, d.to_string());
    }
}
