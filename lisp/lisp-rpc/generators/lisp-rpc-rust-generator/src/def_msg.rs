use std::{error::Error, io::Cursor};

use lisp_rpc_rust_parser::{
    Atom, Expr, Parser, TypeValue,
    data::{ExprData, MapData},
};

#[derive(Debug)]
enum DefMsgErrorType {
    InvalidInput,
}

#[derive(Debug)]
struct DefMsgError {
    msg: String,
    err_type: DefMsgErrorType,
}

impl std::fmt::Display for DefMsgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for DefMsgError {}

#[doc = r#"the struct of def-msg expression
(def-msg name :key value-type)
"#]
#[derive(Debug, Eq, PartialEq)]
struct DefMsg {
    msg_name: String,

    /// the keywords and their types pairs
    keywords: Vec<(String, String)>,
}

impl DefMsg {
    /// make new def msg from str
    fn from_str(source: &str, parser: Option<Parser>) -> Result<Self, Box<dyn Error>> {
        let mut p = match parser {
            Some(p) => p,
            None => Default::default(),
        };

        let expr = p.parse_root_one(Cursor::new(source))?;

        // check the first symbol has to be def-msg
        let rest_expr = match &expr {
            Expr::List(e) => match &e[0] {
                Expr::Atom(Atom {
                    value: TypeValue::Symbol(s),
                    ..
                }) if s == "def-msg" => &e[1..],
                _ => {
                    return Err(Box::new(DefMsgError {
                        msg: "parsing failed, the first symbol should be def-msg".to_string(),
                        err_type: DefMsgErrorType::InvalidInput,
                    }));
                }
            },
            _ => {
                return Err(Box::new(DefMsgError {
                    msg: "parsing failed".to_string(),
                    err_type: DefMsgErrorType::InvalidInput,
                }));
            }
        };

        let name = match &rest_expr[0] {
            Expr::Atom(Atom {
                value: TypeValue::Symbol(s),
                ..
            }) => s.to_string(),
            _ => {
                return Err(Box::new(DefMsgError {
                    msg: "parsing failed, msg name should be symbol".to_string(),
                    err_type: DefMsgErrorType::InvalidInput,
                }));
            }
        };

        let keywords = rest_expr[1..]
            .iter()
            .array_chunks()
            .filter_map(|[k, t]| match (k, t) {
                (
                    Expr::Atom(Atom {
                        value: TypeValue::Keyword(kk),
                    }),
                    Expr::Quote(box Expr::Atom(Atom {
                        value: TypeValue::Symbol(tt),
                    })),
                ) => Some((kk.to_string(), tt.to_string())),
                _ => None,
            })
            .collect::<Vec<_>>();

        Ok(Self {
            msg_name: name,
            keywords: keywords,
        })
    }

    fn gen_code(&self) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_def_msg() {
        let case = r#"(def-msg language-perfer :lang 'string)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                keywords: vec![("lang".to_string(), "string".to_string())],
            }
        );

        // test the dirty string
        let case = r#"  (def-msg language-perfer :lang 'string) (additional)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                keywords: vec![("lang".to_string(), "string".to_string())],
            }
        );

        // test the multiple keywords
        let case = r#"(def-msg language-perfer :lang 'string :version 'number)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                keywords: vec![
                    ("lang".to_string(), "string".to_string()),
                    ("version".to_string(), "number".to_string())
                ],
            }
        );
    }

    #[test]
    fn test_gen_code() {}
}
