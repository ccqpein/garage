use std::{error::Error, io::Cursor};

use lisp_rpc_rust_parser::{Atom, Parser};

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

struct DefMsg {
    msg_name: String,
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
            lisp_rpc_rust_parser::Expr::List(e) => match &e[0] {
                lisp_rpc_rust_parser::Expr::Atom(lisp_rpc_rust_parser::Atom {
                    value: lisp_rpc_rust_parser::TypeValue::Symbol(s),
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

        todo!()
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
        let case0 = r#"(def-msg language-perfer :lang 'string)"#;
    }

    #[test]
    fn test_gen_code() {}
}
