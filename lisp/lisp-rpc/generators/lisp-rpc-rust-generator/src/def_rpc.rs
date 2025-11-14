use std::{error::Error, fs::File, io::Cursor, path::Path};

use lisp_rpc_rust_parser::{Atom, Expr, Parser, TypeValue, data::MapData};
use serde::Serialize;
use tera::{Context, Tera};

use super::*;

#[derive(Debug)]
enum DefRPCErrorType {
    InvalidInput,
}

#[derive(Debug)]
struct DefRPCError {
    msg: String,
    err_type: DefRPCErrorType,
}

impl std::fmt::Display for DefRPCError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for DefRPCError {}

#[derive(Debug, Serialize)]
pub struct GeneratedField {
    pub name: String,
    pub field_type: String,
    pub comment: Option<String>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefRPC {
    rpc_name: String,

    /// the keywords and their types pairs of request body
    keywords: MapData,

    ///
    return_value: Option<String>,
}

impl DefRPC {
    fn from_str(source: &str, parser: Option<Parser>) -> Result<Self, Box<dyn Error>> {
        let mut p = match parser {
            Some(p) => p,
            None => Default::default(),
        };

        let expr = p.parse_root_one(Cursor::new(source))?;

        Self::from_expr(&expr)
    }

    pub fn if_def_rpc_expr(expr: &Expr) -> bool {
        match &expr {
            Expr::List(e) => match &e[0] {
                Expr::Atom(Atom {
                    value: TypeValue::Symbol(s),
                    ..
                }) => s == "def-rpc",
                _ => false,
            },
            _ => false,
        }
    }

    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>> {
        let rest_expr: &[Expr];
        if Self::if_def_rpc_expr(expr) {
            match &expr {
                Expr::List(e) => rest_expr = &e[1..],
                _ => {
                    return Err(Box::new(DefRPCError {
                        msg: "parsing failed, the first symbol should be def-rpc".to_string(),
                        err_type: DefRPCErrorType::InvalidInput,
                    }));
                }
            }
        } else {
            return Err(Box::new(DefRPCError {
                msg: "parsing failed, the first symbol should be def-rpc".to_string(),
                err_type: DefRPCErrorType::InvalidInput,
            }));
        }

        let rpc_name = match &rest_expr[0] {
            Expr::Atom(Atom {
                value: TypeValue::Symbol(s),
                ..
            }) => s.to_string(),
            _ => {
                return Err(Box::new(DefRPCError {
                    msg: "parsing failed, rpc name should be symbol".to_string(),
                    err_type: DefRPCErrorType::InvalidInput,
                }));
            }
        };

        //dbg!(&rest_expr[1]);
        let keywords_pair = MapData::from_expr(&rest_expr[1])?;

        let return_value = match rest_expr.get(2) {
            Some(Expr::Quote(box e)) => match e {
                Expr::Atom(Atom {
                    value: TypeValue::Symbol(rn),
                }) => Some(rn.to_string()),
                //Expr::List(exprs) => todo!(), // need to support the anonymity return type
                _ => {
                    return Err(Box::new(DefRPCError {
                        msg: "parsing failed, quoted quoted".to_string(),
                        err_type: DefRPCErrorType::InvalidInput,
                    }));
                }
            },
            None => None,
            _ => {
                return Err(Box::new(DefRPCError {
                    msg: "parsing failed, return type has to be quoted".to_string(),
                    err_type: DefRPCErrorType::InvalidInput,
                }));
            }
        };

        Ok(Self {
            rpc_name,
            keywords: keywords_pair,
            return_value,
        })
    }

    /// to generate the struct fields
    fn to_fields(&self) -> Vec<GeneratedField> {
        //:= next type translater
        self.keywords
            .iter()
            .map(|(field_name, data)| GeneratedField {
                name: kebab_to_snake_case(field_name),
                field_type: todo!(),
                comment: None, // need add the comment in spec or not
            })
            .collect::<Vec<_>>();

        todo!()
    }

    /// use the GeneratedStruct to generate the code
    fn gen_code(&self, temp_file_path: impl AsRef<Path>) -> Result<String, Box<dyn Error>> {
        let mut tera = Tera::default();
        let mut context = Context::new();

        tera.add_template_file(temp_file_path, None)?;

        let gs = GeneratedStruct::new(
            kebab_to_pascal_case(&self.rpc_name),
            None,
            self.to_fields(),
            None,
        );

        gs.insert_template(&mut context);

        // let struct_name = kebab_to_pascal_case(&self.rpc_name);
        // context.insert("rpc_name", &struct_name);

        // let fields_data = self.to_fields();

        // context.insert("fields", &fields_data);

        let rendered_code = tera.render("rpc_struct_template", &context)?;

        Ok(rendered_code)
    }
}

#[cfg(test)]
mod tests {
    use lisp_rpc_rust_parser::data::FromStr;

    use super::*;

    #[test]
    fn test_parse_def_rpc() {
        let case = r#"(def-rpc get-book
      '(:title 'string :vesion 'string :lang 'language-perfer)
    'book-info)"#;
        let dr = DefRPC::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dr,
            DefRPC {
                rpc_name: "get-book".to_string(),
                keywords: MapData::from_str(
                    &Default::default(),
                    "'(:title 'string :vesion 'string :lang 'language-perfer)"
                )
                .unwrap(),
                return_value: Some("book-info".to_string()),
            }
        );

        let case = r#"(def-rpc get-book
      '(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))
    'book-info)"#;
        let dr = DefRPC::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dr,
            DefRPC {
                rpc_name: "get-book".to_string(),
                keywords: MapData::from_str(
                    &Default::default(),
                    "'(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))"
                )
                .unwrap(),
                return_value: Some("book-info".to_string()),
            }
        )
    }
}
