use std::{error::Error, fs::File, io::Cursor, path::Path};

use lisp_rpc_rust_parser::{Atom, Expr, Parser, TypeValue, data::MapData};
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
    fn to_rust_fields(&self) -> Result<Vec<GeneratedField>, Box<dyn Error + '_>> {
        let mut res = Vec::with_capacity(self.keywords.len());

        for (field_name, data) in self.keywords.iter() {
            let field_type = data_to_field_type(data)?;

            res.push(GeneratedField {
                name: kebab_to_snake_case(field_name),
                field_type: data_to_field_type(data)?,
                comment: None, // need add the comment in spec or not
            });
        }

        Ok(res)
    }

    /// convet this spec to GeneratedStructs (self and the anonymity type)
    fn create_gen_structs(&self) -> Result<Vec<GeneratedStruct>, Box<dyn Error + '_>> {
        let mut res = vec![];
        let mut fields = vec![];
        for (field_name, d) in self.keywords.iter() {
            match d {
                Data::List(list_data) => {
                    todo!()
                }
                Data::Map(map_data) => {
                    //:= make new def msg
                }
                Data::Value(lisp_rpc_rust_parser::TypeValue::Symbol(s)) => fields.push(
                    GeneratedField::new(field_name.to_string(), type_translate(s), None),
                ),
                Data::Data(_) => {
                    return Err(Box::new(SpecError {
                        msg: format!(
                            "data {} convert to type error, cannot be expr in as type",
                            d
                        ),
                        err_type: SpecErrorType::InvalidInput,
                    }));
                }
                Data::Error(data_error) => return Err(Box::new(data_error)),
                _ => {
                    return Err(Box::new(SpecError {
                        msg: format!("data {} convert to type error", d),
                        err_type: SpecErrorType::InvalidInput,
                    }));
                }
            }
        }

        res.push(GeneratedStruct {
            name: self.rpc_name.to_string(),
            derived_traits: None,
            fields,
            comment: None,
        });

        Ok(res)
    }

    /// use the GeneratedStruct to generate the code
    fn gen_code(&self, temp_file_path: impl AsRef<Path>) -> Result<String, Box<dyn Error + '_>> {
        let mut tera = Tera::default();
        let mut context = Context::new();

        tera.add_template_file(temp_file_path, Some("rpc_struct_template"))?;

        let gs = GeneratedStruct::new(
            kebab_to_pascal_case(&self.rpc_name),
            None,
            self.to_rust_fields()?,
            None,
        );

        gs.insert_template(&mut context);

        Ok(tera.render("rpc_struct_template", &context)?)
    }
}

#[cfg(test)]
mod tests {
    use lisp_rpc_rust_parser::data::FromStr;
    use std::path::PathBuf;

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

    #[test]
    fn test_gen_code() {
        let project_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let template_file_path = project_root.join("templates/def_struct.rs.template");

        let case = r#"(def-rpc get-book
      '(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))
    'book-info)"#;
        let dm = DefRPC::from_str(case, Default::default()).unwrap();

        //         assert_eq!(
        //             dm.gen_code(&template_file_path).unwrap(),
        //             r#"#[derive(Debug)]
        // pub struct LanguagePerfer {
        //     lang: String,
        // }"#
        //         );
    }
}
