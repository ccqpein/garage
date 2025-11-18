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
    args: Vec<Expr>,

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

    /// make new DefRPC from the one expr
    /// (def-rpc name '(:keyword value) 'return-value)
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

        //dbg!(&rest_expr);
        let arguments = match &rest_expr[1] {
            Expr::Quote(box Expr::List(exprs)) => exprs,
            _ => {
                return Err(Box::new(DefRPCError {
                    msg: "parsing failed, second arguments has to be list of keyword-value pairs"
                        .to_string(),
                    err_type: DefRPCErrorType::InvalidInput,
                }));
            }
        };

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
            args: arguments.to_vec(),
            return_value,
        })
    }

    /// convet this spec to GeneratedStructs (self and the anonymity type)
    pub fn create_gen_structs(&self) -> Result<Vec<GeneratedStruct>, Box<dyn Error>> {
        let mut res = vec![];
        let mut fields = vec![];
        for [field, ty] in self.args.iter().array_chunks() {
            match (field, ty) {
                (
                    Expr::Atom(Atom {
                        value: TypeValue::Keyword(f),
                    }),
                    Expr::Quote(box Expr::Atom(Atom {
                        value: TypeValue::Symbol(t),
                    })),
                ) => {
                    fields.push(GeneratedField {
                        name: f.to_string(),
                        field_type: t.to_string(),
                        comment: None,
                    });
                }
                (
                    Expr::Atom(Atom {
                        value: TypeValue::Keyword(f),
                    }),
                    Expr::Quote(box Expr::List(inner_exprs)),
                ) => {
                    // anonymity msg type
                    let new_rpc_name = self.rpc_name.to_string() + "-" + f;
                    res.append(&mut DefMsg::new(&new_rpc_name, inner_exprs)?.create_gen_structs()?);
                    fields.push(GeneratedField {
                        name: f.to_string(),
                        field_type: new_rpc_name,
                        comment: None,
                    });
                }
                _ => {
                    return Err(Box::new(DefRPCError {
                        msg:
                            "create gen structs failed, arguments has to be the keywords-value pair"
                                .to_string(),
                        err_type: DefRPCErrorType::InvalidInput,
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

    ///// use the GeneratedStruct to generate the code
    // fn gen_code(&self, temp_file_path: impl AsRef<Path>) -> Result<String, Box<dyn Error + '_>> {
    //     let mut tera = Tera::default();
    //     let mut context = Context::new();

    //     tera.add_template_file(temp_file_path, Some("rpc_struct_template"))?;

    //     let gs = GeneratedStruct::new(
    //         kebab_to_pascal_case(&self.rpc_name),
    //         None,
    //         self.to_rust_fields()?,
    //         None,
    //     );

    //     gs.insert_template(&mut context);

    //     Ok(tera.render("rpc_struct_template", &context)?)
    // }
}

#[cfg(test)]
mod tests {

    use std::path::PathBuf;

    use super::*;

    #[test]
    fn test_parse_def_rpc() {
        let case = r#"(def-rpc get-book
      '(:title 'string :version 'string :lang 'language-perfer)
    'book-info)"#;
        let dr = DefRPC::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dr,
            DefRPC {
                rpc_name: "get-book".to_string(),
                args: vec![
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("language-perfer")))),
                ],
                return_value: Some("book-info".to_string())
            }
        );

        let case = r#"(def-rpc get-book
      '(:title 'string :version 'string :lang '(:lang 'string :encoding 'number))
    'book-info)"#;
        let dr = DefRPC::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dr,
            DefRPC {
                rpc_name: "get-book".to_string(),
                args: vec![
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::List(vec![
                        Expr::Atom(Atom::read_keyword("lang")),
                        Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                        Expr::Atom(Atom::read_keyword("encoding")),
                        Expr::Quote(Box::new(Expr::Atom(Atom::read("number")))),
                    ]))),
                ],
                return_value: Some("book-info".to_string())
            }
        )
    }

    #[test]
    fn test_create_gen_structs() {
        let case = r#"(def-rpc get-book
      '(:title 'string :version 'string :lang 'language-perfer)
    'book-info)"#;
        let dr = DefRPC::from_str(case, Default::default()).unwrap();
        assert_eq!(
            dr.create_gen_structs().unwrap(),
            vec![GeneratedStruct {
                name: "get-book".to_string(),
                derived_traits: None,
                fields: vec![
                    GeneratedField {
                        name: "title".to_string(),
                        field_type: "string".to_string(),
                        comment: None,
                    },
                    GeneratedField {
                        name: "version".to_string(),
                        field_type: "string".to_string(),
                        comment: None
                    },
                    GeneratedField {
                        name: "lang".to_string(),
                        field_type: "language-perfer".to_string(),
                        comment: None
                    }
                ],
                comment: None
            }]
        );

        let spec = r#"(def-rpc get-book
      '(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))
    'book-info)"#;

        let dr = DefRPC::from_str(spec, None).unwrap();
        assert_eq!(
            dr.create_gen_structs().unwrap(),
            vec![
                GeneratedStruct {
                    name: "get-book-lang".to_string(),
                    derived_traits: None,
                    fields: vec![
                        GeneratedField {
                            name: "lang".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "encoding".to_string(),
                            field_type: "number".to_string(),
                            comment: None
                        }
                    ],
                    comment: None
                },
                GeneratedStruct {
                    name: "get-book".to_string(),
                    derived_traits: None,
                    fields: vec![
                        GeneratedField {
                            name: "title".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "vesion".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "lang".to_string(),
                            field_type: "get-book-lang".to_string(),
                            comment: None
                        }
                    ],
                    comment: None
                }
            ]
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
