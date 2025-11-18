//! the mod that handle def-msg expr

use std::{error::Error, fs::File, io::Cursor, path::Path};

use lisp_rpc_rust_parser::{Atom, Expr, Parser, TypeValue, data::MapData};
use tera::{Context, Tera};

use super::*;

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
        write!(f, "{:?}", self)
    }
}

impl Error for DefMsgError {}

#[doc = r#"the struct of def-msg expression
(def-msg name :key value-type)
"#]
#[derive(Debug, Eq, PartialEq)]
pub struct DefMsg {
    msg_name: String,

    /// the keywords and their types pairs
    rest_expr: Vec<Expr>,
}

impl DefMsg {
    pub fn new(msg_name: &str, rest_expr: &[Expr]) -> Result<Self, Box<dyn Error>> {
        if rest_expr.iter().array_chunks().all(|[k, _]| {
            matches!(
                k,
                Expr::Atom(Atom {
                    value: TypeValue::Keyword(_),
                })
            )
        }) {
            Ok(Self {
                msg_name: msg_name.to_string(),
                rest_expr: rest_expr.to_vec(),
            })
        } else {
            Err(Box::new(DefMsgError {
                msg: "parsing failed, msg name arguments should be keyword-value pairs".to_string(),
                err_type: DefMsgErrorType::InvalidInput,
            }))
        }
    }

    /// make new def msg from str
    fn from_str(source: &str, parser: Option<Parser>) -> Result<Self, Box<dyn Error>> {
        let mut p = match parser {
            Some(p) => p,
            None => Default::default(),
        };

        let expr = p.parse_root_one(Cursor::new(source))?;

        Self::from_expr(&expr)
    }

    pub fn if_def_msg_expr(expr: &Expr) -> bool {
        match &expr {
            Expr::List(e) => match &e[0] {
                Expr::Atom(Atom {
                    value: TypeValue::Symbol(s),
                    ..
                }) => s == "def-msg",
                _ => false,
            },
            _ => false,
        }
    }

    /// make new DefMsg from the one expr
    /// (def-msg name :keyword value)
    fn from_expr(expr: &Expr) -> Result<Self, Box<dyn Error>> {
        let rest_expr: &[Expr];
        if Self::if_def_msg_expr(expr) {
            match &expr {
                Expr::List(e) => rest_expr = &e[1..],
                _ => {
                    return Err(Box::new(DefMsgError {
                        msg: "parsing failed, the first symbol should be def-msg".to_string(),
                        err_type: DefMsgErrorType::InvalidInput,
                    }));
                }
            }
        } else {
            return Err(Box::new(DefMsgError {
                msg: "parsing failed, the first symbol should be def-msg".to_string(),
                err_type: DefMsgErrorType::InvalidInput,
            }));
        }

        let name = match &rest_expr[0] {
            Expr::Atom(Atom {
                value: TypeValue::Symbol(s),
                ..
            }) => s,
            _ => {
                return Err(Box::new(DefMsgError {
                    msg: "parsing failed, msg name should be symbol".to_string(),
                    err_type: DefMsgErrorType::InvalidInput,
                }));
            }
        };

        Self::new(name, &rest_expr[1..])
    }

    /// convet this spec to GeneratedStructs (self and the anonymity type)
    pub fn create_gen_structs(&self) -> Result<Vec<GeneratedStruct>, Box<dyn Error>> {
        let mut res = vec![];
        let mut fields = vec![];
        for [k, v] in self.rest_expr.iter().array_chunks() {
            match (k, v) {
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
                    let new_msg_name = self.msg_name.to_string() + "-" + f;
                    res.append(&mut Self::new(&new_msg_name, inner_exprs)?.create_gen_structs()?);
                    fields.push(GeneratedField {
                        name: f.to_string(),
                        field_type: new_msg_name,
                        comment: None,
                    });
                }
                _ => {
                    return Err(Box::new(DefMsgError {
                        msg:
                            "create gen structs failed, arguments has to be the keywords-value pair"
                                .to_string(),
                        err_type: DefMsgErrorType::InvalidInput,
                    }));
                }
            }
        }

        res.push(GeneratedStruct::new(
            self.msg_name.to_string(),
            None,
            fields,
            None,
        ));

        Ok(res)
    }

    // fn gen_code(&self, temp_file_path: impl AsRef<Path>) -> Result<String, Box<dyn Error>> {
    //     let mut tera = Tera::default();
    //     let mut context = Context::new();

    //     tera.add_template_file(temp_file_path, Some("rpc_struct_template"))?;

    //     let gs = GeneratedStruct::new(
    //         kebab_to_pascal_case(&self.msg_name),
    //         None,
    //         self.to_rust_fields()?,
    //         None,
    //     );

    //     gs.insert_template(&mut context);

    //     Ok(tera.render("rpc_struct_template", &context)?)
    //}
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;
    use lisp_rpc_rust_parser::Expr;

    #[test]
    fn test_parse_def_msg() {
        let case = r#"(def-msg language-perfer :lang 'string)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                rest_expr: vec![
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string"))))
                ],
            }
        );

        // test the dirty string
        let case = r#"  (def-msg language-perfer :lang 'string) (additional)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                rest_expr: vec![
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string"))))
                ],
            }
        );

        // test the multiple keywords
        let case = r#"(def-msg language-perfer :lang 'string :version 'number)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm,
            DefMsg {
                msg_name: "language-perfer".to_string(),
                rest_expr: vec![
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("number"))))
                ],
            }
        );
    }

    #[test]
    fn test_create_gen_structs() {
        let spec = r#"(def-msg book-info
    :lang 'language-perfer
    :title 'string
    :version 'string
    :id 'string)"#;

        let x = DefMsg::from_str(spec, None).unwrap();
        assert_eq!(
            x.create_gen_structs().unwrap(),
            vec![GeneratedStruct {
                name: "book-info".to_string(),
                derived_traits: None,
                fields: vec![
                    GeneratedField {
                        name: "lang".to_string(),
                        field_type: "language-perfer".to_string(),
                        comment: None
                    },
                    GeneratedField {
                        name: "title".to_string(),
                        field_type: "string".to_string(),
                        comment: None
                    },
                    GeneratedField {
                        name: "version".to_string(),
                        field_type: "string".to_string(),
                        comment: None
                    },
                    GeneratedField {
                        name: "id".to_string(),
                        field_type: "string".to_string(),
                        comment: None
                    }
                ],
                comment: None
            }],
        );

        // anonymous fields

        let spec = r#"(def-msg book-info
    :lang '(:a 'string :b 'number)
    :title 'string
    :version 'string
    :id 'string)"#;

        let x = DefMsg::from_str(spec, None).unwrap();
        assert_eq!(
            x.create_gen_structs().unwrap(),
            vec![
                GeneratedStruct {
                    name: "book-info-lang".to_string(),
                    derived_traits: None,
                    fields: vec![
                        GeneratedField {
                            name: "a".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "b".to_string(),
                            field_type: "number".to_string(),
                            comment: None
                        }
                    ],
                    comment: None
                },
                GeneratedStruct {
                    name: "book-info".to_string(),
                    derived_traits: None,
                    fields: vec![
                        GeneratedField {
                            name: "lang".to_string(),
                            field_type: "book-info-lang".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "title".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "version".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        },
                        GeneratedField {
                            name: "id".to_string(),
                            field_type: "string".to_string(),
                            comment: None
                        }
                    ],
                    comment: None
                }
            ],
        );
    }

    #[test]
    fn test_gen_code() {
        let project_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let template_file_path = project_root.join("templates/def_struct.rs.template");

        let case = r#"(def-msg language-perfer :lang 'string)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        //         assert_eq!(
        //             dm.gen_code(&template_file_path).unwrap(),
        //             r#"#[derive(Debug)]
        // pub struct LanguagePerfer {
        //     lang: String,
        // }"#
        //         );

        //         //
        //         let case = r#"(def-msg language-perfer :lang 'string :version 'number)"#;
        //         let dm = DefMsg::from_str(case, Default::default()).unwrap();
        //         assert_eq!(
        //             dm.gen_code(&template_file_path).unwrap(),
        //             r#"#[derive(Debug)]
        // pub struct LanguagePerfer {
        //     lang: String,
        //     version: i64,
        // }"#
        //         );
    }
}
