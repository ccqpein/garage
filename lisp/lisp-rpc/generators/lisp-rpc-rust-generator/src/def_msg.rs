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

    /// to generate the struct fields
    fn to_rust_fields(&self) -> Result<Vec<GeneratedField>, Box<dyn Error>> {
        let mut res = Vec::with_capacity(self.keywords.len());

        for (field_name, ty) in self.keywords.iter() {
            res.push(GeneratedField {
                name: kebab_to_snake_case(field_name),
                field_type: type_translate(ty)?,
                comment: None, // need add the comment in spec or not
            });
        }

        Ok(res)
    }

    fn gen_code(&self, temp_file_path: impl AsRef<Path>) -> Result<String, Box<dyn Error>> {
        let mut tera = Tera::default();
        let mut context = Context::new();

        tera.add_template_file(temp_file_path, Some("rpc_struct_template"))?;

        let gs = GeneratedStruct::new(
            kebab_to_pascal_case(&self.msg_name),
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
    use std::path::PathBuf;

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
    fn test_gen_code() {
        let project_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let template_file_path = project_root.join("templates/def_struct.rs.template");

        let case = r#"(def-msg language-perfer :lang 'string)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();

        assert_eq!(
            dm.gen_code(&template_file_path).unwrap(),
            r#"#[derive(Debug)]
pub struct LanguagePerfer {
    lang: String,
}"#
        );

        //
        let case = r#"(def-msg language-perfer :lang 'string :version 'number)"#;
        let dm = DefMsg::from_str(case, Default::default()).unwrap();
        assert_eq!(
            dm.gen_code(&template_file_path).unwrap(),
            r#"#[derive(Debug)]
pub struct LanguagePerfer {
    lang: String,
    version: i64,
}"#
        );
    }
}
