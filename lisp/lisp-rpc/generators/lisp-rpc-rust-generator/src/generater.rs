use serde::Serialize;
use tera::Context;

#[derive(Debug, Serialize)]
pub struct GeneratedField {
    pub name: String,
    pub field_type: String,
    pub comment: Option<String>,
}

impl GeneratedField {
    pub fn new(name: String, field_type: String, comment: Option<String>) -> Self {
        Self {
            name,
            field_type,
            comment,
        }
    }
}

#[derive(Debug, Serialize)]
pub struct GeneratedStruct {
    pub name: String,
    pub derived_traits: Option<Vec<String>>,
    pub fields: Vec<GeneratedField>,
    pub comment: Option<String>,
}

impl GeneratedStruct {
    pub fn new(
        name: String,
        derived_traits: Option<Vec<String>>,
        fields: Vec<GeneratedField>,
        comment: Option<String>,
    ) -> Self {
        Self {
            name,
            derived_traits,
            fields,
            comment,
        }
    }

    pub fn insert_template(&self, ctx: &mut Context) {
        ctx.insert("name", &self.name);
        ctx.insert("fields", &self.fields);
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use tera::{Context, Tera};

    #[test]
    fn test_generate_struct() {
        let temp = include_str!("../templates/def_rpc.rs.template");
        let mut tera = Tera::default();
        let mut context = Context::new();

        //dbg!(temp);
        tera.add_raw_template("test", temp).unwrap();

        let s = GeneratedStruct {
            name: "name".to_string(),
            derived_traits: None,
            fields: vec![
                GeneratedField {
                    name: "a".to_string(),
                    field_type: "String".to_string(),
                    comment: None,
                },
                GeneratedField {
                    name: "a".to_string(),
                    field_type: "i64".to_string(),
                    comment: None,
                },
            ],
            comment: None,
        };

        context.insert("name", &s.name);
        context.insert("fields", &s.fields);
        //dbg!(tera.render("test", &context).unwrap());
        assert_eq!(
            tera.render("test", &context).unwrap(),
            r#"#[derive(Debug)]
pub struct name {
    a: String,
    a: i64,
}"#
        );
    }
}
