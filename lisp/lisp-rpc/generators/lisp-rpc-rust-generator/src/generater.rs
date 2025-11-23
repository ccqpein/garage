use super::*;
use serde::Serialize;
use tera::Context;

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct GeneratedField {
    pub name: String,
    pub field_type: String,
    pub comment: Option<String>,

    /// the original keyword name
    key_name: String,
}

impl GeneratedField {
    pub fn new(key_name: &str, field_type: &str, comment: Option<String>) -> Self {
        Self {
            name: kebab_to_snake_case(key_name),
            field_type: type_translate(field_type),
            comment,

            key_name: key_name.to_string(),
        }
    }
}

/// the GeneratedStruct is the middle layer between render and rpc spec
#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct GeneratedStruct {
    pub name: String,
    pub derived_traits: Option<Vec<String>>,
    pub fields: Vec<GeneratedField>,
    pub comment: Option<String>,

    /// the original data name
    data_name: String,
}

impl GeneratedStruct {
    pub fn new(
        data_name: &str,
        derived_traits: Option<Vec<String>>,
        fields: Vec<GeneratedField>,
        comment: Option<String>,
    ) -> Self {
        Self {
            name: kebab_to_pascal_case(data_name),
            derived_traits,
            fields,
            comment,

            data_name: data_name.to_string(),
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
        let temp = include_str!("../templates/def_struct.rs.template");
        let mut tera = Tera::default();
        let mut context = Context::new();

        //dbg!(temp);
        tera.add_raw_template("test", temp).unwrap();

        let s = GeneratedStruct {
            name: "name".to_string(),
            derived_traits: None,
            fields: vec![
                GeneratedField::new("a", "string", None),
                GeneratedField::new("a", "number", None),
            ],
            comment: None,
            data_name: "name".to_string(),
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
