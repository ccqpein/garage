use serde::{Deserialize, Serialize};
use serde_json::Result;

pub trait Tableable {
    type Item;
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>);
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Data {
    pub name: String,
    pub doc: String,
    pub fields: Vec<String>,
    pub types: Vec<String>,
    pub descriptions: Vec<String>,
}

impl Data {
    pub fn clear(&mut self) {
        self.name.clear();
        self.doc.clear();
        self.fields.clear();
        self.types.clear();
        self.descriptions.clear();
    }

    pub fn to_string(&self) -> Result<String> {
        serde_json::to_string(self)
    }
}

impl Tableable for &mut Data {
    type Item = Vec<String>;
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>) {
        for x in d {
            self.fields.push(x[0].clone());
            self.types.push(x[1].clone());
            self.descriptions.push(x[2].clone());
        }
    }
}

#[derive(Default, Debug, Clone, Serialize, Deserialize)]
pub struct Method {
    pub name: String,
    pub doc: String,
    pub parameters: Vec<String>,
    pub types: Vec<String>,
    pub requireds: Vec<String>,
    pub descriptions: Vec<String>,
}

impl Method {
    pub fn clear(&mut self) {
        self.name.clear();
        self.doc.clear();
        self.parameters.clear();
        self.types.clear();
        self.requireds.clear();
        self.descriptions.clear();
    }

    pub fn to_string(&self) -> Result<String> {
        serde_json::to_string(self)
    }
}

impl<'a> Tableable for &'a mut Method {
    type Item = Vec<String>;
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>) {
        for x in d {
            self.parameters.push(x[0].clone());
            self.types.push(x[1].clone());
            self.requireds.push(x[2].clone());
            self.descriptions.push(x[3].clone());
        }
    }
}
