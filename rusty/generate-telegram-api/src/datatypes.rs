pub trait Tableable {
    type Item;
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>);
}

enum DataType {
    Integer,
    Message,
    InlineQuery,
    ChosenInlineResult,
    CallbackQuery,
    ShippingQuery,
    PreCheckoutQuery,
    Poll,
    PollAnswer,
    Update,
}

#[derive(Default, Debug)]
pub struct Data {
    pub name: String,
    pub doc: String,
    pub fields: Vec<String>,
    pub types: Vec<String>,
    pub descriptions: Vec<String>,
}

impl Data {
    fn clear(&mut self) {
        self.name.clear();
        self.doc.clear();
        self.fields.clear();
        self.types.clear();
        self.descriptions.clear();
    }
}

impl Tableable for &mut Data {
    type Item = (String, String, String);
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>) {
        for (x, y, z) in d {
            self.fields.push(x);
            self.types.push(y);
            self.descriptions.push(z);
        }
    }
}

#[derive(Default, Debug)]
pub struct Method {
    pub name: String,
    pub doc: String,
    pub parameters: Vec<String>,
    pub types: Vec<String>,
    pub requireds: Vec<String>,
    pub descriptions: Vec<String>,
}

impl Method {
    fn clear(&mut self) {
        self.name.clear();
        self.doc.clear();
        self.parameters.clear();
        self.types.clear();
        self.requireds.clear();
        self.descriptions.clear();
    }
}

impl Tableable for &mut Method {
    type Item = (String, String, String, String);
    fn fill_from_table(&mut self, d: impl Iterator<Item = Self::Item>) {
        for (a, b, c, d) in d {
            self.parameters.push(a);
            self.types.push(b);
            self.requireds.push(c);
            self.descriptions.push(d);
        }
    }
}
