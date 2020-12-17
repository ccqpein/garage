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

#[derive(Default, Debug)]
pub struct Method {
    pub name: String,
    pub doc: String,
    pub parameters: Vec<String>,
    pub types: Vec<String>,
    pub requireds: Vec<bool>,
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
