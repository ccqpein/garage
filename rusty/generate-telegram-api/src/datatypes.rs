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

// struct PollAnswer {}

// struct Poll {}

// struct PreCheckoutQuery {}

// struct ShippingQuery {}

// struct Message {}

// struct InlineQuery {}

// struct ChosenInlineResult {}

// struct CallbackQuery {}

// struct Update {}

struct Data {
    name: String,
    doc: String,
    fields: Vec<String>,
    types: Vec<String>,
    descriptions: Vec<String>,
}

struct Method {
    name: String,
    doc: String,
    parameters: Vec<String>,
    types: Vec<String>,
    requireds: Vec<bool>,
    descriptions: Vec<String>,
}
