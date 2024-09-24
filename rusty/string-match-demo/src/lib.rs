#![doc = r#"

```rust
str-match! {
["value ", v, " goes to bot ", bot-num] => {
blablabla
},
_ => {}
}
```

will generate

static RE1: Regex = Regex::new(r"...").unwrap()


"#]

use std::{
    cell::{LazyCell, OnceCell},
    sync::LazyLock,
};

use regex::Regex;

fn target_of_macro_demo() {
    static RE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"value (?<v>)").unwrap());
}
