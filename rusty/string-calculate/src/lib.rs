mod add;
mod mul;

use std::fmt;

pub use add::*;

#[derive(Debug)]
pub struct QStr {
    inner: Box<Vec<u8>>,
}

impl fmt::Display for QStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .as_ref()
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

impl QStr {
    pub fn new(s: &str) -> Self {
        let a = (0..s.len())
            .into_iter()
            .map(|ind| &s[ind..ind + 1])
            .map(|s| s.parse().unwrap())
            .rev()
            .collect::<Vec<u8>>();

        Self { inner: Box::new(a) }
    }
}
