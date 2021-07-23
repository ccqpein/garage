mod add;
mod div;
mod mul;

use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct QStr {
    inner: Vec<u8>,
}

impl fmt::Display for QStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.inner
                .iter()
                .rev()
                .map(|d| d.to_string())
                .collect::<Vec<String>>()
                .join("")
        )
    }
}

impl QStr {
    pub fn new(s: &str) -> Result<Self, String> {
        let a = (0..s.len())
            .into_iter()
            .map(|ind| &s[ind..ind + 1])
            .map(|s| s.parse());

        let mut cache: Vec<u8> = vec![];

        for aa in a {
            if !aa.is_ok() {
                return Err("Chars have to be number".to_string());
            } else {
                cache.push(aa.unwrap())
            }
        }

        cache.reverse();

        Ok(Self { inner: cache })
    }
}
