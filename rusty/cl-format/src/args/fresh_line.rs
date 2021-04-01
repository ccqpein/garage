use super::*;
use std::iter;
use std::str;

#[derive(Debug, Default)]
struct FreshLine(usize);

impl FreshLine {
    pub fn new() -> Self {
        Default::default()
    }

    fn to_string(&self) -> String {
        if self.0 == 0 {
            return String::new();
        }

        iter::repeat("\n".to_string())
            .take(self.0 - 1)
            .fold(String::new(), |mut i, j| {
                i.push_str(&j);
                i
            })
    }
}

impl Tilde for FreshLine {
    fn from_buf(c: &[u8]) -> Result<Self, String> {
        Ok(Self(
            str::from_utf8(&c[1..c.len() - 1])
                .map_err(|e| e.to_string())?
                .parse::<usize>()
                .map_err(|e| e.to_string())?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fresh_line_from_buf() {}
}
