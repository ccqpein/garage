use crate::Tilde;
use std::io::Cursor;
use std::iter;
use std::str;
use std::str::FromStr;

#[derive(Debug)]
struct NewLine(usize);

impl NewLine {
    fn to_string(&self) -> String {
        iter::repeat("\n".to_string())
            .take(self.0)
            .fold(String::new(), |mut i, j| {
                i.push_str(&j);
                i
            })
    }
}

impl Tilde for NewLine {
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
    fn test_newline_to_string() {
        let a = NewLine(3);

        assert_eq!(
            a.to_string(),
            r#"


"#
        )
    }

    #[test]
    fn test_from_buf() {
        let testcase0 = "~2%".as_bytes();
        assert_eq!(
            NewLine::from_buf(testcase0).unwrap().to_string(),
            r#"

"#
        )
    }
}
