use std::iter;
use std::str::FromStr;

struct NewLine(usize);

// impl FromStr for NewLine {
//     type Err = String;
//     fn from_str(s: &str) -> Result<Self, Self::Err> {
//         match s {
//             "~%" => Ok(Self(1)),
//             _ =>
//         }
//     }
// }

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
}
