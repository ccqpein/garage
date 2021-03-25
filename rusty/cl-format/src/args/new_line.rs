use std::iter;

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
