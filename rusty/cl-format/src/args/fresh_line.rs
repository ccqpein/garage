use std::iter;

struct FreshLine(usize);

impl FreshLine {
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
