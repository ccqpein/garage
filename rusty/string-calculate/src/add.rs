use std::fmt;
use std::ops::Add;

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

impl Add for QStr {
    type Output = QStr;
    fn add(self, other: Self) -> QStr {
        let (mut a, mut result) = (0, vec![]);

        addition_(
            self.inner.as_ref().as_ref(),
            other.inner.as_ref().as_ref(),
            &mut a,
            &mut result,
        );

        QStr {
            inner: Box::new(result.into_iter().rev().collect::<Vec<_>>()),
        }
    }
}

fn addition_(num1: &[u8], num2: &[u8], a: &mut u8, result: &mut Vec<u8>) {
    match (num1, num2) {
        (e @ [], [n, tail @ ..]) | ([n, tail @ ..], e @ []) => {
            let this = *n + *a;
            if this >= 10 {
                result.push(this % 10);
                *a = 1;
                addition_(e, tail, a, result)
            } else {
                result.push(this);
                result.extend_from_slice(tail);
                return;
            }
        }
        ([n1, tail1 @ ..], [n2, tail2 @ ..]) => {
            let this = *n1 + *n2 + *a;
            if this >= 10 {
                result.push(this % 10);
                *a = 1;
                addition_(tail1, tail2, a, result)
            } else {
                result.push(this);
                *a = 0;
                addition_(tail1, tail2, a, result)
            }
        }
        ([], []) => {
            if *a != 0 {
                result.push(*a);
            }
            return;
        }
    }
}
