use super::QStr;
use std::ops::Add;

impl Add for QStr {
    type Output = QStr;
    fn add(self, other: Self) -> QStr {
        let result = addition(self.inner.as_ref().as_ref(), other.inner.as_ref().as_ref());

        QStr {
            inner: Box::new(result.into_iter().collect::<Vec<_>>()),
        }
    }
}

pub(crate) fn addition(num1: &[u8], num2: &[u8]) -> Vec<u8> {
    let mut result = vec![];
    let mut a = 0;
    addition_(num1, num2, &mut a, &mut result);
    result
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_addition() {
        assert_eq!(vec![9, 7, 5], addition(&[3, 2, 1], &[6, 5, 4]));
        assert_eq!(vec![5, 5, 2, 1], addition(&[6, 4, 3], &[9, 0, 9]));
    }
}
