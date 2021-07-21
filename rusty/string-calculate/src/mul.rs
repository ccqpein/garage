use super::{add::*, *};
use std::ops::Mul;

impl Mul for QStr {
    type Output = Self;
    fn mul(self, other: Self) -> <Self as Mul>::Output {
        let result = mul(self.inner.as_ref().as_ref(), other.inner.as_ref().as_ref());
        QStr {
            inner: Box::new(result.into_iter().collect::<Vec<_>>()),
        }
    }
}

/// num1 * num2
fn mul(num1: &[u8], num2: &[u8]) -> Vec<u8> {
    let mut result = vec![];

    for (ind, n) in num2.iter().enumerate() {
        let mut cache = vec![0; ind];
        mul_single(num1, n, &mut cache);
        result.push(cache);
    }

    result
        .into_iter()
        .reduce(|a, b| addition(&a, &b))
        .unwrap()
        .clone()
}

fn mul_single(num1: &[u8], num2: &u8, result: &mut Vec<u8>) {
    let mut add = 0;

    for n1 in num1 {
        let mut a = num2 * n1;
        a += add;
        if a >= 10 {
            result.push(a % 10);
            add = a / 10;
        } else {
            add = 0;
            result.push(a);
        }
    }

    if add != 0 {
        result.push(add);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_mul_single() {
        let mut result = vec![];
        mul_single(&[3, 2, 1], &9, &mut result);
        assert_eq!(&result, &[7, 0, 1, 1].to_vec())
    }

    #[test]
    fn test_mul() {
        assert_eq!(mul(&[3, 2, 1], &[6, 5, 4]), vec![8, 8, 0, 6, 5]);
    }
}
