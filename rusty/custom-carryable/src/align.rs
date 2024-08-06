use std::fmt::Display;

use cl_format::{tilde, ControlStr, TildeAble, TildeKindLoop};

use crate::carryable_units::*;

#[derive(Clone, Copy, Debug)]
pub enum Orientation {
    ToLeft,
    ToRight,
    FromCenter,
}

pub trait Align<T: CarryableUnitMut> {
    fn index(&self) -> usize;

    fn get_mut(&mut self, index: usize) -> Option<&mut T>;

    fn orientation(&self) -> Orientation;

    fn next_on(&mut self, index: usize, ori: &Orientation) {
        let a = match self.get_mut(index) {
            Some(aa) => aa.next_mut(),
            None => return,
        };

        if a.carried() {
            match ori {
                Orientation::ToLeft if index != 0 => self.next_on(index - 1, ori),
                Orientation::ToLeft => (),
                Orientation::ToRight => self.next_on(index + 1, ori),
                Orientation::FromCenter => {
                    if index != 0 {
                        self.next_on(index - 1, ori)
                    }
                    self.next_on(index + 1, ori)
                }
            }
        }
    }

    fn next(&mut self) {
        self.next_on(self.index(), &self.orientation())
    }

    fn next_by(&mut self, steps: usize) {
        (0..steps).for_each(|_| self.next())
    }
}

pub trait IntoAlign<T: CarryableUnitMut> {
    type Alignor: Align<T>;
    type Error;
    fn into_align(self, org: Orientation) -> Result<Self::Alignor, Self::Error>;
}

#[derive(Debug)]
pub struct VecAlign<T: CarryableUnitMut> {
    inner: Vec<T>,
    this: usize,
    org: Orientation,
}

impl<T: CarryableUnitMut + Display> Display for VecAlign<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner.get(0) {
            Some(x) => write!(f, "[{}", x)?,
            None => return Ok(()),
        }

        for n in self.inner.iter().skip(1) {
            write!(f, ", {}", n)?;
        }
        write!(f, "]")
    }
}

impl<T: CarryableUnitMut> VecAlign<T> {
    pub fn inner(&self) -> &Vec<T> {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut Vec<T> {
        &mut self.inner
    }
}

impl<T: CarryableUnitMut + TildeAble> VecAlign<T> {
    pub fn into_tildeable(&self) -> Vec<&dyn TildeAble> {
        self.inner.iter().map(|a| tilde!(a)).collect()
    }
}

impl<T: CarryableUnitMut> Align<T> for VecAlign<T> {
    fn index(&self) -> usize {
        self.this
    }

    fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.inner.get_mut(index)
    }

    fn orientation(&self) -> Orientation {
        self.org
    }
}

impl<T: CarryableUnitMut> IntoAlign<T> for Vec<T> {
    type Alignor = VecAlign<T>;
    type Error = String;

    fn into_align(self, org: Orientation) -> Result<Self::Alignor, Self::Error> {
        let len = self.len();
        if len != 0 {
            Ok(VecAlign {
                inner: self,
                this: match org {
                    Orientation::ToLeft => len - 1,
                    Orientation::ToRight => 0,
                    Orientation::FromCenter => (len - 1) / 2,
                },
                org,
            })
        } else {
            Err("vector length is 0".into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cl_format::*;

    #[test]
    fn test_vecalign_1() {
        let mut a = vec![1, 2, 3, 4, 5]
            .into_iter()
            .map(|n| hex::Hex::new(n).unwrap())
            .collect::<Vec<_>>()
            .into_align(Orientation::ToRight)
            .unwrap();

        a.next_by(10);
        assert_eq!(format!("{}", a), "[B, 2, 3, 4, 5]".to_string());

        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "B,2,3,4,5".to_string()
        );

        a.next_by(4);
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "F,2,3,4,5".to_string()
        );

        a.next_by(5); //20 => 14 hex
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "4,3,3,4,5".to_string()
        );

        a.next_by(12); //32 => 20 hex
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "0,4,3,4,5".to_string()
        );
    }

    #[test]
    fn test_vecalign_2() {
        let mut a = vec![1, 2, 3, 4, 5]
            .into_iter()
            .map(|n| hex::Hex::new(n).unwrap())
            .collect::<Vec<_>>()
            .into_align(Orientation::ToLeft)
            .unwrap();

        a.next_by(10);
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "1,2,3,4,F".to_string()
        );

        a.next_by(2);
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "1,2,3,5,1".to_string()
        );
    }

    #[test]
    fn test_vecalign_3() {
        let mut a = vec![1, 2, 3, 4, 5]
            .into_iter()
            .map(|n| hex::Hex::new(n).unwrap())
            .collect::<Vec<_>>()
            .into_align(Orientation::FromCenter)
            .unwrap();

        a.next_by(12);
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "1,2,F,4,5".to_string()
        );

        a.next_by(2);
        let aa = a.into_tildeable();
        assert_eq!(
            cl_format!("~{~a~^,~}", &aa).unwrap(),
            "1,3,1,5,5".to_string()
        );
    }
}
