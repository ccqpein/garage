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

impl<T: CarryableUnitMut> VecAlign<T> {
    pub fn inner(&self) -> &Vec<T> {
        &self.inner
    }

    pub fn inner_mut(&mut self) -> &mut Vec<T> {
        &mut self.inner
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
