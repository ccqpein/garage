use std::fmt::Debug;

mod alphabet;
mod hex;

pub trait CarryableUnit {
    type Item: CarryResult;
    fn next(&self) -> Self::Item;
}

pub trait CarryableUnitMut: CarryableUnit {
    type ItemMut: CarryResult;
    fn next_mut(&mut self) -> Self::ItemMut;
}

pub trait CarryResult: Debug {
    fn carried(&self) -> bool;
}

impl CarryResult for bool {
    fn carried(&self) -> bool {
        *self
    }
}

impl<T: CarryableUnit + Debug> CarryResult for (T, bool) {
    fn carried(&self) -> bool {
        self.1
    }
}
