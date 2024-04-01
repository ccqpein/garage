use crate::{CarryableUnit, CarryableUnitMut};

#[doc = r"alphabet low cases, from a-z"]
#[derive(Debug)]
pub struct AlphaBetLowCase(u8);

impl AlphaBetLowCase {
    pub fn byte(&self) -> &u8 {
        &self.0
    }
}

impl TryFrom<u8> for AlphaBetLowCase {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < 97 || value > 122 {
            Err("not in alphabet".to_string())
        } else {
            Ok(AlphaBetLowCase(value))
        }
    }
}

impl CarryableUnit for AlphaBetLowCase {
    type Item = (AlphaBetLowCase, bool);

    fn next(&self) -> Self::Item {
        if self.0 == 122 {
            (Self(97), true)
        } else {
            (Self(self.0 + 1), false)
        }
    }
}

impl CarryableUnitMut for AlphaBetLowCase {
    type ItemMut = bool;

    fn next_mut(&mut self) -> Self::ItemMut {
        if self.0 == 122 {
            self.0 = 97;
            true
        } else {
            self.0 += 1;
            false
        }
    }
}

#[doc = r"alphabet upper cases, from A-Z"]
#[derive(Debug)]
pub struct AlphaBetUpperCase(u8);

impl AlphaBetUpperCase {
    pub fn byte(&self) -> &u8 {
        &self.0
    }
}

impl TryFrom<u8> for AlphaBetUpperCase {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value < 65 || value > 90 {
            Err("not in alphabet".to_string())
        } else {
            Ok(AlphaBetUpperCase(value))
        }
    }
}

impl CarryableUnit for AlphaBetUpperCase {
    type Item = (AlphaBetUpperCase, bool);

    fn next(&self) -> Self::Item {
        if self.0 == 90 {
            (Self(65), true)
        } else {
            (Self(self.0 + 1), false)
        }
    }
}

impl CarryableUnitMut for AlphaBetUpperCase {
    type ItemMut = bool;

    fn next_mut(&mut self) -> Self::ItemMut {
        if self.0 == 90 {
            self.0 = 65;
            true
        } else {
            self.0 += 1;
            false
        }
    }
}
