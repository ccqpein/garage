use std::fmt::Display;

use crate::{CarryableUnit, CarryableUnitMut};

#[derive(Debug)]
pub struct Hex(u8);

const HEX_TABLE_VIEW: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

const FROM_ERR: &'static str = "cannot transfer to Hex";

impl TryFrom<u8> for Hex {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > 15 {
            Err(FROM_ERR)
        }else {
            Ok(HEX_TABLE_VIEW[value as usize])
        }
    }
}

impl Display for Hex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", HEX_TABLE_VIEW[self.0 as usize])
    }
}

impl CarryableUnit for Hex {
    type Item = (Self, bool);

    fn next(&self) -> Self::Item {
        if self.0 == 15 {
            (Self(0), true)
        } else {
            (Self(self.0 + 1), false)
        }
    }
}



impl CarryableUnitMut for Hex {
    type ItemMut = bool;

    fn next_mut(&mut self) -> Self::ItemMut {
        if self.0 == 15 {
            self.0 = 0;
            true
        } else {
            self.0 += 1;
            false
        }
    }
}
