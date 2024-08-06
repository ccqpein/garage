use std::fmt::Display;

use cl_format::TildeAble;

use super::*;

#[doc = r"Hex from 0 to F"]
#[derive(Debug)]
pub struct Hex(u8);

impl Hex {
    pub fn new<T>(x: T) -> Result<Self, T::Error>
    where
        T: TryInto<Self>,
    {
        x.try_into()
    }
}

const HEX_TABLE_VIEW: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
];

const FROM_ERR: &'static str = "cannot transfer to Hex";

impl TryFrom<u8> for Hex {
    type Error = &'static str;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if value > 15 {
            Err(FROM_ERR)
        } else {
            Ok(Hex(value))
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

impl TildeAble for Hex {
    fn len(&self) -> usize {
        1
    }

    fn into_tildekind_char(&self) -> Option<&dyn cl_format::TildeKindChar> {
        Some(&HEX_TABLE_VIEW[self.0 as usize])
    }

    fn into_tildekind_va(&self) -> Option<&dyn cl_format::TildeKindVa> {
        Some(&HEX_TABLE_VIEW[self.0 as usize])
    }
}
