/// Character impl
/// http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm
use super::*;

#[derive(Debug)]
enum Character {
    C,
    //:= left some else
}

impl Character {
    fn new() -> Self {
        Self::C
    }

    fn to_string(v: impl ToString) -> String {
        v.to_string()
    }
}

impl Tilde for Character {
    fn from_buf(c: &[u8]) -> Result<Self, String> {
        Ok(Self::C)
    }
}
