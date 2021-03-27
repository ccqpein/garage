/// Character impl
/// http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm
use std::str::FromStr;

enum Character {
    C,
}

impl FromStr for Character {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "~C" => Ok(Self::C),
            _ => Err("not support".to_string()),
        }
    }
}

impl Character {
    fn to_string(v: impl ToString) -> String {
        v.to_string()
    }
}
