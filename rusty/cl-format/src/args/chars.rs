/// Character impl
/// http://www.lispworks.com/documentation/HyperSpec/Body/22_caa.htm

enum Character {
    C,
    //:= TODO: maybe more
}

impl Character {
    fn to_string(v: impl ToString) -> String {
        v.to_string()
    }
}
