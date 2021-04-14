use dependent_type::*;

fn main() {
    let n = NatureNum::<3> {};
    assert_eq!(3, b(n));
}
