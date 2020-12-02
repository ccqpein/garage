macro_rules! generate_struct {
    ($($field:ident, $ty:ty),*) => {
        #[derive(Debug)]
        struct Test {
            $($field:$ty),*
        }
    };
}

generate_struct!(a, String, b, usize);

fn main() {
    //generate_test!(a, String, b, usize);
    let a = Test {
        a: String::new(),
        b: 0,
    };

    println!("{}, {}", a.a, a.b);
    dbg!(a);
}
