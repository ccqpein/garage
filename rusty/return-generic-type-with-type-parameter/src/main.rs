use std::io::Cursor;

struct AA;

impl AA {
    // https://users.rust-lang.org/t/how-can-i-return-a-generic-type/40536/4
    // fn return_func<F>(c: &mut Cursor<&'_ str>) -> Result<F, Box<(dyn std::error::Error + 'static)>>
    // where
    //     F: for<'a, 'b> Fn(
    //         &'a mut std::io::Cursor<&'b str>,
    //     ) -> Result<Self, Box<(dyn std::error::Error + 'static)>>,
    // {
    //     Ok(Self::aa)
    // }

    fn return_func(
        _: &mut Cursor<&'_ str>,
    ) -> Result<
        impl for<'a, 'b> Fn(
            &'a mut std::io::Cursor<&'b str>,
        ) -> Result<Self, Box<(dyn std::error::Error + 'static)>>,
        Box<(dyn std::error::Error + 'static)>,
    > {
        Ok(Self::aa)
    }

    fn aa(_: &mut Cursor<&'_ str>) -> Result<Self, Box<(dyn std::error::Error + 'static)>> {
        Ok(AA)
    }
}

fn main() {}
