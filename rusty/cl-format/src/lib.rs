#![feature(let_chains)]
#![feature(pattern)]
#![feature(rustc_attrs)]

mod control_str;
mod tildes;

/// multi_tilde_impl!(TildeKindVa, [float, char, String], self, {Err("un-implenmented yet".into())})
///
/// will expand
///
/// impl TildeKindVa for float{
///     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
///         Err("un-implenmented yet".into())
///     }
/// }
/// ...
#[macro_export]
macro_rules! multi_tilde_impl {
    ($implName:ident, [$($y:ident),+], $s:ident,$body:block) => {
		$(
			impl $implName for $y {
				fn format(&$s, tkind: &TildeKind) -> Result<Option<String>, Box<dyn std::error::Error>>
					$body

			}
		)+
    };
}

//multi_tilde_impl!(TildeKindVa, [usize, i32], self, { println!("test") });

//:= TODO: need the proc macro
/// args!
/// args!(&a) => &a as &dyn tildes::TildeAble
///
/// args!(&[&a, &b]) =>
///     &(Args::new([args!(&a), args!(&b)]) =>
///     &(Args::new([&a as &dyn tildes::TildeAble, &b as &dyn tildes::TildeAble])) as &dyn tildes::TildeAble
#[macro_export]
macro_rules! args {
	(&[$($arg:expr),*]) => {
        &tildes::Args::new([$(args!($arg)),*].to_vec()) as &dyn tildes::TildeAble
    };
    ($arg:expr) => {
        $arg as &dyn tildes::TildeAble
    };
}

/// cl_format! should like vec! macro
///
/// cl_format!(control_str, &a, &b, &c) => {
///      let c = control_str::ControlStr::from("~a, ~a, ~a")?;
///      let a = Into::<
///             tildes::Args<'_>,
///         >::into([
///             &1 as &dyn tildes::TildeAble,
///             &2 as &dyn tildes::TildeAble,
///             &3 as &dyn tildes::TildeAble,
///         ]);
///         c.reveal(a)
/// }
// #[macro_export]
// macro_rules! cl_format {
//     ($control_str:expr, $($ele:expr),*) =>	{
// 		{
// 			let c = control_str::ControlStr::from($control_str)?;
// 			let a = Into::<tildes::Args<'_>>::into([$(args!($ele)),*]);
// 			c.reveal(a)
// 		}
// 	}

// }

// fn iter_to_args<'a, T: 'a + tildes::TildeAble>(a: impl Iterator<Item = &'a T>) -> tildes::Args<'a> {
//     a.map(|v| v as &'a dyn tildes::TildeAble)
//         .collect::<Vec<_>>()
//         .into()
// }

#[cfg(test)]
mod tests {
    use std::convert::TryInto;

    use crate::tildes::Args;

    use super::*;
    use cl_format_macros::*;

    #[test]
    fn test_args_macro_expand() -> Result<(), Box<dyn std::error::Error>> {
        args!(&1);
        args!(&2);
        args!(&[&1, args!(&[&5, &6])]);
        args!(&[&5, &6]);
        Ok(())
    }

    #[test]
    fn test_macro_expand() -> Result<(), Box<dyn std::error::Error>> {
        let a = cl_format!("~a, ~a, ~a", &1_i32, &2, &3);
        dbg!(a);

        let s = String::from("abc");
        let a = cl_format!("~a, ~a, ~a, ~S", &1_i32, &2, &3, &s);
        dbg!(a);

        let a = cl_format!("~a, ~a, ~a, ~a, here", &1_i32, &2, &3, &s);
        dbg!(a);

        let ll: Vec<i32> = vec![1, 2, 3];
        let ll = Into::<tildes::Args<'_>>::into(ll.as_slice());
        //let ll = iter_to_args(ll.iter());

        let a = cl_format!("~a, ~a, ~a, ~{~a,~}", &1_i32, &2, &3, &ll);

        dbg!(a);

        //
        //
        // let x = Into::<tildes::Args<'_>>::into([
        //     &1 as &dyn tildes::TildeAble,
        //     &2 as &dyn tildes::TildeAble,
        //     &Into::<tildes::Args<'_>>::into([
        //         &3 as &dyn tildes::TildeAble,
        //         &4 as &dyn tildes::TildeAble,
        //     ]) as &dyn tildes::TildeAble,
        //     &Into::<tildes::Args<'_>>::into([&Into::<tildes::Args<'_>>::into([
        //         &5 as &dyn tildes::TildeAble
        //     ]) as &dyn tildes::TildeAble]) as &dyn tildes::TildeAble,
        // ]);
        // dbg!(x);

        Ok(())
    }

    #[test]
    fn test_cl_format_macro() -> Result<(), Box<dyn std::error::Error>> {
        let var_name = cl_format!("abc", &1, &4, &3);
        let a = var_name;
        dbg!(a);

        let s = "ss";
        //cl_format!(s, &1, &3, [[&3]]);

        //let i = 3;
        //cl_format!(s, &1, &3, [[&3]]);

        // Into::<tildes::Args<'_>>::into([
        //     &1 as &dyn tildes::TildeAble,
        //     &3 as &dyn tildes::TildeAble,
        //     Into::<tildes::Args<'_>>::into([Into::<tildes::Args<'_>>::into([
        //         &3 as &dyn tildes::TildeAble
        //     ]) as &dyn tildes::TildeAble]) as &dyn tildes::TildeAble,
        // ]);

        //cl_format!(&s, &1, &a, [[&3]]);
        Ok(())
    }
}
