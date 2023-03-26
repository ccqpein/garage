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

fn arg_check(a: &dyn tildes::TildeAble) -> &dyn tildes::TildeAble {
    a
}

macro_rules! test {
    (&[$($arg:expr),*]) => {
        &[$(test!($arg)),*].to_vec();
    };
	($arg:expr) => {
        let a = $arg;
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
#[macro_export]
macro_rules! cl_format {
    ($control_str:expr, $($ele:expr),*) =>	{
		{
			let c = control_str::ControlStr::from($control_str)?;
			let a = Into::<tildes::Args<'_>>::into([$(args!($ele)),*]);
			c.reveal(a)
		}
	}

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_args_macro_expand() -> Result<(), Box<dyn std::error::Error>> {
        test!(&[&1, 2, 3, &[4, 5]]);
        args!(&1);
        args!(&2);
        args!(&[&1, args!(&[&5, &6])]);
        args!(&[&1, &[&5, &6]]);
        args!(&[&5, &6]);
        Ok(())
    }

    #[test]
    fn test_macro_expand() -> Result<(), Box<dyn std::error::Error>> {
        let a = cl_format!("~a, ~a, ~a", &1, &2, &3);
        dbg!(a);

        let a = cl_format!("~a, ~a, ~a", &1, args!(&[&2]), &3);
        dbg!(a);

        Ok(())
    }
}
