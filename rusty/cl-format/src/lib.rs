#![feature(let_chains)]
#![feature(pattern)]
#![feature(rustc_attrs)]

mod control_str;
mod tildes;

/// multi_tilde_impl!(TildeKindVa, [float, char, String], {Err("un-implenmented yet".into())})
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
    ($implName:ident, [$($y:ident),+], $body:block) => {
		$(
			impl $implName for $y {
				fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>>
					$body

			}
		)+
    };
}

//multi_tilde_impl!(TildeKindVa, [usize, i32], { println!("") });
