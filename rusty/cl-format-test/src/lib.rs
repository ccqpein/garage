use cl_format_macros::*;
use std::cell::RefCell;
use std::fmt::Debug;

// trait TildeAble {
//     fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa>;

//     fn into_tildekind_loop(&self) -> Option<&dyn TildeKindLoop>;
//     fn into_tildekind_cond(&self) -> Option<&dyn TildeKindCond>;
// }

// trait TildeKindVa {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>>;
// }

// trait TildeKindLoop {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>>;
// }

// trait TildeKindCond {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>>;
// }

// impl TildeAble for Vec<&dyn TildeAble> {
//     fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
//         todo!()
//     }

//     fn into_tildekind_loop(&self) -> Option<&dyn TildeKindLoop> {
//         todo!()
//     }

//     fn into_tildekind_cond(&self) -> Option<&dyn TildeKindCond> {
//         todo!()
//     }
// }

// impl TildeAble for RefCell<Vec<&dyn TildeAble>> {
//     fn into_tildekind_loop(&self) -> Option<&dyn TildeKindLoop> {
//         Some(self)
//     }

//     fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
//         Some(self.borrow().get(0).unwrap().into_tildekind_va().unwrap())
//     }

//     fn into_tildekind_cond(&self) -> Option<&dyn TildeKindCond> {
//         todo!()
//     }
// }

// #[derive(Debug, PartialEq, Clone)]
// pub enum TildeLoopKind {
//     Nil, // ~{~}
//     At,  // ~@{~}
// }

// pub enum TildeKind {
//     Va,

//     Loop((Vec<Tilde>, TildeLoopKind)),

//     VecTilde(Vec<Tilde>),
// }

// impl TildeKind {
//     pub fn match_reveal(
//         &self,
//         //arg: &RefCell<Vec<&dyn TildeAble>>,
//         arg: &dyn TildeAble,
//     ) -> Result<String, Box<dyn std::error::Error>> {
//         match self {
//             TildeKind::Va => {
//                 let a = arg.into_tildekind_va().unwrap();
//                 //let a = a.into_tildekind_va().unwrap();
//                 return a.format(self);
//             }
//             TildeKind::Loop(_) => {
//                 let a = arg.into_tildekind_loop().unwrap();
//                 return a.format(self);
//             }
//             TildeKind::VecTilde(_) => todo!(),
//         }
//     }
// }

// impl TildeKindVa for i64 {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
//         todo!()
//     }
// }

// impl TildeKindLoop for Vec<&dyn TildeAble> {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
//         todo!()
//     }
// }

// impl TildeKindLoop for RefCell<Vec<&dyn TildeAble>> {
//     fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
//         let a = self.borrow_mut();
//         let a = a.get(0).unwrap();
//         tkind.match_reveal(*a)
//     }
// }

// pub struct Tilde {
//     len: usize,
//     value: TildeKind,
// }

// impl Tilde {
//     pub fn reveal_args_2(
//         &self,
//         args: &RefCell<Vec<&dyn TildeAble>>,
//     ) -> Result<String, Box<dyn std::error::Error>> {
//         self.value.match_reveal(args);

//         Ok(String::new())
//     }
// }

#[derive(Debug)]
pub struct TildeError {
    kind: ErrorKind,
    msg: String,
}

impl TildeError {
    fn new(kind: ErrorKind, msg: impl AsRef<str>) -> Self {
        Self {
            kind,
            msg: msg.as_ref().to_string(),
        }
    }
}

impl std::error::Error for TildeError {}

impl std::fmt::Display for TildeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TilderError {:?}: {}", self.kind, self.msg)
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    ParseError,
    RevealError,
    EmptyImplenmentError,
    FormatError,
}

#[derive(Debug, PartialEq, TildeAble, Clone)]
pub enum TildeKind {
    /// ~C ~:C
    #[implTo(char)]
    Char,
}

// cannot overwrote implement

// impl<T> TildeKindChar for T {
//     fn format(&self, tkind: &TildeKind, buf: &mut String) -> Result<(), TildeError> {
//         Err(TildeError::new(ErrorKind::EmptyImplenmentError, "haven't implenmented yet").into())
//     }
// }

impl TildeKindChar for char {
    fn format(&self, tkind: &TildeKind, buf: &mut String) -> Result<(), TildeError> {
        Err(TildeError::new(ErrorKind::EmptyImplenmentError, "haven't implenmented yet").into())
        //Ok(())
    }
}

impl TildeKindChar for char {
    fn format(&self, tkind: &TildeKind, buf: &mut String) -> Result<(), TildeError> {
        //Err(TildeError::new(ErrorKind::EmptyImplenmentError, "haven't implenmented yet").into())
        Ok(())
    }
}

////////// test super trait

struct A {}

impl A {
    fn function_inside() {}
}

///////////

pub trait TildeAble2: TildeKindChar {}

impl<T: TildeKindChar> TildeAble2 for T {}

//impl<T: TildeKindChar> TildeAble2 for T {}

pub fn v2_test<T: TildeAble2>(t: T, tkind: &TildeKind, buf: &mut String) {
    <T as TildeKindChar>::format(&t, tkind, buf);
}
