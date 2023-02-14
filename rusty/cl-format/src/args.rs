//mod chars;
//mod fresh_line;
//mod new_line;

//pub use chars::*;
//pub use fresh_line::*;
//pub use new_line::*;

//use args::*;
//use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};

// pub trait Tilde: Debug {
//     fn from_buf(c: &[u8]) -> Result<Self, String>
//     where
//         Self: Sized;
// }

// lazy_static! {
//     //static ref newl: Box<(dyn Tilde + Sync)> = Box::new(NewLine::new());
//     pub static ref tilde_table: HashMap<u8, NewLine> = {
//         let mut t = HashMap::new();
//         t.insert(b'%', NewLine::new());
//         t
//     };
// }

enum Tilde {
    /// ~C or ~:C
    Char,

    /// loop
    Loop(Vec<Tilde>),

    /// text inside the tilde
    Text(String),

    /// vec
    VecTilde(Vec<Tilde>),
}

impl Tilde {
    fn parse(mut c: &mut Cursor<&'_ str>) -> Self {
        let mut result = vec![];
        let mut char_buf = [0u8; 1];
        loop {
            c.read(&mut char_buf);
            if char_buf[0] == b'~' {
                c.read(&mut char_buf);
                match char_buf[0] {
                    b'{' => result.push(Self::parse_loop(c)),
                    _ => todo!(),
                }
            } else {
            }
        }

        Self::VecTilde(result)
    }

    fn parse_loop(mut c: &mut Cursor<&'_ str>) -> Self {
        todo!()
    }
}

// #[derive(Debug)]
// enum ParseData {
//     B(Vec<u8>),
//     T(Box<dyn Tilde>),
// }

/*:= implenment notes:
use cursor
 */

/// the control string should including:
/// 1. the whole string
/// 2. the parsed tree
struct ControlStr<'a> {
    inner: &'a str,
    tlides: Vec<((usize, usize), Tilde)>,
}

impl<'a> ControlStr<'a> {
    fn new(s: &'a str) -> Self {
        //let mut cc = Cursor::new(s);

        Self {
            inner: s,
            tlides: vec![],
        }
    }

    fn scan(s: &'a str) -> Vec<((usize, usize), Tilde)> {
        todo!()
    }

    // fn parse(&self) -> Result<Vec<ParseData>, String> {
    //     let mut cursor = Cursor::new(self.inner);
    //     let mut result = vec![];
    //     loop {
    //         match self.scan(&mut cursor)? {
    //             Some(d) => result.push(d),
    //             None => return Ok(result),
    //         }
    //     }
    // }

    //     fn scan(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
    //         match self.until_tilde(c)? {
    //             a @ Some(_) => return Ok(a),
    //             None => (), // maybe start with ~
    //         }

    //         match self.read_next_tilde(c)? {
    //             None => Ok(None),
    //             a @ Some(_) => return Ok(a),
    //         }
    //     }

    //     fn until_tilde(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
    //         let mut buf = vec![];
    //         let i = c.read_until(b'~', &mut buf).map_err(|e| e.to_string())?;

    //         // start with '~' is other parser's job
    //         if i == 1 {
    //             c.seek(SeekFrom::Current(-1));
    //             return Ok(None);
    //         }

    //         if let Some(lst) = buf.last() {
    //             if *lst == b'~' {
    //                 buf.pop();
    //                 c.seek(SeekFrom::Current(-1));
    //             } // else means it is the endding
    //         } else {
    //             return Ok(None);
    //         }

    //         Ok(Some(ParseData::B(buf)))
    //     }

    //     //:= TODO: just demo now
    //     fn read_next_tilde(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
    //         let mut buffer = vec![];
    //         let mut one_byte_buffer: [u8; 1] = [0; 1];
    //         let mut i = 0;

    //         let mut count = 0;
    //         loop {
    //             i = c.read(&mut one_byte_buffer).map_err(|e| e.to_string())?;
    //             if i == 0 {
    //                 break;
    //             }
    //             //:= TODO: more rule here
    //             count += 1;
    //             buffer.extend_from_slice(&one_byte_buffer);
    //             if count == 2 {
    //                 break;
    //             }
    //         }

    //         //:= TODO: parse to real data type here
    //         if buffer.len() == 0 {
    //             return Ok(None);
    //         }
    //         return Ok(Some(ParseData::B(buffer)));
    //     }
}

// #[cfg(test)]
// mod test {
//     use super::*;

//     #[test]
//     fn test_cursor() {
//         let mut testcase0 = Cursor::new("abcd");
//         assert_eq!(testcase0.position(), 0);

//         let mut buffer: [u8; 1] = [0; 1];
//         testcase0.set_position(2);
//         let _ = testcase0.read(&mut buffer);
//         assert_eq!(buffer[0], b'c');
//     }

//     //:= TODO: demo test
//     #[test]
//     fn test_parse() {
//         let testcase0 = ControlStr::new("aaa ~D~%");
//         dbg!(testcase0
//             .parse()
//             .unwrap()
//             .iter()
//             .map(|v| match v {
//                 ParseData::B(b) => {
//                     String::from_utf8(b.clone()).unwrap()
//                 }
//                 _ => {
//                     String::new()
//                 }
//             })
//             .collect::<Vec<String>>());
//     }
// }
