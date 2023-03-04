use std::fmt::Debug;
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};

use cl_format_macros::*;

#[derive(Debug)]
struct TildeError {
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
enum ErrorKind {
    ParseError,
    RevealError,
}

#[derive(Debug)]
struct TildeNull;

#[derive(Debug, PartialEq, TildeAble)]
pub enum TildeKind {
    /// ~C ~:C
    Char,

    /// ~$ ~5$ ~f
    Float(Option<String>),

    /// ~d ~:d ~:@d
    Digit(Option<String>),

    //:= TODO: ~S
    //:= TODO: ~C
    //:= TODO: ~X
    //:= TODO: ~O
    #[implTo(f32, char, String)]
    /// ~a
    Va,

    /// loop
    Loop(Vec<Tilde>),

    #[implTo(usize)]
    /// ~[ ~] condition
    Cond((Vec<Tilde>, bool)),

    /// text inside the tilde
    Text(String),

    /// vec
    VecTilde(Vec<Tilde>),
}

impl TildeKind {
    pub fn match_reveal(&self, arg: &dyn TildeAble) -> Result<String, Box<dyn std::error::Error>> {
        match self {
            TildeKind::Char => todo!(),
            TildeKind::Float(_) => todo!(),
            TildeKind::Digit(_) => todo!(),
            TildeKind::Va => {
                let a = arg.into_tildekind_va().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Va").into(),
                )?;

                return a.format(self);
            }
            TildeKind::Loop(_) => {
                let a = arg.into_tildekind_loop().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Loop").into(),
                )?;

                return a.format(self);
            }
            TildeKind::Text(s) => Ok(s.to_string()),
            TildeKind::VecTilde(_) => todo!(),
            TildeKind::Cond(_) => {
                let a = arg.into_tildekind_cond().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Loop").into(),
                )?;
                return a.format(self);
            }
        }
    }
}

// impl mamually
impl TildeAble for Vec<&dyn TildeAble> {
    fn into_tildekind_loop(&self) -> Option<&dyn TildeKindLoop> {
        Some(self)
    }
}

impl TildeAble for TildeNull {}

////
////
/// impl, re-define the format method for over writing the default method
impl TildeKindVa for f32 {
    fn format(&self, _: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", *self))
    }
}

impl TildeKindVa for String {
    fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Err("un-implenmented yet".into())
    }
}

impl TildeKindVa for char {
    fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Err("un-implenmented yet".into())
    }
}

impl TildeKindLoop for Vec<&dyn TildeAble> {
    fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::Loop(vv) => {
                let zip_pair = vv.iter().cycle().zip(self);
                //dbg!(&zip_pair);
                let mut result = vec![];
                for (tilde, arg) in zip_pair {
                    //:= DEL: dbg!(tilde);
                    //:= DEL: dbg!(arg);
                    result.push(tilde.reveal(*arg)?);
                }
                Ok(result.as_slice().concat())
            }
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to Loop").into()),
        }
    }
}

impl TildeKindCond for usize {
    fn format(&self, tkind: &TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::Cond((vv, true)) => match vv.get(*self) {
                Some(tt) => tt.reveal(&TildeNull),
                None => Ok(String::new()),
            },
            TildeKind::Cond((vv, false)) => match vv.get(*self) {
                Some(tt) => tt.reveal(&TildeNull),
                None => Ok(String::new()),
            },
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to Loop").into()),
        }
    }
}

/*=========================================================*/

/// The tilde struct
#[derive(Debug, PartialEq)]
pub struct Tilde {
    len: usize,
    value: TildeKind,
}

impl Tilde {
    pub fn new(len: usize, value: TildeKind) -> Self {
        Self { len, value }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn reveal(&self, arg: &dyn TildeAble) -> Result<String, Box<dyn std::error::Error>> {
        self.value.match_reveal(arg)
    }

    /// start from '~' to the key char of tilde kind
    fn scan_for_kind(
        c: &mut Cursor<&'_ str>,
    ) -> Result<
        Box<
            dyn for<'a, 'b> Fn(
                &'a mut std::io::Cursor<&'b str>,
            )
                -> Result<Tilde, Box<(dyn std::error::Error + 'static)>>,
        >,
        Box<dyn std::error::Error>,
    > {
        let mut buf = [0u8; 1];
        c.read(&mut buf)?;
        if buf[0] != b'~' {
            return Err(TildeError::new(ErrorKind::ParseError, "should start with ~").into());
        }

        //let mut offset = 1; // after ~

        // read until the tilde key char
        let mut buf = [0_u8; 3];

        c.read(&mut buf)?;
        match buf {
            [b'a', ..] | [b'A', ..] => {
                c.seek(SeekFrom::Current(-4))?; // back to start
                return Ok(box Self::parse_value);
            }
            [b'{', ..] => {
                c.seek(SeekFrom::Current(-4))?; // back to start
                return Ok(box Self::parse_loop);
            }
            [b'$', ..] | [b'f', ..] | [b'F', ..] => {
                c.seek(SeekFrom::Current(-4))?; // back to start
                return Ok(box Self::parse_float);
            }
            [b'd', ..] | [b'D', ..] => {
                c.seek(SeekFrom::Current(-4))?; // back to start
                return Ok(box Self::parse_digit);
            }
            [b'[', ..] => {
                c.seek(SeekFrom::Current(-4))?; // back to start
                return Ok(box Self::parse_cond);
            }
            _ => {
                return Err(
                    TildeError::new(ErrorKind::ParseError, "cannot find the key tilde").into(),
                )
            }
        }
    }

    /// cursor should located on '~'
    pub fn parse(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let parser = Self::scan_for_kind(c)?;
        parser(c)
    }

    /// parse function for '~{~}'
    fn parse_loop(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut char_buf = [0u8; 2]; // two bytes
        c.read(&mut char_buf)?;
        if let Ok(s) = std::str::from_utf8(&char_buf) && s != "~{" {
			c.seek(SeekFrom::Current(-2))?; // restore the location
            return Err(
				TildeError::new(
					ErrorKind::ParseError,
					"should start with ~{",
				).into());
        }

        let mut result = vec![];
        let mut buf = vec![];
        let mut total_len = 2;

        loop {
            // read text until the next '~'
            c.read_until(b'~', &mut buf)?;

            match buf.as_slice() {
                [b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                }
                [.., b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len() - 1;
                }
                [..] => {
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len();
                    return Ok(Tilde::new(total_len, TildeKind::Loop(result)));
                }
            }

            c.read(&mut char_buf)?;

            if let Ok(s) = std::str::from_utf8(&char_buf) && s == "~}" {
				return Ok(Tilde::new(total_len + 2, TildeKind::Loop(result)));
			}

            c.seek(SeekFrom::Current(-2))?;
            //dbg!(c.position());
            // read the tilde
            let next = Tilde::parse(c)?;
            total_len += next.len;
            result.push(next);

            buf.clear()
        }
    }

    /// parse the '~[~]'
    fn parse_cond(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut char_buf = [0u8; 2]; // two bytes
        c.read(&mut char_buf)?;
        if let Ok(s) = std::str::from_utf8(&char_buf) && s != "~[" {
			c.seek(SeekFrom::Current(-2))?; // restore the location
            return Err(
				TildeError::new(
					ErrorKind::ParseError,
					"should start with ~[",
				).into());
        }

        let mut result = vec![];
        let mut buf = vec![];
        let mut total_len = 2;
        let mut flag = false;

        // new buf
        let mut char_buf = [0u8; 3];

        loop {
            // read text until the next '~'
            c.read_until(b'~', &mut buf)?;

            match buf.as_slice() {
                [b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                }
                [.., b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len() - 1;
                }
                [..] => {
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len();
                    return Ok(Tilde::new(total_len, TildeKind::Cond((result, flag))));
                }
            }

            c.read(&mut char_buf)?;

            if let Ok(s) = std::str::from_utf8(&char_buf) && s.starts_with("~]") {
				return Ok(Tilde::new(total_len + 2, TildeKind::Cond((result,flag))));
			}

            if let Ok(s) = std::str::from_utf8(&char_buf) && s.starts_with("~;") {
				c.seek(SeekFrom::Current(-1))?;
				total_len+= 2;
				flag = false;
			}

            if let Ok(s) = std::str::from_utf8(&char_buf) && s.starts_with("~:;") {
				total_len+= 3;
				flag = true;
			}
            //dbg!(c.position());
            buf.clear()
        }
    }

    /// parse function for '~a'
    fn parse_value(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];
        //:= this for in maybe re-write in helper function
        for t in [b'a', b'A'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => return Ok(Tilde::new(buf.len(), TildeKind::Va)),
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "should start with ~a").into())
    }

    /// parse the float
    fn parse_float(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];

        for t in [b'$', b'f', b'F'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => {
                    return Ok(Tilde::new(
                        buf.len(),
                        TildeKind::Float(Some(String::from_utf8(
                            buf.get(1..buf.len() - 1).map_or(Vec::new(), |s| s.to_vec()),
                        )?)),
                    ))
                }
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "cannot find the '$' or 'f'").into())
    }

    /// parse the digit
    fn parse_digit(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];

        for t in [b'd', b'D'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => {
                    return Ok(Tilde::new(
                        buf.len(),
                        TildeKind::Float(Some(String::from_utf8(
                            buf.get(1..buf.len() - 1).map_or(Vec::new(), |s| s.to_vec()),
                        )?)),
                    ))
                }
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "cannot find the '$' or 'f'").into())
    }

    //:= TODO: a lot parse functions below
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cursor() {
        let mut testcase0 = Cursor::new("abcd");
        assert_eq!(testcase0.position(), 0);

        let mut buffer: [u8; 1] = [0; 1];
        testcase0.set_position(2);
        let _ = testcase0.read(&mut buffer);
        assert_eq!(buffer[0], b'c');
    }

    #[test]
    fn test_parse_va() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~a");
        assert_eq!(Tilde::parse_value(&mut case)?, Tilde::new(2, TildeKind::Va));

        let mut case = Cursor::new("~A");
        assert_eq!(Tilde::parse_value(&mut case)?, Tilde::new(2, TildeKind::Va));
        Ok(())
    }

    #[test]
    fn test_parse_loop() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~{~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(4, TildeKind::Loop(Vec::new()))
        );

        let mut case = Cursor::new("~{a bc~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                8,
                TildeKind::Loop(vec![Tilde {
                    len: 4,
                    value: TildeKind::Text(String::from("a bc"))
                }])
            ),
        );

        let mut case = Cursor::new("~{a bc~a~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                10,
                TildeKind::Loop(vec![
                    Tilde {
                        len: 4,
                        value: TildeKind::Text(String::from("a bc"))
                    },
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    }
                ])
            )
        );

        let mut case = Cursor::new("~{~aa bc~a~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                12,
                TildeKind::Loop(vec![
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    },
                    Tilde {
                        len: 4,
                        value: TildeKind::Text(String::from("a bc"))
                    },
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    }
                ])
            )
        );

        Ok(())
    }

    #[test]
    fn test_parse_float() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~$");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(2, TildeKind::Float(Some(String::new())))
        );

        let mut case = Cursor::new("~5$");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(3, TildeKind::Float(Some("5".to_string())))
        );

        let mut case = Cursor::new("~,5f");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(4, TildeKind::Float(Some(",5".to_string())))
        );

        Ok(())
    }

    #[test]
    fn test_scan_for_kind() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~a";
        let mut c = Cursor::new(case);
        let f = Tilde::scan_for_kind(&mut c)?;

        // let mut ss = String::new();
        // c.read_to_string(&mut ss);
        // dbg!(ss);
        // c.seek(SeekFrom::Start(0));
        //dbg!(f(&mut c));

        assert_eq!(Tilde::new(2, TildeKind::Va), f(&mut c)?);
        Ok(())
    }

    #[test]
    fn test_parse_cond() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~[~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(4, TildeKind::Cond((Vec::new(), false)))
        );

        let mut case = Cursor::new("~[cero~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                8,
                TildeKind::Cond((
                    vec![Tilde {
                        len: 4,
                        value: TildeKind::Text(String::from("cero"))
                    }],
                    false
                ))
            ),
        );

        let mut case = Cursor::new("~[cero~;uno~;dos~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                18,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::Text(String::from("cero"))
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::Text(String::from("uno"))
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::Text(String::from("dos"))
                        },
                    ],
                    false
                ))
            )
        );

        let mut case = Cursor::new("~[cero~;uno~:;dos~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                19,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::Text(String::from("cero"))
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::Text(String::from("uno"))
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::Text(String::from("dos"))
                        },
                    ],
                    true
                ))
            )
        );

        Ok(())
    }
}
