#![feature(let_chains)]

use std::collections::HashMap;
use std::fmt::Debug;
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};

#[derive(Debug, PartialEq)]
enum TildeKind {
    /// ~C or ~:C
    Char,

    /// ~$ or ~5$
    Float(Option<usize>),

    /// ~a
    Va,

    /// loop
    Loop(Vec<Tilde>),

    /// text inside the tilde
    Text(String),

    /// vec
    VecTilde(Vec<Tilde>),
}

#[derive(Debug, PartialEq)]
struct Tilde {
    len: usize,
    value: TildeKind,
}

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
}

impl Tilde {
    fn new(len: usize, value: TildeKind) -> Self {
        Self { len, value }
    }

    /// start from '~' to the key char of tilde kind
    fn scan_for_kind(
        c: &mut Cursor<&'_ str>,
    ) -> Result<
        impl for<'a, 'b> Fn(
            &'a mut std::io::Cursor<&'b str>,
        ) -> Result<Tilde, Box<(dyn std::error::Error + 'static)>>,
        Box<dyn std::error::Error>,
    > {
        //:= TODO
        let mut buf = [0u8; 1];
        c.read(&mut buf)?;
        if buf[0] != b'~' {
            return Err(TildeError::new(ErrorKind::ParseError, "should start with ~").into());
        }

        let mut offset = 1; // after ~

        loop {
            buf[0] = 0;
            c.read(&mut buf)?;
            offset += 1;

            match buf[0] {
                b'a' => {
                    c.seek(SeekFrom::Current(-offset))?; // back to start
                    return Ok(Self::parse_value);
                }
                _ => {
                    return Err(
                        TildeError::new(ErrorKind::ParseError, "cannot find the key tilde").into(),
                    )
                }
            }
        }
    }

    /// has to have '~' at current posiont of cursor
    fn parse(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
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

            // read the tilde
            let next = Tilde::parse(c)?;
            total_len += next.len;
            result.push(next);

            buf.clear()
        }
    }

    /// parse function for '~a'
    fn parse_value(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut char_buf = [0u8; 2]; // two bytes
        c.read(&mut char_buf)?;
        if let Ok(s) = std::str::from_utf8(&char_buf) && s != "~a" {
			c.seek(SeekFrom::Current(-2))?; // restore the location
            return Err(
				TildeError::new(
					ErrorKind::ParseError,
					"should start with ~a",
				).into());
        }

        Ok(Tilde::new(2, TildeKind::Va))
    }

    // parse the float
    //fn parse_float(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {}

    //:= TODO: a lot parse functions below
}

/// the control string should including:
/// 1. the whole string
/// 2. the parsed tree
struct ControlStr<'a> {
    inner: &'a str,
    tildes: Vec<((usize, usize), Tilde)>,
}

impl<'a> ControlStr<'a> {
    fn new(s: &'a str) -> Result<Self, Box<dyn std::error::Error>> {
        let cc = Cursor::new(s);
        let tildes = Self::scan(cc)?;

        Ok(Self { inner: s, tildes })
    }

    fn scan(
        mut s: Cursor<&'_ str>,
    ) -> Result<Vec<((usize, usize), Tilde)>, Box<dyn std::error::Error>> {
        let mut buf = vec![];
        let mut has_read_len = 0;
        let mut result = vec![];

        loop {
            dbg!(s.position());
            s.read_until(b'~', &mut buf)?;
            match buf.last() {
                // find the next '~'
                Some(b'~') => {
                    has_read_len += buf.len() - 1;
                    s.seek(SeekFrom::Current(-1))?;
                }
                _ => return Ok(result),
            }

            let t = Tilde::parse(&mut s)?;
            let end_index = has_read_len + t.len;

            result.push(((has_read_len, end_index), t));
            has_read_len = end_index;
            buf.clear();
        }
    }
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
    fn test_control_str_scan() -> Result<(), Box<dyn std::error::Error>> {
        let case = "hello wor~{~a~}";
        let c = Cursor::new(case);

        assert_eq!(
            ControlStr::scan(c)?,
            vec![(
                (9, 15),
                Tilde::new(6, TildeKind::Loop(vec![Tilde::new(2, TildeKind::Va)]))
            )]
        );

        Ok(())
    }

    //:= DEL: just for test
    #[test]
    fn test_read_until_tilde_symbol() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~,5f";
        let mut c = Cursor::new(case);
        let mut buf = [0u8; 1];

        c.read(&mut buf)?;
        dbg!(String::from_utf8(buf.to_vec())?);
        c.read(&mut buf)?;

        dbg!(String::from_utf8(buf.to_vec())?);

        c.read(&mut buf)?;
        c.read(&mut buf)?;
        c.read(&mut buf)?;
        c.read(&mut buf)?;
        c.read(&mut buf)?;
        dbg!(String::from_utf8(buf.to_vec())?);

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
}
