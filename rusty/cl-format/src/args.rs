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

#[derive(Debug, PartialEq)]
enum Tilde {
    /// ~C or ~:C
    Char,

    /// ~a
    Va,

    /// loop
    Loop(Vec<Tilde>),

    /// text inside the tilde
    Text(String),

    /// vec
    VecTilde(Vec<Tilde>),
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
    /// has to have '~' at current posiont of cursor
    fn parse(mut c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut char_buf = [0u8; 1];

        c.read(&mut char_buf)?;
        if char_buf[0] != b'~' {
            return Err(
                TildeError::new(ErrorKind::ParseError, "'~' has to be the first char").into(),
            );
        }

        c.read(&mut char_buf)?;
        match char_buf.as_slice() {
            [b'a'] => {
                c.seek(SeekFrom::Current(-2))?;
                Self::parse_value(c)
            }
            [b'{'] => {
                c.seek(SeekFrom::Current(-2))?;
                Self::parse_loop(c)
            }
            _ => todo!(),
        }
    }

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

        loop {
            // read text until the next '~'
            c.read_until(b'~', &mut buf)?;

            match buf.as_slice() {
                [b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                }
                [.., b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                    result.push(Tilde::Text(String::from_utf8(
                        buf[..buf.len() - 1].to_vec(),
                    )?));
                }
                [..] => {
                    result.push(Tilde::Text(String::from_utf8(buf.to_vec())?));
                    return Ok(Self::Loop(result));
                }
            }

            c.read(&mut char_buf)?;
            if let Ok(s) = std::str::from_utf8(&char_buf) && s == "~}" {
				return Ok(Self::Loop(result))
			}

            c.seek(SeekFrom::Current(-2))?;

            // read the tilde
            result.push(Tilde::parse(c)?);
            buf.clear()
        }
    }

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

        Ok(Self::Va)
    }
}

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
        assert_eq!(Tilde::parse_value(&mut case)?, Tilde::Va,);
        Ok(())
    }

    #[test]
    fn test_parse_loop() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~{~}");

        assert_eq!(Tilde::parse_loop(&mut case)?, Tilde::Loop(Vec::new()));

        let mut case = Cursor::new("~{a bc~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::Loop(vec![Tilde::Text(String::from("a bc"))])
        );

        let mut case = Cursor::new("~{a bc~a~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::Loop(vec![Tilde::Text(String::from("a bc")), Tilde::Va,])
        );

        Ok(())
    }
}
