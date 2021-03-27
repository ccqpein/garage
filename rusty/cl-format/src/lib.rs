mod args;

use core::fmt::Debug;
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};

trait Tilde {}

impl Debug for dyn Tilde {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Ok(())
    }
}

#[derive(Debug)]
enum ParseData {
    B(Vec<u8>),
    T(Box<dyn Tilde>),
}

struct ControlStr<'a> {
    inner: &'a str,
}

impl<'a> ControlStr<'a> {
    fn new(s: &'a str) -> Self {
        Self { inner: s }
    }

    fn parse(&self) -> Result<Vec<ParseData>, String> {
        let mut cursor = Cursor::new(self.inner);
        let mut result = vec![];
        loop {
            match self.scan(&mut cursor)? {
                Some(d) => result.push(d),
                None => return Ok(result),
            }
        }
    }

    fn scan(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
        match self.until_tilde(c)? {
            a @ Some(_) => return Ok(a),
            None => (), // maybe start with ~
        }

        match self.read_next_tilde(c)? {
            None => Ok(None),
            a @ Some(_) => return Ok(a),
        }
    }

    fn until_tilde(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
        let mut buf = vec![];
        let i = c.read_until(b'~', &mut buf).map_err(|e| e.to_string())?;

        // start with '~' is other parser's job
        if i == 1 {
            c.seek(SeekFrom::Current(-1));
            return Ok(None);
        }

        if let Some(lst) = buf.last() {
            if *lst == b'~' {
                buf.pop();
                c.seek(SeekFrom::Current(-1));
            } // else means it is the endding
        } else {
            return Ok(None);
        }

        Ok(Some(ParseData::B(buf)))
    }

    //:= TODO: just demo now
    fn read_next_tilde(&self, c: &mut Cursor<&str>) -> Result<Option<ParseData>, String> {
        let mut buffer = vec![];
        let mut one_byte_buffer: [u8; 1] = [0; 1];
        let mut i = 0;

        let mut count = 0;
        loop {
            i = c.read(&mut one_byte_buffer).map_err(|e| e.to_string())?;
            if i == 0 {
                break;
            }
            //:= TODO: more rule here
            count += 1;
            buffer.extend_from_slice(&one_byte_buffer);
            if count == 2 {
                break;
            }
        }

        //:= TODO: parse to real data type here
        if buffer.len() == 0 {
            return Ok(None);
        }
        return Ok(Some(ParseData::B(buffer)));
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

    //:= TODO: demo test
    #[test]
    fn test_parse() {
        let testcase0 = ControlStr::new("aaa ~D~%");
        dbg!(testcase0
            .parse()
            .unwrap()
            .iter()
            .map(|v| match v {
                ParseData::B(b) => {
                    String::from_utf8(b.clone()).unwrap()
                }
                _ => {
                    String::new()
                }
            })
            .collect::<Vec<String>>());
    }
}
