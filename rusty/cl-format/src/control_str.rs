use crate::tildes::*;
use std::fmt::{Debug, Display};
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};

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
            let end_index = has_read_len + t.len();

            result.push(((has_read_len, end_index), t));
            has_read_len = end_index;
            buf.clear();
        }
    }

    //:= TODO
    //fn output(&self, args: impl Iterator<Item = &dyn Revealable>) {}
}

pub trait Revealable: Display {}

#[cfg(test)]
mod test {
    use super::*;

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

        let case = "~{~5$~}";
        let c = Cursor::new(case);

        assert_eq!(
            ControlStr::scan(c)?,
            vec![(
                (0, 7),
                Tilde::new(
                    7,
                    TildeKind::Loop(vec![Tilde::new(3, TildeKind::Float(Some("5".to_string())))])
                )
            )]
        );

        Ok(())
    }
}
