use std::{
    collections::VecDeque,
    io::{Cursor, Read, Seek, SeekFrom},
    string::ParseError,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ParserType {
    String,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Sym {
    pub name: String,
    pub read_type: Option<ParserType>,
}

impl Sym {
    fn read(s: &str) -> Self {
        Self {
            name: s.to_string(),
            ..Default::default()
        }
    }

    fn read_string(s: &str) -> Self {
        Self {
            name: s.to_string(),
            read_type: Some(ParserType::String),
        }
    }

    pub fn is_string(&self) -> bool {
        match self.read_type {
            Some(ParserType::String) => true,
            _ => false,
        }
    }

    pub fn sym_name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    InvalidStart,
    InvalidToken(&'static str),
    UnknownToken,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Atom {
    /// symbol, if it is the number, it can also be number, can parse when use it
    Sym(Sym),
    List(Vec<Atom>),
}

impl Atom {
    pub fn into_sym(&self) -> Option<&Sym> {
        match self {
            Atom::Sym(sym) => Some(sym),
            _ => None,
        }
    }
}

pub fn tokenize(mut source_code: impl Read) -> VecDeque<String> {
    let mut buf = [0; 1];
    let mut cache = vec![];
    let mut res = vec![];
    loop {
        match source_code.read(&mut buf) {
            Ok(n) if n != 0 => {
                let c = buf.get(0).unwrap();
                match c {
                    b'(' | b' ' | b')' | b'\'' | b'"' => {
                        if !cache.is_empty() {
                            res.push(String::from_utf8(cache.clone()).unwrap());
                            cache.clear();
                        }

                        if *c != b' ' {
                            res.push(String::from_utf8(vec![*c]).unwrap());
                        }
                    }
                    _ => {
                        cache.push(*c);
                    }
                }
            }
            Ok(_) => break,
            Err(_) => todo!(),
        }
    }

    res.into()
}

/// entry
pub fn read_root(tokens: &mut VecDeque<String>) -> Result<Vec<Atom>, ParserError> {
    match tokens.get(0) {
        Some(t) if t == "(" => {}
        _ => return Err(ParserError::InvalidStart),
    }

    let mut res = vec![];
    loop {
        match tokens.front() {
            Some(b) => match b.as_str() {
                "(" => {
                    res.push(read_exp(tokens)?);
                }
                " " => (),
                _ => {
                    return {
                        println!("{:?}", b);
                        Err(ParserError::InvalidToken("in read_root"))
                    };
                }
            },
            None => break,
        }
    }

    Ok(res)
}

/// choose which read function
fn read_router(
    token: &str,
) -> Result<fn(&mut VecDeque<String>) -> Result<Atom, ParserError>, ParserError> {
    match token {
        "(" => Ok(read_exp),
        "'" => Ok(read_quote),
        "\"" => Ok(read_string),
        _ => Ok(read_sym),
    }
}

fn read_sym(tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
    let token = tokens
        .pop_front()
        .ok_or(ParserError::InvalidToken("in read_sym"))?;

    Ok(Atom::Sym(Sym::read(&token)))
}

fn read_quote(tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
    let mut res = vec![Atom::Sym(Sym::read("quote"))];
    tokens
        .pop_front()
        .ok_or(ParserError::InvalidToken("in read_quote"))?;

    match tokens.front() {
        Some(t) => res.push(read_router(t)?(tokens)?),
        None => return Err(ParserError::InvalidToken("in read_quote")),
    }

    Ok(Atom::List(res))
}

/// start from '\('
fn read_exp(tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
    let mut res = vec![];
    tokens.pop_front();

    loop {
        match tokens.front() {
            Some(t) if t == ")" => {
                tokens.pop_front();
                break;
            }
            Some(t) => res.push(read_router(t)?(tokens)?),
            None => return Err(ParserError::InvalidToken("in read_exp")),
        }
    }

    Ok(Atom::List(res))
}

/// start with "
fn read_string(tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
    tokens.pop_front();

    let token = tokens
        .pop_front()
        .ok_or(ParserError::InvalidToken("in read_string"))?;

    tokens.pop_front();
    Ok(Atom::Sym(Sym::read_string(&token)))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize() {
        let s = "(a b c 123 c)";
        assert_eq!(
            tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", "b", "c", "123", "c", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        let s = r#"(a '(""))"#;
        assert_eq!(
            tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", "'", "(", "\"", "\"", ")", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        let s = r#"(a '() '1)"#;
        assert_eq!(
            tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", "'", "(", ")", "'", "1", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_read_exp() {
        let mut t = tokenize(Cursor::new("(a b c 123 c)".as_bytes()));
        assert_eq!(
            read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("a")),
                    Atom::Sym(Sym::read("b")),
                    Atom::Sym(Sym::read("c")),
                    Atom::Sym(Sym::read("123")),
                    Atom::Sym(Sym::read("c")),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        let mut t = tokenize(Cursor::new("((a) b c 123 c)".as_bytes()));
        assert_eq!(
            read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::List([Atom::Sym(Sym::read("a"))].to_vec()),
                    Atom::Sym(Sym::read("b")),
                    Atom::Sym(Sym::read("c")),
                    Atom::Sym(Sym::read("123")),
                    Atom::Sym(Sym::read("c")),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());
    }

    #[test]
    fn test_read_string() {
        let mut t = tokenize(Cursor::new(r#""hello""#.as_bytes()));
        assert_eq!(read_string(&mut t), Ok(Atom::Sym(Sym::read("hello"))));
        assert!(t.is_empty());
    }

    #[test]
    fn test_read_root() {
        let mut t = tokenize(Cursor::new("(a b c 123 c) (a '(1 2 3))".as_bytes()));
        //dbg!(read_root(&mut t));

        assert_eq!(
            read_root(&mut t),
            Ok(vec![
                Atom::List(vec![
                    Atom::Sym(Sym::read("a")),
                    Atom::Sym(Sym::read("b")),
                    Atom::Sym(Sym::read("c")),
                    Atom::Sym(Sym::read("123")),
                    Atom::Sym(Sym::read("c")),
                ],),
                Atom::List(vec![
                    Atom::Sym(Sym::read("a")),
                    Atom::List(vec![
                        Atom::Sym(Sym::read("quote")),
                        Atom::List(vec![
                            Atom::Sym(Sym::read("1")),
                            Atom::Sym(Sym::read("2")),
                            Atom::Sym(Sym::read("3")),
                        ],),
                    ],),
                ],),
            ],)
        );

        assert!(t.is_empty());

        let mut t = tokenize(Cursor::new(r#"('a "hello")"#.as_bytes()));
        assert_eq!(
            read_root(&mut t),
            Ok(vec![Atom::List(vec![
                Atom::List(vec![
                    Atom::Sym(Sym::read("quote")),
                    Atom::Sym(Sym::read("a")),
                ],),
                Atom::Sym(Sym::read_string("hello")),
            ])]),
        );
        assert!(t.is_empty());
    }
}
