#![feature(iter_array_chunks)]
#![feature(lazy_get)]
mod data;

use std::{collections::VecDeque, error::Error, io::Read};

use itertools::Itertools;
use tracing::error;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeValue {
    Symbol(String),
    String(String),
    Keyword(String),
    Number(i64),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Sym {
    pub literal: String,
    pub value: TypeValue,
}

impl Sym {
    fn read(s: &str) -> Self {
        Self {
            literal: s.to_string(),
            value: TypeValue::Symbol(s.to_string()),
        }
    }

    fn read_string(s: &str) -> Self {
        Self {
            literal: s.to_string(),
            value: TypeValue::String(s.to_string()),
        }
    }

    fn read_keyword(s: &str) -> Self {
        Self {
            literal: s.to_string(),
            value: TypeValue::Keyword(s.to_string()),
        }
    }

    fn read_number(s: &str, n: i64) -> Self {
        Self {
            literal: s.to_string(),
            value: TypeValue::Number(n),
        }
    }

    pub fn is_string(&self) -> bool {
        match self.value {
            TypeValue::String(_) => true,
            _ => false,
        }
    }

    pub fn sym_lit(&self) -> &str {
        &self.literal
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    InvalidStart,
    InvalidToken(&'static str),
    UnknownToken,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unable to read configuration at {:?}", self)
    }
}

impl Error for ParserError {}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Atom {
    /// symbol, if it is the number, it can also be number, can parse when use it
    Sym(Sym),
    List(Vec<Atom>),
    Quote(Box<Atom>),
}

impl Atom {
    pub fn into_tokens(&self) -> String {
        match self {
            Atom::Sym(sym) => match sym.value {
                TypeValue::Symbol(_) => sym.literal.clone(),
                TypeValue::String(_) => String::from("\"") + &sym.literal + "\"",
                TypeValue::Keyword(_) => String::from(":") + &sym.literal,
                TypeValue::Number(d) => d.to_string(),
            },
            Atom::List(atoms) => {
                String::from("(")
                    + &atoms
                        .iter()
                        .map(|a| a.into_tokens())
                        .collect::<Vec<String>>()
                        .join(" ")
                    + ")"
            }
            Atom::Quote(atom) => String::from("'") + &atom.into_tokens(),
        }
    }

    pub fn nth(&self, ind: usize) -> Option<&Self> {
        match self {
            Atom::List(atoms) => atoms.get(ind),
            _ => None,
        }
    }

    pub fn iter(&self) -> Option<impl Iterator<Item = &Atom>> {
        match self {
            Atom::List(atoms) => Some(atoms.iter()),
            _ => None,
        }
    }
}

pub struct Parser {
    /// will read number if this field is true
    read_number_config: bool,

    inner_atoms: Vec<Atom>,
}

impl Parser {
    fn new() -> Self {
        Self {
            read_number_config: false,
            inner_atoms: vec![],
        }
    }

    /// set the parser read_number config
    fn config_read_number(mut self, v: bool) -> Self {
        self.read_number_config = v;
        self
    }

    /// tokenize the source code
    pub fn tokenize(&self, mut source_code: impl Read) -> VecDeque<String> {
        let mut buf = [0; 1];
        let mut cache = vec![];
        let mut res = vec![];
        loop {
            match source_code.read(&mut buf) {
                Ok(n) if n != 0 => {
                    let c = buf.get(0).unwrap();
                    match c {
                        b'(' | b' ' | b')' | b'\'' | b'"' | b':' | b'\n' => {
                            if !cache.is_empty() {
                                res.push(String::from_utf8(cache.clone()).unwrap());
                                cache.clear();
                            }

                            match res.last() {
                                Some(le) if le == " " && *c == b' ' => continue,
                                _ => (),
                            }

                            res.push(String::from_utf8(vec![*c]).unwrap())
                        }
                        _ => {
                            cache.push(*c);
                        }
                    }
                }
                Ok(_) => break,
                Err(e) => error!("error in tokenize step {}", e),
            }
        }

        if !cache.is_empty() {
            res.push(String::from_utf8(cache.clone()).unwrap());
        }

        res.into()
    }

    pub fn parse_root(&mut self, source_code: impl Read) -> Result<(), ParserError> {
        self.inner_atoms.clear();
        let mut tokens = self.tokenize(source_code);

        match tokens.get(0) {
            Some(t) if t == "(" => {}
            _ => return Err(ParserError::InvalidStart),
        }

        loop {
            match tokens.front() {
                Some(b) => match b.as_str() {
                    "(" => {
                        self.inner_atoms.push(self.read_exp(&mut tokens)?);
                    }
                    " " | "\n" => {
                        tokens.pop_front();
                    }
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

        Ok(())
    }

    /// parse the single expression and add to parser's inner atoms
    pub fn parse_exp(&mut self, source_code: impl Read) -> Result<(), ParserError> {
        let mut tokens = self.tokenize(source_code);
        self.inner_atoms.push(self.read_exp(&mut tokens)?);

        Ok(())
    }

    /// choose which read function
    fn read_router(
        &self,
        token: &str,
    ) -> Result<fn(&Self, &mut VecDeque<String>) -> Result<Atom, ParserError>, ParserError> {
        match token {
            "(" => Ok(Self::read_exp),
            "'" => Ok(Self::read_quote),
            "\"" => Ok(Self::read_string),
            ":" => Ok(Self::read_keyword),
            _ => Ok(Self::read_sym),
        }
    }

    fn read_sym(&self, tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
        let token = tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_sym"))?;

        if self.read_number_config {
            match token.parse::<i64>() {
                Ok(n) => return Ok(Atom::Sym(Sym::read_number(&token, n))),
                Err(_) => (),
            }
        }

        Ok(Atom::Sym(Sym::read(&token)))
    }

    fn read_quote(&self, tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
        tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_quote"))?;

        let res = match tokens.front() {
            Some(t) => self.read_router(t)?(self, tokens)?,
            None => return Err(ParserError::InvalidToken("in read_quote")),
        };

        Ok(Atom::Quote(Box::new(res)))
    }

    /// start from '\('
    pub fn read_exp(&self, tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
        let mut res = vec![];
        tokens.pop_front();

        loop {
            match tokens.front() {
                Some(t) if t == ")" => {
                    tokens.pop_front();
                    break;
                }
                // ignore spaces
                Some(t) if t == " " || t == "\n" => {
                    tokens.pop_front();
                }
                Some(t) => res.push(self.read_router(t)?(self, tokens)?),
                None => return Err(ParserError::InvalidToken("in read_exp")),
            }
        }

        Ok(Atom::List(res))
    }

    /// start with "
    fn read_string(&self, tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
        tokens.pop_front();

        let mut escape = false;
        let mut res = String::new();
        let mut this_token;
        loop {
            this_token = tokens
                .pop_front()
                .ok_or(ParserError::InvalidToken("in read_string"))?;

            if escape {
                res = res + &this_token;
                escape = false;
                continue;
            }

            match this_token.as_str() {
                "\\" => escape = true,
                "\"" => break,
                _ => res = res + &this_token,
            }
        }

        Ok(Atom::Sym(Sym::read_string(&res)))
    }

    /// start with :
    fn read_keyword(&self, tokens: &mut VecDeque<String>) -> Result<Atom, ParserError> {
        tokens.pop_front();

        let token = tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_keyword"))?;

        Ok(Atom::Sym(Sym::read_keyword(&token)))
    }

    /// parser reverse the tokens from atoms
    pub fn into_tokens(&self) -> String {
        self.inner_atoms.iter().map(|a| a.into_tokens()).join("\n")
    }
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn test_tokenize() {
        let parser = Parser::new();
        //
        let s = "(a b c 123 c)";
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "b", " ", "c", " ", "123", " ", "c", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(a '(""))"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "'", "(", "\"", "\"", ")", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(a '() '1)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "'", "(", ")", " ", "'", "1", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(def-msg language-perfer :lang 'string)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(",
                "def-msg",
                " ",
                "language-perfer",
                " ",
                ":",
                "lang",
                " ",
                "'",
                "string",
                ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        //
        let s = r#"(def-rpc get-book
                     '(:title 'string :vesion 'string :lang 'language-perfer)
                    'book-info)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(",
                "def-rpc",
                " ",
                "get-book",
                "\n",
                " ",
                "'",
                "(",
                ":",
                "title",
                " ",
                "'",
                "string",
                " ",
                ":",
                "vesion",
                " ",
                "'",
                "string",
                " ",
                ":",
                "lang",
                " ",
                "'",
                "language-perfer",
                ")",
                "\n",
                " ",
                "'",
                "book-info",
                ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        //
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(", "get-book", " ", ":", "title", " ", "\"", "hello", " ", "world", "\"", " ",
                ":", "version", " ", "\"", "1984", "\"", ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        // escapr "
        let s = r#"( get-book :title "hello \"world" :version "1984")"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(", " ", "get-book", " ", ":", "title", " ", "\"", "hello", " ", "\\", "\"",
                "world", "\"", " ", ":", "version", " ", "\"", "1984", "\"", ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        // number

        let s = r#"( get-book :id 1984)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", " ", "get-book", " ", ":", "id", " ", "1984", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_read_string() {
        let parser = Parser::new();
        let mut t = parser.tokenize(Cursor::new(r#""hello""#.as_bytes()));
        assert_eq!(
            parser.read_string(&mut t),
            Ok(Atom::Sym(Sym::read_string("hello")))
        );
        assert!(t.is_empty());
    }

    #[test]
    fn test_read_number() {
        let parser = Parser::new().config_read_number(true);

        let mut t = parser.tokenize(Cursor::new(r#"123"#.as_bytes()));

        assert_eq!(
            parser.read_sym(&mut t),
            Ok(Atom::Sym(Sym::read_number("123", 123)))
        );
    }

    #[test]
    fn test_read_exp() {
        let parser = Parser::new();
        let mut t = parser.tokenize(Cursor::new("(a b c 123 c)".as_bytes()));
        assert_eq!(
            parser.read_exp(&mut t),
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

        //
        let mut t = parser.tokenize(Cursor::new("((a) b c 123 c)".as_bytes()));
        assert_eq!(
            parser.read_exp(&mut t),
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

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(def-msg language-perfer :lang 'string)"#.as_bytes(),
        ));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("def-msg")),
                    Atom::Sym(Sym::read("language-perfer")),
                    Atom::Sym(Sym::read_keyword("lang")),
                    Atom::Quote(Box::new(Atom::Sym(Sym::read("string")))),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        ));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("def-rpc")),
                    Atom::Sym(Sym::read("get-book")),
                    Atom::Quote(Box::new(Atom::List(
                        [
                            Atom::Sym(Sym::read_keyword("title")),
                            Atom::Quote(Box::new(Atom::Sym(Sym::read("string")))),
                            Atom::Sym(Sym::read_keyword("version")),
                            Atom::Quote(Box::new(Atom::Sym(Sym::read("string")))),
                            Atom::Sym(Sym::read_keyword("lang")),
                            Atom::Quote(Box::new(Atom::Sym(Sym::read("language-perfer")))),
                        ]
                        .to_vec()
                    ))),
                    Atom::Quote(Box::new(Atom::Sym(Sym::read("book-info")))),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello world" :version "1984")"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("get-book")),
                    Atom::Sym(Sym::read_keyword("title")),
                    Atom::Sym(Sym::read_string("hello world")),
                    Atom::Sym(Sym::read_keyword("version")),
                    Atom::Sym(Sym::read_string("1984")),
                ]
                .to_vec()
            ),)
        );

        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello \"world" :version "1984")"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("get-book")),
                    Atom::Sym(Sym::read_keyword("title")),
                    Atom::Sym(Sym::read_string("hello \"world")),
                    Atom::Sym(Sym::read_keyword("version")),
                    Atom::Sym(Sym::read_string("1984")),
                ]
                .to_vec()
            ),)
        );

        //

        let parser = Parser::new().config_read_number(true);

        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello world" :id 1984)"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Atom::List(
                [
                    Atom::Sym(Sym::read("get-book")),
                    Atom::Sym(Sym::read_keyword("title")),
                    Atom::Sym(Sym::read_string("hello world")),
                    Atom::Sym(Sym::read_keyword("id")),
                    Atom::Sym(Sym::read_number("1984", 1984)),
                ]
                .to_vec()
            ),)
        );
    }

    #[test]
    fn test_read_root() {
        let mut parser = Parser::new();

        parser
            .parse_root(&mut Cursor::new("(a b c 123 c) (a '(1 2 3))".as_bytes()))
            .unwrap();
        assert_eq!(
            parser.inner_atoms,
            vec![
                Atom::List(vec![
                    Atom::Sym(Sym::read("a")),
                    Atom::Sym(Sym::read("b")),
                    Atom::Sym(Sym::read("c")),
                    Atom::Sym(Sym::read("123")),
                    Atom::Sym(Sym::read("c")),
                ],),
                Atom::List(vec![
                    Atom::Sym(Sym::read("a")),
                    Atom::Quote(Box::new(Atom::List(vec![
                        Atom::Sym(Sym::read("1")),
                        Atom::Sym(Sym::read("2")),
                        Atom::Sym(Sym::read("3")),
                    ]))),
                ],),
            ],
        );

        parser.parse_root(Cursor::new(r#"('a "hello")"#.as_bytes()));
        assert_eq!(
            parser.inner_atoms,
            vec![Atom::List(vec![
                Atom::Quote(Box::new(Atom::Sym(Sym::read("a")))),
                Atom::Sym(Sym::read_string("hello")),
            ])],
        );

        //
        let mut t = Cursor::new(
            r#"(def-msg language-perfer :lang 'string)

(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        );

        let mut t0 = parser.tokenize(Cursor::new(
            r#"(def-msg language-perfer :lang 'string)"#.as_bytes(),
        ));

        let mut t1 = parser.tokenize(Cursor::new(
            r#"(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        ));

        parser.parse_root(&mut t);
        assert_eq!(
            parser.inner_atoms,
            vec![
                parser.read_exp(&mut t0).unwrap(),
                parser.read_exp(&mut t1).unwrap()
            ]
        );
    }

    #[test]
    fn test_into_tokens() {
        let mut parser = Parser::new();
        let mut t = Cursor::new(
            r#"(def-msg language-perfer :lang 'string)

(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        );

        parser.parse_root(&mut t).unwrap();

        assert_eq!(
            parser.into_tokens(),
            vec![
                "(def-msg language-perfer :lang 'string)".to_string(),
                "(def-rpc get-book '(:title 'string :version 'string :lang 'language-perfer) 'book-info)".to_string(),
            ].join("\n"),
        );
    }
}
