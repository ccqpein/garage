use clap::Clap;
use lazy_static::*;
use serde_json::{Value, *};
use std::{collections::HashMap, env, ffi::OsString};

const DICT: &'static str = r#"
{
"rs":["//"],
"go":["//"],
"lisp":[";"]
}
"#;

lazy_static! {
    static ref TABLE: HashMap<String, Vec<String>> = serde_json::from_str(DICT).unwrap();
    static ref REGEX_TABLE: HashMap<String, String> = TABLE
        .iter()
        .map(|(k, v)| (k.clone(), make_regex(v)))
        .collect();
}

fn make_regex(com_syms: &Vec<String>) -> String {
    let mut head = String::new();
    for s in com_syms {
        head.push('|');
        head.push_str(s);
        head.push('+');
    }

    let _ = head.drain(..1).collect::<String>();

    format!("({}):=\\s+(.*)", head)
}

#[derive(Default)]
pub struct Config {
    pub(super) filetypes: Vec<OsString>,
    pub(super) ignore_dirs: Vec<OsString>,
    keywords: Option<Vec<String>>,
    dirs: Option<Vec<String>>,
}

impl From<&Args> for Config {
    fn from(a: &Args) -> Self {
        Self {
            filetypes: a.filetypes.clone(),
            ignore_dirs: a.ignore_dir.clone(),
            keywords: a.keywords.clone(),
            dirs: a.dirs.clone(),
        }
    }
}

/// Args struct from command line
#[derive(Default, Clap, Debug)]
#[clap(version = "0.1.0", author = "ccQpein")]
pub struct Args {
    #[clap(short, long)]
    filetypes: Vec<OsString>,

    #[clap(short, long, default_value = ".")]
    dirs: Option<Vec<String>>,

    #[clap(short = "x", long = "ignore-dir")]
    ignore_dir: Vec<OsString>,

    #[clap(short, long)]
    keywords: Option<Vec<String>>,

    #[clap(short, long)]
    jsonx: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use regex::Regex;

    // #[test]
    // fn test_comments_sym() {
    //     assert_eq!(comments_sym("rs"), &vec!["//"]);
    //     assert_eq!(comments_sym("lisp"), &vec![";"]);
    // }

    #[test]
    fn test_make_regex() {
        assert_eq!(
            make_regex(&vec![String::from("//"), String::from(";")]),
            String::from(r#"(//+|;+):=\s+(.*)"#)
        );
    }

    #[test]
    fn test_regex() {
        let re = Regex::new(&make_regex(&vec![String::from("//"), String::from(";")])).unwrap();
        let cap = re.captures("Aabbcc //:= test").unwrap();
        assert_eq!(&cap[2], "test");

        let cap = re.captures("Aabbcc ;:= test").unwrap();
        assert_eq!(&cap[2], "test");

        let cap = re.captures("Aabbcc ;;;:= test").unwrap();
        assert_eq!(&cap[2], "test");

        assert!(re.captures("Aabbcc #:= test").is_none());

        assert!(re.captures("Aabbcc ; test").is_none());
    }
}
