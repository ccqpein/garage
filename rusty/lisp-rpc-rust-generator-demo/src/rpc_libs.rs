// let me assume I have this struct have been generate by generater
use super::*;
use lisp_rpc_rust_serializer::register_global_map_type;
use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LanguagePerfer {
    pub lang: String,
}

impl_to_rpc!(LanguagePerfer, RPCType::Msg("language-perfer".to_string()));

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct BookInfo {
    pub lang: LanguagePerfer,
    pub title: String,
    pub version: String,
    pub id: String,
}

impl_to_rpc!(BookInfo, RPCType::Msg("book-info".to_string()));

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct GetBookLang {
    pub lang: String,
    pub encoding: i64,
}

impl_to_rpc!(GetBookLang, RPCType::Map);

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct GetBook {
    pub title: String,
    pub version: String,
    pub lang: GetBookLang,
    pub authors: Authors,
}

impl_to_rpc!(GetBook, RPCType::RPC("get-book".to_string()));

#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct Authors {
    pub names: Vec<String>,
}

impl_to_rpc!(Authors, RPCType::Msg("authors".to_string()));

pub fn init() {
    register_global_map_type("GetBookLang")
}

#[cfg(test)]
mod tests {
    use super::*;
    use lisp_rpc_rust_serializer::*;

    #[test]
    fn test_get_book_rpc_data() {
        init();
        let gb = GetBook {
            title: "hello world".to_string(),
            version: "1984".to_string(),
            lang: GetBookLang {
                lang: "english".to_string(),
                encoding: 11,
            },
            authors: Authors {
                names: vec!["a".to_string()],
            },
        };

        //dbg!(&GLOBAL_MAP_TYPES);

        assert_eq!(
            gb.serialize_lisp().unwrap(),
            r#"(get-book :title "hello world" :version "1984" :lang '(:lang "english" :encoding 11) :authors (authors :names '("a")))"#
        );
    }

    #[test]
    fn test_book_info_rpc_data() {
        let bi = BookInfo {
            id: "123".to_string(),
            title: "hello world".to_string(),
            version: "1984".to_string(),
            lang: LanguagePerfer {
                lang: "english".to_string(),
            },
        };
        assert_eq!(
            bi.serialize_lisp().unwrap(),
            r#"(book-info :lang (language-perfer :lang "english") :title "hello world" :version "1984" :id "123")"#
        )
    }

    #[test]
    fn test_book_info_rpc_data_to_struct() {
        let bi = BookInfo {
            id: "123".to_string(),
            title: "hello world".to_string(),
            version: "1984".to_string(),
            lang: LanguagePerfer {
                lang: "english".to_string(),
            },
        };

        let s = bi.serialize_lisp().unwrap();

        assert_eq!(lisp_rpc_from_str::<BookInfo>(&s).unwrap(), bi);
    }

    #[test]
    fn test_list_rpc_data_to_struct() {
        let authors = r#"(authors :names '("a"))"#;
        assert_eq!(
            lisp_rpc_from_str::<Authors>(authors).unwrap(),
            Authors {
                names: vec!["a".to_string()],
            }
        );

        let raw_list = r#"'(1 2 3 4)"#;
        assert_eq!(
            lisp_rpc_from_str::<Vec<i64>>(raw_list).unwrap(),
            vec![1, 2, 3, 4]
        )
    }

    #[test]
    fn test_map_rpc_data_to_struct() {
        let gbl = r#"(get-book-lang :lang "english" :encoding 11)"#;
        assert_eq!(
            lisp_rpc_from_str::<GetBookLang>(gbl).unwrap(),
            GetBookLang {
                lang: "english".to_string(),
                encoding: 11,
            }
        );
    }

    #[test]
    fn test_get_book_rpc_data_to_struct() {
        let gb = r#"(get-book :title "hello world" :version "1984" :lang (get-book-lang :lang "english" :encoding 11) :authors (authors :names '("a")))"#;
        assert_eq!(
            lisp_rpc_from_str::<GetBook>(gb).unwrap(),
            GetBook {
                title: "hello world".to_string(),
                version: "1984".to_string(),
                lang: GetBookLang {
                    lang: "english".to_string(),
                    encoding: 11,
                },
                authors: Authors {
                    names: vec!["a".to_string()],
                },
            }
        );
    }
}
