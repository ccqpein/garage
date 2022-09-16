use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

#[derive(PartialEq, Debug)]
pub struct YAMLObject {
    key: String,
    value: V,
}

impl YAMLObject {
    pub fn new(key: &str, value: V) -> Self {
        Self {
            key: key.to_string(),
            value,
        }
    }
}

impl std::fmt::Display for YAMLObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(:{} {})", self.key, self.value)
    }
}

#[derive(PartialEq, Debug)]
pub enum V {
    O(Option<Box<YAMLObject>>),
    L(Vec<V>),
    Item(String),
    SingleV(String),
}

impl V {
    pub fn new_item(item: &str) -> Self {
        V::Item(item.to_string())
    }

    pub fn new_l(ll: Vec<Self>) -> Self {
        V::L(ll)
    }

    pub fn new_o(o: YAMLObject) -> Self {
        V::O(Some(box o))
    }

    pub fn new_singlev(v: &str) -> Self {
        V::SingleV(v.to_string())
    }
}

impl std::fmt::Display for V {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            V::O(o) => match o {
                Some(oo) => {
                    write!(f, "{}", oo)
                }
                None => Ok(()),
            },
            V::L(l) => {
                write!(f, "(")?;
                for vv in l {
                    write!(f, "{} ", vv)?;
                }
                write!(f, ")")
            }
            V::Item(i) => write!(f, "{}", i),
            V::SingleV(v) => write!(f, "{}", v),
        }
    }
}

fn string_from_u8(s: &[u8]) -> std::io::Result<String> {
    String::from_utf8(s.to_vec())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
}

#[derive(PartialEq, Debug)]
enum LineStatus {
    Nil,
    OnlyKey(String, Option<String>), // key:
    Value(V, Option<String>),        // a line is a V (key-value (yamlobject) or Item)
    Comment(String),
}

type Offset = usize;

/// get all special char index
fn get_special_symbol(line: &[u8], specials: &[u8]) -> Vec<Option<usize>> {
    let mut result = vec![None; specials.len()];
    for (loc, c) in line.iter().enumerate() {
        for (ind, cc) in specials.iter().enumerate() {
            if c == cc {
                result[ind] = Some(loc)
            }
        }
    }
    result
}

/// parse a line, line in arguments shouldn't has the \n at the endding
fn parse_a_line(line: &[u8]) -> std::io::Result<(LineStatus, Offset)> {
    let (colon_loc, sharp_loc) =
        if let [colon_loc, sharp_loc, ..] = get_special_symbol(line, &vec![b':', b'#'])[..] {
            (colon_loc, sharp_loc)
        } else {
            (None, None)
        };

    match (colon_loc, sharp_loc) {
        (None, None) => {
            let (x, offset, _) = trim_space_start_and_end(&line);
            if x.is_empty() {
                Ok((LineStatus::Nil, offset))
            } else {
                Ok((LineStatus::Value(parse_value(&line)?, None), offset))
            }
        }
        (None, Some(s)) => Ok((LineStatus::Comment(string_from_u8(&line[..s])?), 0)),
        (Some(c), None) => {
            let (k, offset, _) = trim_space_start_and_end(&line[0..c]);

            let k = string_from_u8(k)?;

            let (rest, _, _) = trim_space_start_and_end(&line[c + 1..]);
            if rest.is_empty() {
                // only key
                return Ok((LineStatus::OnlyKey(k, None), offset));
            } else {
                let v = parse_value(rest)?;
                return Ok((
                    LineStatus::Value(V::O(Some(box YAMLObject { key: k, value: v })), None),
                    offset,
                ));
            }
        }
        (Some(c), Some(s)) => {
            if s < c {
                let (rest, _, _) = trim_space_start_and_end(&line[s + 1..]);
                let rest = string_from_u8(rest)?;

                Ok((LineStatus::Comment(rest), 0))
            } else {
                let (k, offset, _) = trim_space_start_and_end(&line[0..c]);

                let k = string_from_u8(k)?;

                let (rest, _, _) = trim_space_start_and_end(&line[c + 1..s]);
                let (comment, _, _) = trim_space_start_and_end(&line[s + 1..]);
                if rest.is_empty() {
                    // only key
                    return Ok((
                        LineStatus::OnlyKey(k, Some(string_from_u8(comment)?)),
                        offset,
                    ));
                } else {
                    let v = parse_value(rest)?;
                    return Ok((
                        LineStatus::Value(
                            V::O(Some(box YAMLObject { key: k, value: v })),
                            Some(string_from_u8(comment)?),
                        ),
                        offset,
                    ));
                }
            }
        }
    }
}

fn parser<R>(
    reader: &mut R,
    line: &mut String,
    mut current_offset: Option<Offset>,
) -> std::io::Result<V>
where
    R: BufRead,
{
    let mut values: Vec<V> = vec![];
    if reader.read_line(line)? == 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "empty",
        ));
    };

    *line = line.trim_end().to_string();

    loop {
        match parse_a_line(line.as_bytes())? {
            (LineStatus::Nil, _) => (),
            (LineStatus::OnlyKey(k, _), offset) => {
                match current_offset {
                    Some(lo) => {
                        if offset < lo {
                            // this break will keep the line
                            // which actually is the "next" line
                            break;
                        }
                    }
                    None => current_offset = Some(offset),
                }

                line.clear();

                values.push(V::O(Some(box YAMLObject {
                    key: k,
                    value: parser(reader, line, None)?,
                })));

                // line actually already updated by the next line
                // continue jump the new reading at the endding of the loop
                continue;
            }
            (LineStatus::Value(v, _), offset) => {
                match current_offset {
                    Some(lo) => {
                        if offset < lo {
                            // this break will keep the line
                            // which actually is the "next" line
                            break;
                        } else {
                            values.push(v);
                        }
                    }
                    None => {
                        current_offset = Some(offset);
                        values.push(v)
                    }
                }
            }
            (LineStatus::Comment(_), _) => todo!(), //:= TODO: need find a way make comment line status inside parser()
        }

        line.clear();

        if reader.read_line(line)? == 0 {
            break;
        };

        *line = line.trim_end().to_string();
    }

    Ok(V::L(values))
}

fn trim_space_start_and_end(content: &[u8]) -> (&[u8], usize, usize) {
    let mut i = 0;
    let mut j = 0;

    for ii in 0..content.len() {
        if content[ii] != b' ' {
            i = ii;
            break;
        }
    }

    for jj in (0..content.len()).rev() {
        if content[jj] != b' ' {
            j = jj;
            break;
        }
    }

    if content.is_empty() || (i == j && content[i] == b' ') {
        (&[], i, j)
    } else {
        (&content[i..=j], i, j)
    }
}

fn parse_value(mut content: &[u8]) -> std::io::Result<V> {
    content = trim_space_start_and_end(content).0;
    if content.is_empty() {
        return Ok(V::O(None));
    }

    let content = string_from_u8(content)?;

    if content.starts_with("- ") {
        return Ok(V::Item(content.trim_start_matches("- ").to_string()));
    }

    Ok(V::SingleV(content))
}

pub fn parse_yaml_file(p: impl AsRef<Path>) -> std::io::Result<V> {
    let f = File::open(p)?;

    let mut b = BufReader::new(f);
    let mut line = String::new();
    parser(&mut b, &mut line, None)
}

#[cfg(test)]
mod test {
    use std::io::BufReader;

    use super::*;

    #[test]
    fn test_trim_space_start_and_end() {
        let s0 = " dddd ";
        assert_eq!(trim_space_start_and_end(s0.as_bytes()).0, "dddd".as_bytes());

        let s1 = "  ddddd aaa bbb  ";
        assert_eq!(
            trim_space_start_and_end(s1.as_bytes()).0,
            "ddddd aaa bbb".as_bytes()
        );

        let s2 = "     ";
        assert_eq!(trim_space_start_and_end(s2.as_bytes()).0, "".as_bytes());
    }

    #[test]
    fn test_parse_value() {
        assert_eq!(parse_value(&[]).expect("should success"), V::O(None));
        assert_eq!(
            parse_value("- aaa".as_bytes()).expect("should success"),
            V::Item("aaa".to_string())
        );

        let (con, pre, _) = trim_space_start_and_end("  - aaa ".as_bytes());
        assert_eq!(pre, 2);
        assert_eq!(
            parse_value(con).expect("should success"),
            V::Item("aaa".to_string())
        );
    }

    #[test]
    fn test_parse_a_line() {
        let l = r#"doe: "a deer, a female deer""#;
        let (v, offset) = parse_a_line(l.as_bytes()).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_, _) => panic!(),
            LineStatus::Value(v, _) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(
            v,
            box YAMLObject {
                key: "doe".to_string(),
                value: V::SingleV(r#""a deer, a female deer""#.to_string())
            }
        );

        //
        let l = r#"doe: "#;
        let (v, offset) = parse_a_line(l.as_bytes()).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(s, _) => s,
            LineStatus::Value(_, _) => panic!(),
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(v, r#"doe"#);

        //
        let l = r#"doe : "#;
        let (v, offset) = parse_a_line(l.as_bytes()).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(s, _) => s,
            LineStatus::Value(_, _) => panic!(),
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(v, r#"doe"#);

        //
        let l = r#"pi: 3.14159"#;
        let (v, offset) = parse_a_line(l.as_bytes()).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_, _) => panic!(),
            LineStatus::Value(v, _) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(
            v,
            box YAMLObject {
                key: "pi".to_string(),
                value: V::SingleV(r#"3.14159"#.to_string())
            }
        );
    }

    #[test]
    fn test_parse_a_line_offsets() {
        let l = r#"  doe: "a deer, a female deer""#;
        let (v, offset) = parse_a_line(l.as_bytes()).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_, _) => panic!(),
            LineStatus::Value(v, _) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(2, offset);
        assert_eq!(
            v,
            box YAMLObject {
                key: "doe".to_string(),
                value: V::SingleV(r#""a deer, a female deer""#.to_string())
            }
        );

        //
        //

        let mut b = BufReader::new(
            r#"
    doe: "a deer, a female deer"
  turtle-doves: two
"#
            .as_bytes(),
        );

        let mut line = String::new();

        // first line
        b.read_line(&mut line).unwrap();

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1]).unwrap();
        assert_eq!(v, LineStatus::Nil);
        assert_eq!(0, offset);

        // second line
        line.clear();
        b.read_line(&mut line).unwrap();

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1]).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_, _) => panic!(),
            LineStatus::Value(v, _) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(4, offset);
        assert_eq!(
            v,
            box YAMLObject {
                key: "doe".to_string(),
                value: V::SingleV(r#""a deer, a female deer""#.to_string())
            }
        );

        // third line
        line.clear();
        b.read_line(&mut line).unwrap();

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1]).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_, _) => panic!(),
            LineStatus::Value(v, _) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
            LineStatus::Comment(_) => panic!(),
        };

        assert_eq!(2, offset);
        assert_eq!(
            v,
            box YAMLObject {
                key: "turtle-doves".to_string(),
                value: V::SingleV(r#"two"#.to_string())
            }
        );
    }

    #[test]
    fn test_display_for_v() {
        let v = V::Item(String::from("aaa"));
        assert_eq!("aaa".to_string(), format!("{}", v));

        let v = V::SingleV(String::from("aaa"));
        assert_eq!("aaa".to_string(), format!("{}", v));

        let v = V::L(vec![
            V::Item(String::from("aaa")),
            V::Item(String::from("bbb")),
            V::Item(String::from("ccc")),
        ]);
        assert_eq!("(aaa bbb ccc )".to_string(), format!("{}", v));

        let v = V::O(Some(box YAMLObject {
            key: "key".to_string(),
            value: V::L(vec![
                V::Item(String::from("aaa")),
                V::O(Some(Box::new(YAMLObject {
                    key: "inner_key".to_string(),
                    value: V::SingleV("3.1415926".to_string()),
                }))),
                V::Item(String::from("bbb")),
                V::Item(String::from("ccc")),
            ]),
        }));
        assert_eq!(
            "(:key (aaa (:inner_key 3.1415926) bbb ccc ))".to_string(),
            format!("{}", v)
        );
    }

    #[test]
    fn test_parser() {
        let mut content = BufReader::new(
            r#"
doe: "a deer, a female deer"
pi: 3.14159
xmas-fifth-day:
   calling-birds: four
   french-hens: 3
turtle-doves: two
partridges:
     count: 1
"#
            .as_bytes(),
        );

        //dbg!(parser(&mut content, &mut String::new(), None));
        assert_eq!(
            parser(&mut content, &mut String::new(), None).unwrap(),
            V::L(vec![
                V::O(Some(Box::new(YAMLObject {
                    key: "doe".to_string(),
                    value: V::SingleV(r#""a deer, a female deer""#.to_string())
                }))),
                V::O(Some(Box::new(YAMLObject {
                    key: "pi".to_string(),
                    value: V::SingleV(r#"3.14159"#.to_string())
                }))),
                V::O(Some(Box::new(YAMLObject {
                    key: "xmas-fifth-day".to_string(),
                    value: V::L(vec![
                        V::O(Some(Box::new(YAMLObject {
                            key: "calling-birds".to_string(),
                            value: V::SingleV(r#"four"#.to_string())
                        }))),
                        V::O(Some(Box::new(YAMLObject {
                            key: "french-hens".to_string(),
                            value: V::SingleV(r#"3"#.to_string())
                        })))
                    ])
                }))),
                V::O(Some(Box::new(YAMLObject {
                    key: "turtle-doves".to_string(),
                    value: V::SingleV(r#"two"#.to_string())
                }))),
                V::O(Some(Box::new(YAMLObject {
                    key: "partridges".to_string(),
                    value: V::L(vec![V::O(Some(Box::new(YAMLObject {
                        key: "count".to_string(),
                        value: V::SingleV(r#"1"#.to_string())
                    }))),])
                }))),
            ]),
        );

        //
        //

        let mut content = BufReader::new(
            r#"
xmas-fifth-day:
   french-hens:
     key1:       
       turtle-doves: two
partridges:
     count: 1
"#
            .as_bytes(),
        );

        //dbg!(parser(&mut content, &mut String::new(), None));

        assert_eq!(
            parser(&mut content, &mut String::new(), None).unwrap(),
            V::L(vec![
                V::O(Some(Box::new(YAMLObject {
                    key: "xmas-fifth-day".to_string(),
                    value: V::L(vec![V::O(Some(Box::new(YAMLObject {
                        key: "french-hens".to_string(),
                        value: V::L(vec![V::O(Some(Box::new(YAMLObject {
                            key: "key1".to_string(),
                            value: V::L(vec![V::O(Some(Box::new(YAMLObject {
                                key: "turtle-doves".to_string(),
                                value: V::SingleV(r#"two"#.to_string())
                            })))])
                        }))),])
                    })))])
                }))),
                V::O(Some(Box::new(YAMLObject {
                    key: "partridges".to_string(),
                    value: V::L(vec![V::O(Some(Box::new(YAMLObject {
                        key: "count".to_string(),
                        value: V::SingleV(r#"1"#.to_string())
                    }))),])
                }))),
            ]),
        );
    }
}
