use std::{
    io::{BufRead, BufReader},
    rc::Rc,
};

#[derive(PartialEq, Debug)]
struct YAMLObject {
    key: String,
    value: V,
}

#[derive(PartialEq, Debug)]
enum V {
    O(Option<Box<YAMLObject>>),
    L(Vec<V>),
    Item(String),
    SingleV(String),
}

#[derive(PartialEq, Debug)]
enum LineStatus {
    Nil,
    OnlyKey(String), // key:
    Value(V),        // - task
}

type Offset = usize;

/// read until the colon and split the buffer on it
fn read_until_colon<R>(mut reader: R, buf: &mut Vec<u8>) -> std::io::Result<()>
where
    R: BufRead,
{
    reader.read_until(b':', buf)?; //:= TODO: need clean the unexpect space around ":"
    Ok(())
}

/// parse a line, line in arguments shouldn't has the \n at the endding
fn parse_a_line<L>(mut line: L, buf: &mut Vec<u8>) -> std::io::Result<(LineStatus, Offset)>
where
    L: BufRead,
{
    read_until_colon(&mut line, buf)?;

    match buf.last() {
        Some(&z) if z == b':' => {
            // this line has key
            // can be key:\n or key:value
            // read to the end of this line
            let (k, offset, _) = trim_space_start_and_end(&buf[0..buf.len() - 1]);

            let k = String::from_utf8(k.to_vec())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

            buf.clear();
            line.read_to_end(buf)?; // read the rest of line

            let (rest, _, _) = trim_space_start_and_end(buf);
            if rest.is_empty() {
                // only key
                return Ok((LineStatus::OnlyKey(k), offset));
            } else {
                let v = parse_value(rest)?;
                return Ok((
                    LineStatus::Value(V::O(Some(box YAMLObject { key: k, value: v }))),
                    offset,
                ));
            }
        }
        Some(_) => {
            let (_, offset, _) = trim_space_start_and_end(&buf[0..buf.len()]);
            // don't find the colon, this line is value
            return Ok((LineStatus::Value(parse_value(&buf)?), offset));
        }
        None => Ok((LineStatus::Nil, 0)),
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
    let mut line_buf = vec![];
    if reader.read_line(line)? == 0 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "empty",
        ));
    };

    *line = line.trim_end().to_string();

    loop {
        match parse_a_line(line.as_bytes(), &mut line_buf)? {
            (LineStatus::Nil, _) => (),
            (LineStatus::OnlyKey(k), offset) => {
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
                line_buf.clear();

                values.push(V::O(Some(box YAMLObject {
                    key: k,
                    value: parser(reader, line, None)?,
                })));

                // line actually already updated by the next line
                // continue jump the new reading at the endding of the loop
                continue;
            }
            (LineStatus::Value(v), offset) => {
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
        }

        line.clear();
        line_buf.clear();

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

    Ok(V::SingleV(String::from_utf8(content.to_vec()).map_err(
        |e| std::io::Error::new(std::io::ErrorKind::InvalidData, e),
    )?))
}

//fn parse_yaml_file(f: File) {}

#[cfg(test)]
mod test {
    use std::io::{BufReader, Cursor};

    use super::*;

    #[test]
    fn test_split_on_colon() {
        // make sure &[u8] can be used
        let mut reader = BufReader::new("aa:a\nbbb".as_bytes());
        let mut buf = Vec::new();
        assert!(reader.read_until(b'\n', &mut buf).is_ok());
        assert_eq!(buf, "aa:a\n".as_bytes());

        let reader = buf.as_slice();
        let mut buf = Vec::new();
        assert!(read_until_colon(reader, &mut buf).is_ok());
        assert_eq!(buf, "aa:".as_bytes());

        // make string can be used
        let s = "aaa:bbb".to_string();
        let mut buf = vec![];
        assert!(read_until_colon(s.as_bytes(), &mut buf).is_ok());
        assert_eq!(buf, "aaa:".as_bytes());
    }

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
    }

    #[test]
    fn test_parse_a_line() {
        let mut buf = vec![];

        let l = r#"doe: "a deer, a female deer""#;
        let (v, offset) = parse_a_line(l.as_bytes(), &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
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
        buf.clear();
        let l = r#"doe: "#;
        let (v, offset) = parse_a_line(l.as_bytes(), &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(s) => s,
            LineStatus::Value(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(v, r#"doe"#);

        //
        buf.clear();
        let l = r#"doe : "#;
        let (v, offset) = parse_a_line(l.as_bytes(), &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(s) => s,
            LineStatus::Value(_) => panic!(),
        };

        assert_eq!(0, offset);
        assert_eq!(v, r#"doe"#);

        //
        buf.clear();
        let l = r#"pi: 3.14159"#;
        let (v, offset) = parse_a_line(l.as_bytes(), &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
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
        let mut buf = vec![];

        let l = r#"  doe: "a deer, a female deer""#;
        let (v, offset) = parse_a_line(l.as_bytes(), &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
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

        buf.clear();
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

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1], &mut buf).unwrap();
        assert_eq!(v, LineStatus::Nil);
        assert_eq!(0, offset);

        // second line
        line.clear();
        buf.clear();
        b.read_line(&mut line).unwrap();

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1], &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
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
        buf.clear();
        b.read_line(&mut line).unwrap();

        let (v, offset) = parse_a_line(&line.as_bytes()[0..line.len() - 1], &mut buf).unwrap();
        let v = match v {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v) => match v {
                V::O(v) => v.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
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
