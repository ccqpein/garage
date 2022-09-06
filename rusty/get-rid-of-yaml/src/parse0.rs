use std::{io::BufRead, rc::Rc};

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
            if buf.is_empty() {
                // only key
                return Ok((LineStatus::OnlyKey(k), offset));
            } else {
                let v = parse_value(buf)?;
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

fn parser(
    mut reader: impl BufRead,
    mut line: &mut String,
    last_offset: Offset,
) -> std::io::Result<V> {
    let mut values: Vec<V> = vec![];
    let mut line_buf = vec![];
    reader.read_line(&mut line)?;

    loop {
        match parse_a_line(&mut line.as_bytes(), &mut line_buf)? {
            (LineStatus::Nil, offset) => todo!(),
            (LineStatus::OnlyKey(k), offset) => {
                line.clear();
                values.push(V::O(Some(box YAMLObject {
                    key: k,
                    value: parser(&mut reader, line, offset)?,
                })));
                continue;
            }
            (LineStatus::Value(v), offset) => {
                if offset == last_offset {
                    values.push(v);
                } else {
                    break;
                }
            }
        }
        line.clear();
        reader.read_line(&mut line)?;
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
    use std::io::BufReader;

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
        let l0 = r#"doe: "a deer, a female deer""#;
        let (v0, offset) = parse_a_line(l0.as_bytes(), &mut buf).unwrap();
        let v0 = match v0 {
            LineStatus::Nil => panic!(),
            LineStatus::OnlyKey(_) => panic!(),
            LineStatus::Value(v0) => match v0 {
                V::O(v0) => v0.unwrap(),
                V::L(_) => panic!(),
                V::Item(_) => panic!(),
                V::SingleV(_) => panic!(),
            },
        };

        assert_eq!(0, offset);
        assert_eq!(
            v0,
            box YAMLObject {
                key: "doe".to_string(),
                value: V::SingleV(r#""a deer, a female deer""#.to_string())
            }
        );
    }
}
