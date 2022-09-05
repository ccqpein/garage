use std::io::{BufRead, Split};

/// read until the colon and split the buffer on it
fn read_until_colon<R>(mut reader: R, buf: &mut Vec<u8>) -> std::io::Result<()>
where
    R: BufRead,
{
    reader.read_until(b':', buf)?; //:= TODO: need clean the unexpect space around ":"
    Ok(())
}

enum LineStatus {
    Nil,
    KVPair(String, Value), // key:value
    OnlyKey,               // key:
    Value(Value),          // - task
}

enum Value {
    SingleValue(String), // like the value inside k:value
}

/// parse a line of yaml
fn parse_a_line<L>(mut line: L) -> std::io::Result<LineStatus>
where
    L: BufRead,
{
    let mut buf = vec![];
    read_until_colon(&mut line, &mut buf)?;

    // read successfully
    match buf.last() {
        Some(&z) if z == b':' => {
            // this line has key
            // can be key:\n or key:value
            // read to the end of this line
            let k = String::from_utf8(buf[0..buf.len()].to_vec())
                .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;

            buf.clear();
            line.read_to_end(&mut buf)?;
            let v = parse_value(&buf)?; //:= mutil line start from here, OnlyKey
            return Ok(LineStatus::KVPair(k, v));
        }
        Some(_) => {
            // don't find the colon, this line is value
            buf.clear();
            line.read_to_end(&mut buf)?;
            let v = parse_value(&buf)?;
            return Ok(LineStatus::Value(v));
        }
        None => todo!(),
    }
}

fn parse_value(mut content: &[u8]) -> std::io::Result<Value> {
    content = if let Some(c) = content.strip_prefix(&[b' ']) {
        c // clean the prefix space
    } else {
        content
    };

    content = if let Some(c) = content.strip_suffix(&[b' ']) {
        c // clean the suffix space
    } else {
        content
    };

    Ok(Value::SingleValue(
        String::from_utf8(content.to_vec())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?,
    ))
}

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
}
