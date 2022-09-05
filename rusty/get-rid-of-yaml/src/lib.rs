use std::io::{BufRead, Split};

/// read until the colon and split the buffer on it
fn split_on_colon<R>(reader: R) -> Split<R>
where
    R: BufRead,
{
    reader.split(b':')
}

//:= TODO: need a function to check the Split return from split_on_colon
//:=

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
        assert_eq!(
            split_on_colon(buf.as_slice()).next().unwrap().unwrap(),
            "aa".as_bytes()
        );

        // make string can be used
        let s = "aaa:bbb";
        assert_eq!(
            split_on_colon(s.as_bytes()).next().unwrap().unwrap(),
            "aaa".as_bytes()
        );
    }
}
