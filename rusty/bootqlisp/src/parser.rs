use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Sym(Vec<u8>),
    List(Vec<Atom>),
}

fn read(source_code: &mut VecDeque<u8>) -> Vec<Atom> {
    let mut result = vec![];
    loop {
        if let Some(b) = source_code.get(0) {
            match b {
                b'(' => read_cons(source_code, &mut result),
                b' ' => {}
                _ => {
                    read_sym(source_code, &mut result);
                }
            }
        } else {
            break;
        }
    }
    result
}

fn read_sym(source_code: &mut VecDeque<u8>, buf: &mut Vec<Atom>) {
    let mut sym_buf = vec![];
    loop {
        if let Some(b) = source_code.get(0) {
            match b {
                b' ' | b')' | b'(' => {
                    source_code.pop_front();
                    break;
                }
                _ => sym_buf.push(source_code.pop_front().unwrap()),
            }
        } else {
            break;
        }
    }

    if !sym_buf.is_empty() {
        buf.push(Atom::Sym(sym_buf))
    }
}

fn read_cons(source_code: &mut VecDeque<u8>, buf: &mut Vec<Atom>) {
    source_code.pop_front();
    loop {
        if let Some(b) = source_code.get(0) {
            match b {
                b')' => {
                    source_code.pop_front();
                    break;
                }
                _ => buf.push(Atom::List(read(source_code))),
            }
        } else {
            break;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::VecDeque;

    #[test]
    fn test_read_sym() {
        let mut case = "aaaa bbbb )".bytes().into_iter().collect();
        let mut buf = vec![];
        read_sym(&mut case, &mut buf);

        assert_eq!(
            buf,
            vec![Atom::Sym("aaaa".bytes().into_iter().collect::<Vec<_>>())]
        );

        let mut buf = vec![];
        read_sym(&mut case, &mut buf);
        assert_eq!(
            buf,
            vec![Atom::Sym("bbbb".bytes().into_iter().collect::<Vec<_>>())]
        );
    }

    #[test]
    fn test_read_cons() {
        let mut case = "(aaaa bbbb)".bytes().into_iter().collect();
        let mut buf = vec![];
        read_cons(&mut case, &mut buf);

        assert_eq!(
            buf,
            vec![Atom::List(vec![
                Atom::Sym("aaaa".bytes().into_iter().collect::<Vec<_>>()),
                Atom::Sym("bbbb".bytes().into_iter().collect::<Vec<_>>())
            ])]
        );

        let mut case = "(aaaa (bbbb))".bytes().into_iter().collect();
        let mut buf = vec![];
        read_cons(&mut case, &mut buf);

        assert_eq!(
            buf,
            vec![Atom::List(vec![
                Atom::Sym("aaaa".bytes().into_iter().collect::<Vec<_>>()),
                Atom::List(vec![Atom::Sym(
                    "bbbb".bytes().into_iter().collect::<Vec<_>>()
                )])
            ])]
        );
    }

    #[test]
    fn test_read_and_read_sym() {
        let mut case = "aaaa bbbb".bytes().into_iter().collect();
        assert_eq!(
            read(&mut case),
            vec![
                Atom::Sym("aaaa".bytes().into_iter().collect()),
                Atom::Sym("bbbb".bytes().into_iter().collect())
            ]
        );

        let mut case = "(aaaa (bbbb))".bytes().into_iter().collect();
        let re = read(&mut case);

        assert_eq!(
            re,
            vec![Atom::List(vec![
                Atom::Sym("aaaa".bytes().into_iter().collect::<Vec<_>>()),
                Atom::List(vec![Atom::Sym(
                    "bbbb".bytes().into_iter().collect::<Vec<_>>()
                )])
            ])]
        );
    }
}
