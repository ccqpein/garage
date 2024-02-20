use std::collections::VecDeque;

use std::io::BufRead;
use std::result;

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Sym(Vec<u8>),
    List(Vec<Atom>),
}

//struct Exp {}

// fn read(source_code: &mut VecDeque<u8>, current: Option<Atom>, result: &mut Vec<Atom>) {
//     if let Some(b) = source_code.pop_front() {
//         match current {
//             Some(Atom::Sym(mut s)) => read(
//                 source_code,
//                 Some(Atom::Sym({
//                     s.push(b);
//                     s
//                 })),
//                 result,
//             ),
//             Some(Atom::List(l)) => {}
//             None => read(source_code, Some(Atom::Sym(vec![b])), result),
//         }
//     }
// }

fn read(source_code: &mut VecDeque<u8>) -> Vec<Atom> {
    let mut sym_buf = vec![];
    let mut result = vec![];
    loop {
        if let Some(b) = source_code.pop_front() {
            match b {
                b'(' => {} //:= here
                b' ' => {}
                _ => {
                    sym_buf.push(b);
                    read_sym(source_code, &mut sym_buf);
                    result.push(Atom::Sym(sym_buf.clone()));
                    sym_buf.clear()
                }
            }
        } else {
            break;
        }
    }
    result
}

fn read_sym(source_code: &mut VecDeque<u8>, buf: &mut Vec<u8>) {
    loop {
        if let Some(b) = source_code.pop_front() {
            match b {
                b' ' => break,
                _ => buf.push(b),
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

        assert_eq!(buf, "aaaa".bytes().into_iter().collect::<Vec<_>>());

        let mut buf = vec![];
        read_sym(&mut case, &mut buf);
        assert_eq!(buf, "bbbb".bytes().into_iter().collect::<Vec<_>>());
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
    }
}
