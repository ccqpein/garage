use crate::tildes::*;
use std::fmt::{Debug, Display};
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};
use std::iter;

/// the control string should including:
/// 1. the whole string
/// 2. the parsed tree
#[derive(Debug)]
struct ControlStr<'a> {
    inner: &'a str,
    tildes: Vec<((usize, usize), Tilde)>,
}

impl<'a> ControlStr<'a> {
    fn new(s: &'a str) -> Result<Self, Box<dyn std::error::Error>> {
        let cc = Cursor::new(s);
        let tildes = Self::scan(cc)?;

        Ok(Self { inner: s, tildes })
    }

    fn scan(
        mut s: Cursor<&'_ str>,
    ) -> Result<Vec<((usize, usize), Tilde)>, Box<dyn std::error::Error>> {
        let mut buf = vec![];
        let mut has_read_len = 0;
        let mut result = vec![];

        loop {
            //dbg!(s.position());
            s.read_until(b'~', &mut buf)?;
            match buf.last() {
                // find the next '~'
                Some(b'~') => {
                    has_read_len += buf.len() - 1;
                    s.seek(SeekFrom::Current(-1))?;
                }
                _ => return Ok(result),
            }

            let t = Tilde::parse(&mut s)?;
            let end_index = has_read_len + t.len();

            result.push(((has_read_len, end_index), t));
            has_read_len = end_index;
            buf.clear();
        }
    }

    fn reveal_tildes<'s, 'cs: 's>(
        &'cs mut self,
        args: impl Iterator<Item = &'s dyn TildeAble>,
    ) -> impl Iterator<Item = Result<String, Box<dyn std::error::Error + 's>>> {
        let mut args = args.collect();
        self.tildes
            .iter_mut()
            .map(move |tilde| tilde.1.reveal_args(&mut args))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_control_str_scan() -> Result<(), Box<dyn std::error::Error>> {
        let case = "hello wor~{~a~}";
        let c = Cursor::new(case);

        assert_eq!(
            ControlStr::scan(c)?,
            vec![(
                (9, 15),
                Tilde::new(
                    6,
                    TildeKind::Loop((vec![Tilde::new(2, TildeKind::Va)], TildeLoopKind::Nil))
                )
            )]
        );

        let case = "~{~5$~}";
        let c = Cursor::new(case);

        assert_eq!(
            ControlStr::scan(c)?,
            vec![(
                (0, 7),
                Tilde::new(
                    7,
                    TildeKind::Loop((
                        vec![Tilde::new(3, TildeKind::Float(Some("5".to_string())))],
                        TildeLoopKind::Nil
                    ))
                )
            )]
        );

        Ok(())
    }

    #[test]
    fn test_reveal_normal_tildes() -> Result<(), Box<dyn std::error::Error>> {
        let case = "hello wor~a";
        let mut cs = ControlStr::new(case)?;
        let arg: &dyn TildeAble = &13_f32;
        dbg!(arg.into_tildekind_va());

        let result: Vec<String> = vec!["13".to_string()];

        assert_eq!(
            result,
            cs.reveal_tildes(vec![arg].into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );
        Ok(())
    }

    #[test]
    fn test_reveal_loop_tildes() -> Result<(), Box<dyn std::error::Error>> {
        let case = "hello wor~{~a~}~a";
        let mut cs = ControlStr::new(case)?;
        let arg0: &dyn TildeAble = &13_f32;
        let arg1: &dyn TildeAble = &14_f32;
        let arg2: &dyn TildeAble = &15_f32;
        let arg00: Vec<&dyn TildeAble> = vec![arg0, arg1];
        let arg: Vec<&dyn TildeAble> = vec![&arg00, arg2];

        let result: Vec<String> = vec!["1314".to_string(), "15".to_string()];
        // dbg!(&cs.tildes[0]);
        // dbg!(cs
        //     .reveal_tildes(arg.into_iter())
        //     .map(|a| a)
        //     .collect::<Vec<_>>());

        //dbg!(&cs);
        assert_eq!(
            result,
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let case = "hello, ~@{~a~^, ~}";
        let mut cs = ControlStr::new(case)?;
        let arg: Vec<&dyn TildeAble> = vec![&1_i64, &2_i64, &3_i64];
        let result: Vec<String> = vec!["1, 2, 3".to_string()];
        assert_eq!(
            result,
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );
        Ok(())
    }

    #[test]
    fn test_reveal_normal_cond_tildes() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~[cero~;uno~;dos~]";
        let mut cs = ControlStr::new(case)?;

        //dbg!(&cs);

        let arg: Vec<&dyn TildeAble> = vec![&0_usize];
        assert_eq!(
            vec!["cero".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        //
        let arg: Vec<&dyn TildeAble> = vec![&1_usize];
        assert_eq!(
            vec!["uno".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        //
        let case = "~[cero~;uno~:;dos~]";
        let mut cs = ControlStr::new(case)?;
        //dbg!(&cs);

        let arg: Vec<&dyn TildeAble> = vec![&0_usize];
        assert_eq!(
            vec!["cero".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let arg: Vec<&dyn TildeAble> = vec![&2_usize];
        assert_eq!(
            vec!["dos".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        //dbg!(&cs);
        let arg: Vec<&dyn TildeAble> = vec![&3_usize];
        assert_eq!(
            vec!["dos".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        //dbg!(&cs);
        let arg: Vec<&dyn TildeAble> = vec![&4_usize];
        assert_eq!(
            vec!["dos".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let arg: Vec<&dyn TildeAble> = vec![&100_usize];
        assert_eq!(
            vec!["dos".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        Ok(())
    }

    #[test]
    fn test_reveal_sharp_cond_tildes() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].";
        let mut cs = ControlStr::new(case)?;
        dbg!(&cs);

        let arg: Vec<&dyn TildeAble> = vec![];
        assert_eq!(
            vec!["NONE".to_string(), "".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        Ok(())
    }

    #[test]
    fn test_reveal_at_cond_tildes() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~@[x = ~a ~]~@[y = ~a~]";
        let mut cs = ControlStr::new(case)?;
        //dbg!(&cs);

        let arg: Vec<&dyn TildeAble> = vec![&Some(&1_i64 as &dyn TildeAble), &None];
        assert_eq!(
            vec!["x = 1 ".to_string(), "".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let case = "~@[x = ~a ~]~@[y = ~a~]";
        let mut cs = ControlStr::new(case)?;
        let arg: Vec<&dyn TildeAble> = vec![
            &Some(&1_i64 as &dyn TildeAble),
            &Some(&2_usize as &dyn TildeAble),
        ];
        assert_eq!(
            vec!["x = 1 ".to_string(), "y = 2".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        Ok(())
    }

    #[test]
    fn test_reveal_loop_cond_combine() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~{~a~#[~;, and ~:;, ~]~}";
        let mut cs = ControlStr::new(case)?;
        //dbg!(&cs);

        let a = vec![&1_i64 as &dyn TildeAble, &2_i64 as &dyn TildeAble];
        let arg: Vec<&dyn TildeAble> = vec![&a];

        assert_eq!(
            vec!["1, and 2".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        //

        let case = "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}";
        let mut cs = ControlStr::new(case)?;
        //dbg!(&cs);
        let a = vec![];
        let arg: Vec<&dyn TildeAble> = vec![&a];
        assert_eq!(
            vec!["".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let mut cs = ControlStr::new(case)?;
        let a = vec![&1_i64 as &dyn TildeAble];
        let arg: Vec<&dyn TildeAble> = vec![&a as &dyn TildeAble];
        assert_eq!(
            vec!["1".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let mut cs = ControlStr::new(case)?;
        let a = vec![&1_i64 as &dyn TildeAble, &2_i64];
        let arg: Vec<&dyn TildeAble> = vec![&a as &dyn TildeAble];
        assert_eq!(
            vec!["1 and 2".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let mut cs = ControlStr::new(case)?;
        let a = vec![&1_i64 as &dyn TildeAble, &2_i64, &3_i64];
        let arg: Vec<&dyn TildeAble> = vec![&a as &dyn TildeAble];
        assert_eq!(
            vec!["1, 2, and 3".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let mut cs = ControlStr::new(case)?;
        let a = vec![&1_i64 as &dyn TildeAble, &2_i64, &3_i64, &4_i64];
        let arg: Vec<&dyn TildeAble> = vec![&a as &dyn TildeAble];
        assert_eq!(
            vec!["1, 2, 3, and 4".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let mut cs = ControlStr::new(case)?;
        let a = vec![&1_i64 as &dyn TildeAble, &2_i64, &3_i64, &4_i64, &5_i64];
        let arg: Vec<&dyn TildeAble> = vec![&a as &dyn TildeAble];
        assert_eq!(
            vec!["1, 2, 3, 4, and 5".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        let case = "~{~#[empty~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}";
        let mut cs = ControlStr::new(case)?;
        //dbg!(&cs);
        let a = vec![];
        let arg: Vec<&dyn TildeAble> = vec![&a];
        assert_eq!(
            vec!["empty".to_string()],
            cs.reveal_tildes(arg.into_iter())
                .map(|a| a.unwrap())
                .collect::<Vec<_>>()
        );

        Ok(())
    }
}
