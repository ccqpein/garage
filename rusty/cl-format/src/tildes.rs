use std::collections::VecDeque;
use std::fmt::Debug;
use std::io::{BufRead, Cursor, Read, Seek, SeekFrom};
use std::ops::{DerefMut, Range};

use cl_format_macros::*;
use lazy_static::__Deref;

#[derive(Debug)]
struct TildeError {
    kind: ErrorKind,
    msg: String,
}

impl TildeError {
    fn new(kind: ErrorKind, msg: impl AsRef<str>) -> Self {
        Self {
            kind,
            msg: msg.as_ref().to_string(),
        }
    }
}

impl std::error::Error for TildeError {}

impl std::fmt::Display for TildeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TilderError {:?}: {}", self.kind, self.msg)
    }
}

#[derive(Debug)]
enum ErrorKind {
    ParseError,
    RevealError,
}

#[derive(Debug, PartialEq, Clone)]
enum TildeCondKind {
    Nil(bool),    // ~[, bool for the last ~:;
    Sharp(usize), // ~#[, usize for which one need to use to format
    At,           // ~@[
}

impl TildeCondKind {
    fn to_true(&mut self) {
        match self {
            TildeCondKind::Nil(_) => *self = TildeCondKind::Nil(true),
            TildeCondKind::Sharp(_) => (),
            TildeCondKind::At => (),
        }
    }
}

#[derive(PartialEq, Debug)]
enum CatchCount {
    N(usize),
    R(Vec<usize>),
}

impl CatchCount {
    fn as_n(&self) -> Result<usize, Box<dyn std::error::Error>> {
        if let Self::N(a) = self {
            Ok(*a)
        } else {
            Err("cannot as N".into())
        }
    }

    fn max(&self) -> Result<usize, Box<dyn std::error::Error>> {
        if let Self::R(a) = self {
            Ok(*a.iter().max().unwrap_or(&0))
        } else {
            Err("cannot as R to find the max".into())
        }
    }
}

impl From<usize> for CatchCount {
    fn from(value: usize) -> Self {
        Self::N(value)
    }
}

impl From<Vec<usize>> for CatchCount {
    fn from(value: Vec<usize>) -> Self {
        Self::R(value)
    }
}

impl From<&'_ [usize]> for CatchCount {
    fn from(value: &'_ [usize]) -> Self {
        Self::R(value.to_vec())
    }
}

#[derive(Debug)]
struct TildeNil;

#[derive(Debug, PartialEq, TildeAble, Clone)]
pub enum TildeKind {
    #[implTo(char)]
    /// ~C ~:C
    Char,

    /// ~$ ~5$ ~f
    Float(Option<String>),

    /// ~d ~:d ~:@d
    Digit(Option<String>),

    //:= TODO: ~S
    //:= TODO: ~C
    //:= TODO: ~X
    //:= TODO: ~O
    #[implTo(f32, char, String, TildeNil, i64)]
    /// ~a
    Va,

    /// loop
    Loop(Vec<Tilde>),

    #[implTo(usize)]
    /// ~[ ~] condition
    Cond((Vec<Tilde>, TildeCondKind)),

    /// text inside the tilde
    Text(String),

    /// vec
    #[implTo(TildeNil)]
    VecTilde(Vec<Tilde>),
}

impl TildeKind {
    /// find how many args this tilde kind need
    pub fn catch_able(&self) -> Result<CatchCount, Box<dyn std::error::Error>> {
        match self {
            TildeKind::Char => Ok(1.into()),
            TildeKind::Float(_) => Ok(1.into()),
            TildeKind::Digit(_) => Ok(1.into()),
            TildeKind::Va => Ok(1.into()),
            TildeKind::Loop(_) => Ok(1.into()),
            TildeKind::Cond((vv, TildeCondKind::Nil(_))) => Ok(1.into()),
            TildeKind::Cond((vv, a @ TildeCondKind::Sharp(_))) => Ok(vv
                .iter()
                .map(|v| v.catch_able().unwrap().as_n().unwrap())
                .collect::<Vec<_>>()
                .into()),

            TildeKind::Text(_) => Ok(0.into()),
            TildeKind::VecTilde(vv) => {
                let mut s = 0;
                for v in vv {
                    s += v.catch_able()?.as_n()?;
                }
                Ok(s.into())
            }
            _ => todo!(),
        }
    }

    pub fn match_reveal(
        &mut self,
        arg: &dyn TildeAble,
    ) -> Result<String, Box<dyn std::error::Error>> {
        match self {
            TildeKind::Char => todo!(),
            TildeKind::Float(_) => todo!(),
            TildeKind::Digit(_) => todo!(),
            TildeKind::Va => {
                //dbg!(arg);
                //dbg!(&self);
                let a = arg.into_tildekind_va().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Va").into(),
                )?;

                return a.format(self);
            }
            TildeKind::Loop(_) => {
                let a = arg.into_tildekind_loop().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Loop").into(),
                )?;

                return a.format(self);
            }
            TildeKind::Text(s) => Ok(s.to_string()),
            TildeKind::VecTilde(_) => {
                let a = arg.into_tildekind_vectilde().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to VecTilde").into(),
                )?;

                return a.format(self);
            }
            TildeKind::Cond((_, _)) => {
                let a = arg.into_tildekind_cond().ok_or::<TildeError>(
                    TildeError::new(ErrorKind::RevealError, "cannot reveal to Cond").into(),
                )?;
                return a.format(self);
            }
        }
    }
}

/// impl mamually
impl TildeAble for Vec<&dyn TildeAble> {
    fn len(&self) -> usize {
        self.len()
    }

    fn into_tildekind_va(&self) -> Option<&dyn TildeKindVa> {
        Some(self)
    }

    fn into_tildekind_loop(&self) -> Option<&dyn TildeKindLoop> {
        Some(self)
    }

    fn into_tildekind_cond(&self) -> Option<&dyn TildeKindCond> {
        Some(self)
    }

    fn into_tildekind_vectilde(&self) -> Option<&dyn TildeKindVecTilde> {
        Some(self)
    }
}

////
////
/// impl, re-define the format method for over writing the default method
impl TildeKindChar for char {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        if let TildeKind::Char = tkind {
            Ok(format!("'{}'", self))
        } else {
            Err(TildeError::new(ErrorKind::RevealError, "cannot format to Char").into())
        }
    }
}

impl TildeKindVa for i64 {
    fn format(&self, _: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", self))
    }
}

impl TildeKindVa for f32 {
    fn format(&self, _: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", self))
    }
}

impl TildeKindVa for String {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", self))
    }
}

impl TildeKindVa for char {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok(format!("{}", self))
    }
}

impl TildeKindVa for TildeNil {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        Ok("nil".into())
    }
}

impl TildeKindVa for Vec<&dyn TildeAble> {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::Va => {
                tkind.match_reveal(*self.get(0).ok_or::<String>("empty vec to get".into())?)
            }
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to Va").into()),
        }
    }
}

impl TildeKindLoop for Vec<&dyn TildeAble> {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            // self[0] is the Vec<&dyn TildeAble> of loop
            TildeKind::Loop(vv) => {
                //dbg!(&self);
                //dbg!(&tkind);
                let rest_len = self[0].len();
                let one_loop_catch_num = vv
                    .iter()
                    .map(|v| v.catch_able().unwrap().as_n().unwrap())
                    .sum::<usize>();

                let mut new_vv = vec![];
                for _ in 0..rest_len / one_loop_catch_num {
                    let mut temp = vv.clone();
                    new_vv.append(&mut temp)
                }

                let mut k = TildeKind::VecTilde(new_vv.to_vec());

                k.match_reveal(self[0])
            }
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to Loop").into()),
        }
    }
}

impl TildeKindCond for usize {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::Cond((vv, TildeCondKind::Nil(true))) => match vv.get_mut(*self) {
                Some(tt) => tt.reveal(&TildeNil),
                None => {
                    let last = vv.len() - 1;
                    match vv.get_mut(last) {
                        Some(tt) => tt.reveal(&TildeNil),
                        None => Ok(String::new()),
                    }
                }
            },
            TildeKind::Cond((vv, TildeCondKind::Nil(false))) => match vv.get_mut(*self) {
                Some(tt) => tt.reveal(&TildeNil),
                None => Ok(String::new()),
            },
            //:= more TildeCondKind below
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to Cond").into()),
        }
    }
}

impl TildeKindCond for Vec<&dyn TildeAble> {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::Cond((vv, TildeCondKind::Sharp(ind))) => {
                //dbg!(&vv[*ind]);
                //dbg!(self);
                (vv[*ind]).reveal(self)
            }
            TildeKind::Cond((vv, TildeCondKind::Nil(_))) => tkind.match_reveal(self[0]),
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to VecTilde").into()),
        }
    }
}

impl TildeKindVecTilde for TildeNil {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        match tkind {
            TildeKind::VecTilde(vv) => {
                let mut result = vec![];
                for t in vv {
                    result.push(t.reveal(self)?);
                }
                Ok(result.as_slice().concat())
            }
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to VecTilde").into()),
        }
    }
}

impl TildeKindVecTilde for Vec<&dyn TildeAble> {
    fn format(&self, tkind: &mut TildeKind) -> Result<String, Box<dyn std::error::Error>> {
        //dbg!(self);
        match tkind {
            TildeKind::VecTilde(vv) => {
                let mut result = vec![];
                let mut start = 0;
                for v in vv {
                    match v.catch_able()? {
                        CatchCount::N(n) if n == 0 => result.push(v.reveal(&TildeNil)?),
                        CatchCount::N(n) => {
                            let aa = self.get(start..start + n).unwrap().to_vec();
                            //dbg!(&aa);
                            result.push(v.reveal(&aa)?);
                            start += n;
                        }
                        CatchCount::R(_) => todo!(),
                    }
                }

                Ok(result.as_slice().concat())
            }
            _ => Err(TildeError::new(ErrorKind::RevealError, "cannot format to VecTilde").into()),
        }
    }
}

/*=========================================================*/

/// The tilde struct
#[derive(Debug, PartialEq, Clone)]
pub struct Tilde {
    len: usize,
    value: TildeKind,
}

impl Tilde {
    pub fn new(len: usize, value: TildeKind) -> Self {
        Self { len, value }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    /// return how many args this tilde can catch
    pub fn catch_able(&self) -> Result<CatchCount, Box<dyn std::error::Error>> {
        self.value.catch_able()
    }

    pub fn reveal(&mut self, arg: &dyn TildeAble) -> Result<String, Box<dyn std::error::Error>> {
        self.value.match_reveal(arg)
    }

    /// entry function from outside, groups args to tilde
    pub fn reveal_args<'a>(
        &mut self,
        args: &mut Vec<&dyn TildeAble>, //:= can be &[]?
    ) -> Result<String, Box<dyn std::error::Error>> {
        let rest_args_count = args.len();

        // the count of args that catch
        let can_catch = match &self.catch_able()? {
            CatchCount::N(a) => *a,
            cr @ CatchCount::R(rr) => match &mut self.value {
                TildeKind::Cond((vv, a @ TildeCondKind::Sharp(_))) => {
                    if rest_args_count > cr.max()? {
                        *a = TildeCondKind::Sharp(vv.len() - 1);
                        *rr.last().unwrap()
                    } else {
                        let it = rr.iter().enumerate().map(|(ind, v)| (ind, v));

                        let mut largest = 0;
                        let mut largest_ind = 0;
                        for (ind, i) in it {
                            if *i > largest && *i <= rest_args_count {
                                largest = *i;
                                largest_ind = ind;
                            }
                        }
                        *a = TildeCondKind::Sharp(largest_ind);
                        largest
                    }
                }
                _ => *rr
                    .iter()
                    .filter(|r| **r <= rest_args_count)
                    .max()
                    .ok_or::<String>("cannot get the largest catch number".into())?,
            },
        };

        let a = args.drain(0..can_catch);
        //dbg!(&a);
        self.value.match_reveal(&a.collect::<Vec<_>>())
    }

    /*
    ===============================
    parse functions below
    ===============================
    */

    /// start from '~' to the key char of tilde kind
    fn scan_for_kind(
        c: &mut Cursor<&'_ str>,
    ) -> Result<
        Box<
            dyn for<'a, 'b> Fn(
                &'a mut std::io::Cursor<&'b str>,
            )
                -> Result<Tilde, Box<(dyn std::error::Error + 'static)>>,
        >,
        Box<dyn std::error::Error>,
    > {
        let mut buf = [0u8; 1];
        c.read(&mut buf)?;
        if buf[0] != b'~' {
            return Err(TildeError::new(ErrorKind::ParseError, "should start with ~").into());
        }

        // read until the tilde key char
        let mut buf = [0_u8; 3];
        let mut buf_offset = 1;
        c.read(&mut buf)?;
        for b in buf {
            if b == 0_u8 {
                break;
            } else {
                buf_offset += 1;
            }
        }

        match buf {
            [b'a', ..] | [b'A', ..] => {
                c.seek(SeekFrom::Current(-buf_offset))?; // back to start
                return Ok(box Self::parse_value);
            }
            [b'{', ..] => {
                c.seek(SeekFrom::Current(-buf_offset))?; // back to start
                return Ok(box Self::parse_loop);
            }
            [b'$', ..] | [b'f', ..] | [b'F', ..] | [_, b'$', ..] => {
                c.seek(SeekFrom::Current(-buf_offset))?; // back to start
                return Ok(box Self::parse_float);
            }
            [b'd', ..] | [b'D', ..] => {
                c.seek(SeekFrom::Current(-buf_offset))?; // back to start
                return Ok(box Self::parse_digit);
            }
            [b'[', ..] | [b'#', b'[', ..] | [b':', b'[', ..] => {
                c.seek(SeekFrom::Current(-buf_offset))?; // back to start
                return Ok(box Self::parse_cond);
            }
            _ => {
                return Err(
                    TildeError::new(ErrorKind::ParseError, "cannot find the key tilde").into(),
                )
            }
        }
    }

    /// cursor should located on '~'
    pub fn parse(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let parser = Self::scan_for_kind(c)?;
        parser(c)
    }

    /// parse function for '~{~}'
    fn parse_loop(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut char_buf = [0u8; 2]; // two bytes
        c.read(&mut char_buf)?;
        if let Ok(s) = std::str::from_utf8(&char_buf) && s != "~{" {
			c.seek(SeekFrom::Current(-2))?; // restore the location
            return Err(
				TildeError::new(
					ErrorKind::ParseError,
					"should start with ~{",
				).into());
        }

        let mut result = vec![];
        let mut buf = vec![];
        let mut total_len = 2;

        loop {
            // read text until the next '~'
            c.read_until(b'~', &mut buf)?;

            match buf.as_slice() {
                [b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                }
                [.., b'~'] => {
                    c.seek(SeekFrom::Current(-1))?;
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len() - 1;
                }
                [..] => {
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len();
                    return Ok(Tilde::new(total_len, TildeKind::Loop(result)));
                }
            }

            c.read(&mut char_buf)?;

            if let Ok(s) = std::str::from_utf8(&char_buf) && s == "~}" {
				return Ok(Tilde::new(total_len + 2, TildeKind::Loop(result)));
			}

            c.seek(SeekFrom::Current(-2))?;
            //dbg!(c.position());
            // read the tilde
            let next = Tilde::parse(c)?;
            total_len += next.len;
            result.push(next);

            buf.clear()
        }
    }

    /// parse the '~[~]'
    fn parse_cond(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        //dbg!(c.get_ref().to_string());
        let mut char_buf = [0u8; 3]; // three bytes
        let mut total_len = 0;
        let mut cond_kind;
        c.read(&mut char_buf)?;

        match char_buf {
            [b'~', b'[', ..] => {
                total_len += 2;
                cond_kind = TildeCondKind::Nil(false);
                c.seek(SeekFrom::Current(-1))?;
            }
            [b'~', b'#', b'['] => {
                total_len += 3;
                cond_kind = TildeCondKind::Sharp(0);
            }
            [b'~', b'@', b'['] => {
                total_len += 3;
                cond_kind = TildeCondKind::At;
            }
            _ => {
                c.seek(SeekFrom::Current(-3))?;
                return Err(TildeError::new(
                    ErrorKind::ParseError,
                    "should start with ~[, ~#[, ~@[",
                )
                .into());
            }
        }

        let mut bucket = vec![];
        let mut one_byte = [0_u8; 1];
        loop {
            c.read_exact(&mut one_byte)?;
            if let Some(x) = bucket.last() && *x == b'~' && one_byte[0] == b']'{
				bucket.pop(); // bucket doesn't have ~] inside
				break;
			}
            bucket.push(one_byte[0]);
        }

        total_len += 2 + bucket.len();

        let mut result = vec![];
        let mut inner_c = Cursor::new(std::str::from_utf8(&bucket)?);

        let mut ind = 0;
        let mut delimiters = vec![];
        loop {
            if ind == bucket.len() {
                delimiters.push(ind);
                break;
            }
            if bucket[ind] == b';' {
                match bucket.get(0..ind) {
                    Some([.., b'~', b':']) => {
                        cond_kind.to_true();
                        delimiters.push(ind - 2);
                        delimiters.push(ind + 1);
                    }
                    Some([.., b'~']) => {
                        delimiters.push(ind - 1);
                        delimiters.push(ind + 1);
                    }
                    _ => (),
                }
            }

            ind += 1;
        }

        //dbg!(&delimiters);

        for x in delimiters
            .iter()
            .enumerate()
            .map(|(ind, v)| {
                if ind == 0 {
                    *v
                } else {
                    v - delimiters[ind - 1]
                }
            })
            .collect::<Vec<_>>()
            .as_slice()
            .chunks(2)
        {
            //dbg!(&x);
            result.push(Self::parse_vec(&mut inner_c, x[0])?);
            if let Some(offset) = x.get(1) {
                inner_c.seek(SeekFrom::Current(*offset as i64))?;
            }
        }

        Ok(Tilde::new(total_len, TildeKind::Cond((result, cond_kind))))
    }

    fn parse_vec(
        c: &mut Cursor<&'_ str>,
        end_len: usize,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let mut bucket = vec![0; end_len].into_boxed_slice();
        c.read_exact(&mut bucket)?;

        let ss = String::from_utf8(bucket.as_ref().to_vec())?;
        //dbg!(&ss);
        let mut inner_c = Cursor::new(ss.as_str());
        let mut buf = vec![];
        let mut result = vec![];
        let mut total_len = 0;

        loop {
            // read text until the next '~'
            inner_c.read_until(b'~', &mut buf)?;

            match buf.as_slice() {
                [b'~'] => {
                    inner_c.seek(SeekFrom::Current(-1))?;
                }
                [.., b'~'] => {
                    inner_c.seek(SeekFrom::Current(-1))?;
                    result.push(Tilde::new(
                        buf.len() - 1,
                        TildeKind::Text(String::from_utf8(buf[..buf.len() - 1].to_vec())?),
                    ));
                    total_len += buf.len() - 1;
                }
                [] => {
                    c.seek(SeekFrom::Current((end_len - total_len) as i64 * -1))?;
                    return Ok(Tilde::new(total_len, TildeKind::VecTilde(result)));
                }
                [..] => {
                    result.push(Tilde::new(
                        buf.len(),
                        TildeKind::Text(String::from_utf8(buf[..buf.len()].to_vec())?),
                    ));
                    total_len += buf.len();
                    c.seek(SeekFrom::Current((end_len - total_len) as i64 * -1))?;
                    return Ok(Tilde::new(total_len, TildeKind::VecTilde(result)));
                }
            }

            let next = Tilde::parse(&mut inner_c)?;
            total_len += next.len;
            result.push(next);

            buf.clear()
        }
    }

    /// parse function for '~a'
    fn parse_value(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];
        //:= this for in maybe re-write in helper function
        for t in [b'a', b'A'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => return Ok(Tilde::new(buf.len(), TildeKind::Va)),
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "should start with ~a").into())
    }

    /// parse the float
    fn parse_float(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];

        for t in [b'$', b'f', b'F'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => {
                    return Ok(Tilde::new(
                        buf.len(),
                        TildeKind::Float(Some(String::from_utf8(
                            buf.get(1..buf.len() - 1).map_or(Vec::new(), |s| s.to_vec()),
                        )?)),
                    ))
                }
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "cannot find the '$' or 'f'").into())
    }

    /// parse the digit
    fn parse_digit(c: &mut Cursor<&'_ str>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut buf = vec![];

        for t in [b'd', b'D'] {
            c.read_until(t, &mut buf)?;
            match buf.last() {
                Some(b) if *b == t => {
                    return Ok(Tilde::new(
                        buf.len(),
                        TildeKind::Float(Some(String::from_utf8(
                            buf.get(1..buf.len() - 1).map_or(Vec::new(), |s| s.to_vec()),
                        )?)),
                    ))
                }
                _ => (),
            }
            c.seek(SeekFrom::Current(-(buf.len() as i64)))?;
            buf.clear();
        }
        Err(TildeError::new(ErrorKind::ParseError, "cannot find the '$' or 'f'").into())
    }

    //:= TODO: a lot parse functions below
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cursor() {
        let mut testcase0 = Cursor::new("abcd");
        assert_eq!(testcase0.position(), 0);

        let mut buffer: [u8; 1] = [0; 1];
        testcase0.set_position(2);
        let _ = testcase0.read(&mut buffer);
        assert_eq!(buffer[0], b'c');
    }

    #[test]
    fn test_parse_va() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~a");
        assert_eq!(Tilde::parse_value(&mut case)?, Tilde::new(2, TildeKind::Va));

        let mut case = Cursor::new("~A");
        assert_eq!(Tilde::parse_value(&mut case)?, Tilde::new(2, TildeKind::Va));
        Ok(())
    }

    #[test]
    fn test_parse_loop() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~{~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(4, TildeKind::Loop(Vec::new()))
        );

        let mut case = Cursor::new("~{a bc~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                8,
                TildeKind::Loop(vec![Tilde {
                    len: 4,
                    value: TildeKind::Text(String::from("a bc"))
                }])
            ),
        );

        let mut case = Cursor::new("~{a bc~a~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                10,
                TildeKind::Loop(vec![
                    Tilde {
                        len: 4,
                        value: TildeKind::Text(String::from("a bc"))
                    },
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    }
                ])
            )
        );

        let mut case = Cursor::new("~{~aa bc~a~}");

        assert_eq!(
            Tilde::parse_loop(&mut case)?,
            Tilde::new(
                12,
                TildeKind::Loop(vec![
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    },
                    Tilde {
                        len: 4,
                        value: TildeKind::Text(String::from("a bc"))
                    },
                    Tilde {
                        len: 2,
                        value: TildeKind::Va,
                    }
                ])
            )
        );

        Ok(())
    }

    #[test]
    fn test_parse_float() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~$");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(2, TildeKind::Float(Some(String::new())))
        );

        let mut case = Cursor::new("~5$");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(3, TildeKind::Float(Some("5".to_string())))
        );

        let mut case = Cursor::new("~,5f");
        assert_eq!(
            Tilde::parse_float(&mut case)?,
            Tilde::new(4, TildeKind::Float(Some(",5".to_string())))
        );

        Ok(())
    }

    #[test]
    fn test_scan_for_kind() -> Result<(), Box<dyn std::error::Error>> {
        let case = "~a";
        let mut c = Cursor::new(case);
        let f = Tilde::scan_for_kind(&mut c)?;

        // let mut ss = String::new();
        // c.read_to_string(&mut ss);
        // dbg!(ss);
        // c.seek(SeekFrom::Start(0));
        //dbg!(f(&mut c));

        assert_eq!(Tilde::new(2, TildeKind::Va), f(&mut c)?);
        Ok(())
    }

    #[test]
    fn test_parse_vec() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~a and ~a~a~a");
        assert_eq!(
            Tilde::parse_vec(&mut case, 9)?,
            Tilde::new(
                9,
                TildeKind::VecTilde(vec![
                    Tilde {
                        len: 2,
                        value: TildeKind::Va
                    },
                    Tilde {
                        len: 5,
                        value: TildeKind::Text(String::from(" and "))
                    },
                    Tilde {
                        len: 2,
                        value: TildeKind::Va
                    }
                ])
            )
        );

        //dbg!(&case.position());
        let mut rest = vec![];
        case.read_to_end(&mut rest)?;
        assert_eq!(String::from_utf8(rest)?, "~a~a");

        //
        let mut case = Cursor::new("~a a");
        assert!(Tilde::parse_vec(&mut case, 9).is_err());

        //
        let mut case = Cursor::new("a");
        assert_eq!(
            Tilde::parse_vec(&mut case, 1)?,
            Tilde::new(
                1,
                TildeKind::VecTilde(vec![Tilde {
                    len: 1,
                    value: TildeKind::Text(String::from("a"))
                }])
            )
        );

        Ok(())
    }

    #[test]
    fn test_parse_cond() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~[~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                4,
                TildeKind::Cond((
                    vec![Tilde {
                        len: 0,
                        value: TildeKind::VecTilde(vec![])
                    }],
                    TildeCondKind::Nil(false)
                ))
            )
        );

        let mut case = Cursor::new("~[cero~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                8,
                TildeKind::Cond((
                    vec![Tilde {
                        len: 4,
                        value: TildeKind::VecTilde(vec![Tilde::new(
                            4,
                            TildeKind::Text(String::from("cero"))
                        )]),
                    }],
                    TildeCondKind::Nil(false)
                ))
            ),
        );

        let mut case = Cursor::new("~[cero~;uno~;dos~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                18,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                4,
                                TildeKind::Text(String::from("cero"))
                            )])
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                3,
                                TildeKind::Text(String::from("uno"))
                            )])
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                3,
                                TildeKind::Text(String::from("dos"))
                            )])
                        },
                    ],
                    TildeCondKind::Nil(false)
                ))
            )
        );

        let mut case = Cursor::new("~[cero~;uno~;~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                15,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                4,
                                TildeKind::Text(String::from("cero"))
                            )])
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                3,
                                TildeKind::Text(String::from("uno"))
                            )])
                        },
                        Tilde {
                            len: 0,
                            value: TildeKind::VecTilde(vec![])
                        },
                    ],
                    TildeCondKind::Nil(false)
                ))
            )
        );

        let mut case = Cursor::new("~[cero~;uno~:;dos~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                19,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                4,
                                TildeKind::Text(String::from("cero"))
                            )])
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                3,
                                TildeKind::Text(String::from("uno"))
                            )])
                        },
                        Tilde {
                            len: 3,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                3,
                                TildeKind::Text(String::from("dos"))
                            )])
                        },
                    ],
                    TildeCondKind::Nil(true)
                ))
            )
        );

        let mut case = Cursor::new("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]");

        assert_eq!(
            Tilde::parse_cond(&mut case)?,
            Tilde::new(
                33,
                TildeKind::Cond((
                    vec![
                        Tilde {
                            len: 4,
                            value: TildeKind::VecTilde(vec![Tilde::new(
                                4,
                                TildeKind::Text(String::from("NONE"))
                            )])
                        },
                        Tilde {
                            len: 2,
                            value: TildeKind::VecTilde(vec![Tilde::new(2, TildeKind::Va)])
                        },
                        Tilde {
                            len: 9,
                            value: TildeKind::VecTilde(vec![
                                Tilde {
                                    len: 2,
                                    value: TildeKind::Va
                                },
                                Tilde {
                                    len: 5,
                                    value: TildeKind::Text(String::from(" and "))
                                },
                                Tilde {
                                    len: 2,
                                    value: TildeKind::Va,
                                }
                            ]),
                        },
                        Tilde {
                            len: 6,
                            value: TildeKind::VecTilde(vec![
                                Tilde {
                                    len: 2,
                                    value: TildeKind::Va
                                },
                                Tilde {
                                    len: 2,
                                    value: TildeKind::Text(String::from(", "))
                                },
                                Tilde {
                                    len: 2,
                                    value: TildeKind::Va,
                                }
                            ]),
                        },
                    ],
                    TildeCondKind::Sharp(0)
                ))
            )
        );

        let mut case = Cursor::new("~#[NONE~;~a~;~a and ~a~:;~a, ~a~]");
        assert_eq!(
            Tilde::parse_cond(&mut case)?.catch_able()?,
            vec![0_usize, 1, 2, 2].into()
        );

        Ok(())
    }

    #[test]
    fn test_reveal_cond() -> Result<(), Box<dyn std::error::Error>> {
        let mut case = Cursor::new("~[cero~;uno~:;dos~]");
        let mut t = Tilde::parse_cond(&mut case)?;
        let mut args: Vec<&dyn TildeAble> = vec![&0_usize];
        //dbg!(t.reveal_args(args.into_iter()));
        //dbg!(t.reveal_args(&mut args));
        assert_eq!("cero".to_string(), t.reveal_args(&mut args).unwrap());

        let case = Cursor::new("~#[NONE~;first: ~a~;~a and ~a~:;~a, ~a~]");
        let mut t = Tilde::parse_cond(&mut case.clone())?;
        let mut args: Vec<&dyn TildeAble> = vec![&1_i64];
        //dbg!(t.reveal_args(&mut args));
        assert_eq!("first: 1".to_string(), t.reveal_args(&mut args).unwrap());

        let mut t = Tilde::parse_cond(&mut case.clone())?;
        let mut args: Vec<&dyn TildeAble> = vec![&2_i64, &2_i64];
        //dbg!(t.reveal_args(&mut args));
        assert_eq!("2 and 2".to_string(), t.reveal_args(&mut args).unwrap());

        let mut t = Tilde::parse_cond(&mut case.clone())?;
        let mut args: Vec<&dyn TildeAble> = vec![&3_i64, &3_i64, &3_i64];
        //dbg!(t.reveal_args(&mut args));
        assert_eq!("3, 3".to_string(), t.reveal_args(&mut args).unwrap());
        dbg!(args);

        Ok(())
    }
}
