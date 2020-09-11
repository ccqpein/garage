use crate::plugboard::*;
use crate::rotor::*;
use std::io::{Error, ErrorKind, Result};

#[derive(Debug)]
pub struct EnigmaM<'a> {
    len: usize,
    rc: &'a mut RotorChain<'a>,
    plugboard: Plugboard,
}

impl<'a> EnigmaM<'a> {
    pub fn new(rc: &'a mut RotorChain<'a>, pb: &Plugboard) -> Self {
        Self {
            len: rc.len(),
            rc,
            plugboard: pb.clone(),
        }
    }

    pub fn input(&mut self, i: usize) -> Result<usize> {
        if i >= self.len {
            return Err(Error::new(ErrorKind::InvalidInput, "Beyond the range"));
        }
        let a = self.plugboard.input(i);
        self.rc.input(a)
    }
}
