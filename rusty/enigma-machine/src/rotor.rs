use rand::seq::SliceRandom;
use std::io::{Error, ErrorKind, Result};

struct Rotor {
    len: usize,
    input_vec: Vec<u8>,
    output_vec: Vec<u8>,
}

impl Rotor {
    fn init(mut letter_set: Vec<u8>) -> Self {
        let mut rng = rand::thread_rng();
        Self {
            len: letter_set.len(),
            input_vec: letter_set.clone(),
            output_vec: {
                letter_set.shuffle(&mut rng);
                letter_set
            },
        }
    }

    fn len(&self) -> usize {
        self.len
    }

    /// for manually set rotor without randomize input letter
    fn set(&mut self, input: Vec<u8>, output: Vec<u8>) {
        self.input_vec = input;
        self.output_vec = output;
    }

    fn input(&self, i: &u8) -> Result<u8> {
        if let Some(p) = self.input_vec.iter().position(|x| x == i) {
            Ok(self.output_vec[p])
        } else {
            Err(Error::new(
                ErrorKind::NotFound,
                format!("input {} cannot found", i.to_string()),
            ))
        }
    }

    fn spin(&mut self, s: usize) {
        self.input_vec.rotate_left(s);
        self.output_vec.rotate_left(s);
    }

    fn spin_one(&mut self) {
        self.spin(1);
    }
}

impl Default for Rotor {
    fn default() -> Self {
        Self::init("abcdefghijklmnopqrstuvwxyz".to_string().into_bytes())
    }
}

struct RotorChain<'a> {
    spin_status: Vec<usize>,
    chain: Vec<&'a Rotor>,
}

impl<'a> RotorChain<'a> {
    fn new(rotors: Vec<&'a Rotor>) -> Self {
        Self {
            spin_status: rotors.iter().map(|r| r.len()).collect(),
            chain: rotors,
        }
    }
}
