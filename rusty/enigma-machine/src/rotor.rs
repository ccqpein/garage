use rand::seq::SliceRandom;
use std::io::{Error, ErrorKind, Result};

#[derive(Clone, Debug)]
struct Rotor {
    len: usize,
    output_vec: Vec<usize>,
}

impl Rotor {
    fn init(mut contactor_len: u32) -> Result<Self> {
        if contactor_len % 2 != 0 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "input numbers has to be even",
            ));
        }

        let mut rng = rand::thread_rng();
        let mut contactors: Vec<usize> =
            (0..contactor_len).into_iter().map(|x| x as usize).collect();

        Ok(Self {
            len: contactor_len as usize,
            output_vec: {
                contactors.shuffle(&mut rng);
                contactors
            },
        })
    }

    fn len(&self) -> usize {
        self.len
    }

    /// for manually set rotor without randomize
    fn set(&mut self, output: Vec<usize>) -> Result<()> {
        if output.len() != output.len() {
            return Err(Error::new(
                ErrorKind::NotFound,
                "input length does not match output length",
            ));
        }

        self.len = output.len();
        self.output_vec = output;

        Ok(())
    }

    fn input(&self, i: usize) -> Result<usize> {
        if i >= self.len() {
            Err(Error::new(
                ErrorKind::NotFound,
                format!("input {} cannot found", i.to_string()),
            ))
        } else {
            Ok(self.output_vec[i])
        }
    }

    fn rev_input(&self, i: usize) -> Result<usize> {
        if let Some(p) = self.output_vec.iter().position(|x| *x == i) {
            Ok(p)
        } else {
            Err(Error::new(
                ErrorKind::NotFound,
                format!("input {} cannot found", i.to_string()),
            ))
        }
    }

    fn input_with_offset(&self, i: usize, offset: usize) -> Result<usize> {
        let a = (i + offset) % self.len();
        let mut re = self.input(a).unwrap() as i32 - offset as i32;

        if re < 0 {
            re += self.len() as i32;
        }
        Ok(re as usize)
    }

    fn rev_input_with_offset(&self, i: usize, offset: usize) -> Result<usize> {
        let a = (i + offset) % self.len();
        let mut re = self.rev_input(a).unwrap() as i32 - offset as i32;
        if re < 0 {
            re += self.len() as i32;
        }
        Ok(re as usize)
    }

    // fn spin(&mut self, s: usize) {
    //     self.output_vec.rotate_left(s);
    // }

    // fn spin_one(&mut self) {
    //     self.spin(1);
    // }
}

impl Default for Rotor {
    fn default() -> Self {
        Self::init(26).unwrap()
    }
}

#[derive(Debug)]
struct Reflector {
    input: Vec<usize>,
    output: Vec<usize>,
}

impl Reflector {
    /// reflector input and output should be if A->B, then B->A.
    /// diff than Rotor. Rotor can be A->B, B->C, C->A, and follow A<-B
    /// that's why the output is input's reverse
    pub fn init(mut length: usize) -> Result<Self> {
        if length % 2 != 0 {
            return Err(Error::new(
                ErrorKind::InvalidData,
                "input numbers has to be even",
            ));
        }

        let mut rng = rand::thread_rng();
        let mut ll = (0..length).into_iter().collect::<Vec<usize>>();
        ll.shuffle(&mut rng);

        Ok(Self {
            input: ll.clone(),
            output: {
                ll.reverse();
                ll
            },
        })
    }

    fn input(&self, i: usize) -> Result<usize> {
        if let Some(p) = self.input.iter().position(|x| *x == i) {
            Ok(self.output[p])
        } else {
            Err(Error::new(
                ErrorKind::NotFound,
                format!("input {} cannot found", i.to_string()),
            ))
        }
    }

    fn len(&self) -> usize {
        self.input.len()
    }
}

#[derive(Debug)]
struct RotorChain<'a> {
    spin_status: Vec<usize>,
    chain: Vec<&'a mut Rotor>,
    reflect: &'a Reflector,
}

impl<'a> RotorChain<'a> {
    fn new(rotors: Vec<&'a mut Rotor>, reflect: &'a Reflector) -> Result<Self> {
        if !rotors.iter().all(|x| x.len() == reflect.len()) {
            return Err(Error::new(
                ErrorKind::NotFound,
                "rotors length does not match reflect length",
            ));
        }

        Ok(Self {
            spin_status: rotors.iter().map(|_| 0).collect(),
            chain: rotors,
            reflect,
        })
    }

    fn input(&mut self, i: &usize) -> Result<usize> {
        let mut temp = *i;

        // in order
        for ind in 0..self.chain.len() {
            temp = self.chain[ind].input_with_offset(temp, self.spin_status[ind])?;
        }

        temp = self.reflect.input(temp)?;

        // in rev order
        for ind in (0..self.chain.len()).rev() {
            temp = self.chain[ind].rev_input_with_offset(temp, self.spin_status[ind])?
        }

        // spin all rotors
        for ind in 0..self.chain.len() {
            self.spin_status[ind] += 1;
            if self.spin_status[ind] == self.chain[ind].len() {
                self.spin_status[ind] = 0;
            } else {
                break;
            }
        }

        Ok(temp)
    }

    fn reset_spin_status(&mut self) {
        self.spin_status = self.chain.iter().map(|_| 0).collect();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rotor_input() {
        let r = Rotor::default();
        let o = r.input(2).unwrap();
        let o1 = r.rev_input(o).unwrap();
        assert_eq!(2, o1);

        let o = r.input_with_offset(2, 1).unwrap();
        assert_eq!(r.input(3).unwrap() - 1, o);
    }

    #[test]
    fn input_vs_output() {
        let mut r1: Rotor = Default::default();
        let mut r2: Rotor = Default::default();
        let mut re: Reflector = Reflector::init(26).unwrap(); // reflect

        let mut r12 = r1.clone();
        let mut r22 = r2.clone();

        let mut c1 = RotorChain::new(vec![&mut r1, &mut r2], &re).unwrap();
        // need the new rotorchain for test
        let mut c2 = RotorChain::new(vec![&mut r12, &mut r22], &re).unwrap();

        let output = c1.input(&3).unwrap();
        assert_eq!(3, c2.input(&output).unwrap());

        // output shoud different
        let output_1 = c1.input(&3).unwrap();
        assert!(output_1 != output); // two times answers should not same as each other
        assert_eq!(3, c2.input(&output_1).unwrap());

        // reset r1, because everytime, rotor spin one after input
        assert!(c2.spin_status[0] == c1.spin_status[0]);
        assert!(c2.spin_status[0] == 2);
        c2.reset_spin_status();
        assert!(output == c2.input(&3).unwrap());

        // check spin status overflow
        c1.reset_spin_status();
        c1.spin_status = vec![25, 25];
        c1.input(&3);
        assert_eq!(c1.spin_status, vec![0, 0]);
        assert_eq!(output, c1.input(&3).unwrap());
    }
}
