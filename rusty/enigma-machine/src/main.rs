use enigma_machine::plugboard::*;
use enigma_machine::*;

fn main() {
    let pb = Plugboard::default(); // empty
    let mut enigma_machine = EnigmaM::new(26, 3, &pb);
}
