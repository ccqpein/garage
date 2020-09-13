use enigma_machine::plugboard::*;
use enigma_machine::*;

fn main() {
    let pb = Plugboard::new(&vec![(1, 23), (4, 9)]);
    let mut enigma_machine = EnigmaM::new(26, 3, &pb);

    // ask input
    loop {}
}
