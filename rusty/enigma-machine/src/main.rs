use enigma_machine::plugboard::*;
use enigma_machine::*;
use std::io;

fn main() -> io::Result<()> {
    let pb = Plugboard::new(&vec![(1, 23), (4, 9)]); // just random plugboard
    let mut enigma_machine = EnigmaM::new(26, 3, &pb)?;

    let alphabet = "abcdefghijklmnopqrstuvwxyz".as_bytes();

    let mut input = String::new();
    let mut output: Vec<usize> = vec![];
    loop {
        println!("input string:");
        io::stdin().read_line(&mut input)?;

        for b in input.as_bytes() {
            if let Some(p) = alphabet.iter().position(|x| x == b) {
                output.push(enigma_machine.input(p)?);
            } else {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "cannot find this char in alphabet",
                ));
            }
        }

        // make output here

        input.clear();
        output.clear()
    }

    Ok(())
}
