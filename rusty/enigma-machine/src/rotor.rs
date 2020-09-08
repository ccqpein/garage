use rand::seq::SliceRandom;

struct Rotor {
    input_vec: Vec<u8>,
    output_vec: Vec<u8>,
}

impl Rotor {
    fn init(mut letter_set: Vec<u8>) -> Self {
        let mut rng = rand::thread_rng();
        Self {
            input_vec: letter_set.clone(),
            output_vec: {
                letter_set.shuffle(&mut rng);
                letter_set
            },
        }
    }
}

impl Default for Rotor {
    fn default() -> Self {
        Self::init("abcdefghijklmnopqrstuvwxyz".to_string().into_bytes())
    }
}
