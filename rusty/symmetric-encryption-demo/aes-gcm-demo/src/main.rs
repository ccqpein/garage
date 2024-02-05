use std::{
    env,
    fs::{self, File},
    io::{BufReader, Write},
};

use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng},
    AeadInPlace, Aes128Gcm, Aes256Gcm, Key, Nonce,
};

fn read_file(path: &str) -> Vec<u8> {
    let current_path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    dbg!(&current_path);

    let file_path = fs::read_link(current_path + path).unwrap();
    dbg!(&file_path);

    std::fs::read(&file_path).unwrap()
}

fn aes_ctr_test(input_path: &str, output_path: &str) {
    let mut content = read_file(input_path);
    let key = Aes128Gcm::generate_key(OsRng);

    let cipher = Aes128Gcm::new(&key);
    let nonce = Aes128Gcm::generate_nonce(&mut OsRng);

    let tag = cipher
        .encrypt_in_place_detached(&nonce, &[], &mut content)
        .unwrap();
    //dbg!(&tag);
    //let mut decrypted_data = vec![];
    let current_path = env::current_dir()
        .unwrap()
        .into_os_string()
        .into_string()
        .unwrap();
    let mut output_file = File::open(current_path + output_path).unwrap();

    match cipher.decrypt_in_place_detached(&nonce, &[], &mut content, &tag) {
        Ok(_) => output_file.write_all(&content),
        Err(e) => panic!("{:?}", e),
    };
}

fn main() {
    let key = Aes256Gcm::generate_key(OsRng);
    //dbg!(key);

    let cipher = Aes256Gcm::new(&key);
    let nonce = Aes256Gcm::generate_nonce(&mut OsRng); // 96-bits; unique per message

    // example below
    // let ciphertext = cipher
    //     .encrypt(&nonce, b"plaintext message".as_ref())
    //     .unwrap();
    // let plaintext = cipher.decrypt(&nonce, ciphertext.as_ref()).unwrap();

    /////// small file
    let content = read_file("/data/paper.pdf");
    let ciphertext = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    let plaintext = cipher.decrypt(&nonce, ciphertext.as_ref()).unwrap();
    assert_eq!(plaintext, content);

    // test the same nonce with multi times
    let ciphertext2 = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    assert_eq!(ciphertext2, ciphertext);

    //////// read big file

    // let content = read_file("/data/movie.mkv");
    // let ciphertext = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    // let plaintext = cipher.decrypt(&nonce, ciphertext.as_ref()).unwrap();

    // assert_eq!(plaintext, content);

    ////// test ctr
    aes_ctr_test("/data/paper.pdf", "/data/outpaper.pdf");
    //aes_ctr_test("/data/movie.mkv", "/data/movieOut.mkv");
}
