use std::{env, fs};

use aes_gcm::{
    aead::{Aead, AeadCore, KeyInit, OsRng},
    Aes256Gcm,
    Key, // Or `Aes128Gcm`
    Nonce,
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

    // small file
    let content = read_file("/data/paper.pdf");
    let ciphertext = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    let plaintext = cipher.decrypt(&nonce, ciphertext.as_ref()).unwrap();
    assert_eq!(plaintext, content);

    // test the same nonce with multi times
    let ciphertext2 = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    assert_eq!(ciphertext2, ciphertext);

    // read big file

    // let content = read_file("/data/movie.mkv");

    // let ciphertext = cipher.encrypt(&nonce, content.as_ref()).unwrap();
    // let plaintext = cipher.decrypt(&nonce, ciphertext.as_ref()).unwrap();

    // assert_eq!(plaintext, content);
}
