use std::iter::FromIterator;
use std::{
    fs::File,
    io::{self, BufReader, Read, Write},
    net::TcpStream,
};

fn load_root_ca() -> rustls::RootCertStore {
    let mut root_store = rustls::RootCertStore::empty();
    let cert_file = &mut BufReader::new(File::open("ca/ca.crt").unwrap());
    let certs = rustls_pemfile::certs(cert_file).unwrap();
    for cert in certs.iter() {
        root_store.add(&rustls::Certificate(cert.to_vec())).unwrap();
    }

    //dbg!(&cert_file);

    root_store
}

fn client_cert() -> Vec<rustls::Certificate> {
    let cert_file = &mut BufReader::new(File::open("ca/client_0.pem").unwrap());
    let cert = rustls::Certificate(cert_file.buffer().to_vec());

    vec![cert]
}

fn client_key() -> rustls::PrivateKey {
    let key_file = &mut BufReader::new(File::open("ca/client_0.key").unwrap());
    let mut keys = rustls_pemfile::pkcs8_private_keys(key_file).unwrap();
    rustls::PrivateKey(keys.remove(0))
}

fn main() {
    let config = rustls::ClientConfig::builder()
        .with_safe_defaults()
        .with_root_certificates(load_root_ca())
        .with_single_cert(client_cert(), client_key())
        .unwrap();

    //dbg!(config.client_auth_cert_resolver);
    let rc_config = std::sync::Arc::new(config);

    let mut client =
        rustls::ClientConnection::new(rc_config, "localhost".try_into().unwrap()).unwrap();

    ////////////////////////
    dbg!(client.wants_read());
    dbg!(client.wants_write());

    let request = b"GET / HTTP/2\r\n\
         Host: localhost:3030\r\n\
         Connection: close\r\n\
         Accept-Encoding: identity\r\n\
         accept: */*\r\n";

    let mut socket = TcpStream::connect("localhost:3030").unwrap();

    let mut stream = rustls::Stream::new(&mut client, &mut socket);

    // Complete handshake.
    //stream.flush().unwrap();

    dbg!(stream.write_all(request).unwrap());
    let ciphersuite = stream.conn.negotiated_cipher_suite().unwrap();
    writeln!(
        &mut std::io::stderr(),
        "Current ciphersuite: {:?}",
        ciphersuite.suite()
    );

    let mut plaintext = Vec::new();
    stream.read_to_end(&mut plaintext).unwrap();

    println!("{:?}", String::from_utf8(plaintext));
}
