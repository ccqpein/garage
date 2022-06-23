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
        root_store.add_parsable_certificates(&certs);
    }

    //dbg!(&cert_file);

    root_store
}

fn client_cert() -> Vec<rustls::Certificate> {
    let mut cert_file = &mut BufReader::new(File::open("ca/client_0.crt").unwrap());
    rustls_pemfile::certs(&mut cert_file)
        .unwrap()
        .iter()
        .map(|v| rustls::Certificate(v.clone()))
        .collect()
}

fn client_key() -> rustls::PrivateKey {
    let key_file = &mut BufReader::new(File::open("ca/client_0.key").unwrap());
    let mut keys = rustls_pemfile::pkcs8_private_keys(key_file).unwrap();
    rustls::PrivateKey(keys.remove(0))
}

fn main() {
    let config = rustls::ClientConfig::builder()
        .with_cipher_suites(&rustls::DEFAULT_CIPHER_SUITES.to_vec())
        .with_safe_default_kx_groups()
        .with_protocol_versions(&rustls::DEFAULT_VERSIONS.to_vec())
        .unwrap()
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

    let request = b"GET / HTTP/1.0\r\nHost: localhost\r\nConnection: \
                               close\r\nAccept-Encoding: identity\r\n\r\n";

    let mut socket = TcpStream::connect("localhost:3030").unwrap();

    //client.writer().write(request).unwrap();

    let mut stream = rustls::Stream::new(&mut client, &mut socket);

    // Complete handshake.
    //stream.flush().unwrap();

    //dbg!(stream.write_all(request));
    dbg!(stream.write(request));
    let ciphersuite = stream.conn.negotiated_cipher_suite().unwrap();
    writeln!(
        &mut std::io::stderr(),
        "Current ciphersuite: {:?}",
        ciphersuite.suite()
    );

    let mut plaintext = Vec::new();
    dbg!(stream.conn.wants_read());

    stream.read_to_end(&mut plaintext).unwrap();
    //stream.conn.reader().read_to_end(&mut plaintext);

    println!("{:?}", String::from_utf8(plaintext));
}
