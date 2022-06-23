use std::{
    fs::File,
    io::{BufReader, Read, Write},
    net::TcpListener,
    sync::Arc,
};

use rustls::server::AllowAnyAuthenticatedClient;

fn load_certs() -> Vec<rustls::Certificate> {
    let mut cert_file = &mut BufReader::new(File::open("ca/localhost.bundle.crt").unwrap());
    rustls_pemfile::certs(&mut cert_file)
        .unwrap()
        .iter()
        .map(|v| rustls::Certificate(v.clone()))
        .collect()
}

fn load_private_key() -> rustls::PrivateKey {
    let key_file = &mut BufReader::new(File::open("ca/localhost.key").unwrap());
    let mut keys = rustls_pemfile::pkcs8_private_keys(key_file).unwrap();
    rustls::PrivateKey(keys.remove(0))
}

fn make_config() -> Arc<rustls::ServerConfig> {
    // load ca
    let client_auth = {
        let mut root_store = rustls::RootCertStore::empty();
        let cert_file = &mut BufReader::new(File::open("ca/ca.crt").unwrap());
        let certs = rustls_pemfile::certs(cert_file).unwrap();
        for cert in certs.iter() {
            root_store.add_parsable_certificates(&certs);
        }

        //dbg!(&cert_file);

        AllowAnyAuthenticatedClient::new(root_store)
    };

    let suites = rustls::ALL_CIPHER_SUITES.to_vec();
    let versions = rustls::ALL_VERSIONS.to_vec();

    let certs = load_certs();
    let privkey = load_private_key();

    let mut config = rustls::ServerConfig::builder()
        .with_cipher_suites(&suites)
        .with_safe_default_kx_groups()
        .with_protocol_versions(&versions)
        .expect("inconsistent cipher-suites/versions specified")
        .with_client_cert_verifier(client_auth)
        .with_single_cert_with_ocsp_and_sct(certs, privkey, vec![], vec![])
        .expect("bad certificates/private key");

    Arc::new(config)
}

fn main() {
    let listener = TcpListener::bind("0.0.0.0:3030").unwrap();
    let tls_config = make_config();

    for stream in listener.incoming() {
        match stream {
            Ok(mut stream) => {
                let mut tls_conn = rustls::ServerConnection::new(Arc::clone(&tls_config)).unwrap();
                let mut tls_stream = rustls::Stream::new(&mut tls_conn, &mut stream);
                let mut buffer = vec![0; 512];
                //loop {

                match tls_stream.read(&mut buffer) {
                    Err(_) => panic!(),
                    Ok(n) => println!("read {} bytes", n),
                }
                //                }
                println!("from client: {}", String::from_utf8(buffer).unwrap());
                tls_stream.write("yoyoyo".as_bytes());
            }
            Err(e) => {
                panic!()
            }
        }
    }
}
