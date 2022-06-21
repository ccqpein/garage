use std::{fs::File, io::BufReader, sync::Arc};

use rustls::server::AllowAnyAuthenticatedClient;

fn main() {
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
}
