// use mio::net::{TcpListener, TcpStream};
// use std::{env, fs::File, io::BufReader};

// use rustls::server::AllowAnyAuthenticatedClient;
// use warp::Filter;

// warp isn't good for mTLS
// async fn run_server() {
//     let routes = warp::any().map(|| "Hello, mTLS World!");

//     warp::serve(routes)
//         .tls()
//         .key_path("ca/localhost.key")
//         .cert_path("ca/localhost.bundle.crt")
//         .client_auth_required_path("ca/ca.crt")
//         .run(([0, 0, 0, 0], 3030))
//         .await
// }

// #[tokio::main]
// async fn main() {
//     let args: Vec<String> = env::args().collect();
//     let server = run_server();
//     server.await;
// }

// fn server_config() -> Arc<rustls::ServerConfig> {
//     let mut root_store = rustls::RootCertStore::empty();
//     let cert_file = &mut BufReader::new(File::open("ca/ca.crt").unwrap());
//     let certs = rustls_pemfile::certs(cert_file).unwrap();
//     for cert in certs.iter() {
//         root_store.add(&rustls::Certificate(cert.to_vec())).unwrap();
//     }

//     //dbg!(&cert_file);

//     let client_auth = AllowAnyAuthenticatedClient::new(root_store);
//     let suites = rustls::ALL_CIPHER_SUITES.to_vec();
//     let versions = rustls::ALL_VERSIONS.to_vec();

//     let cert_file = &mut BufReader::new(File::open("ca/localhost.bundle.crt").unwrap());
//     let certs = vec![rustls::Certificate(cert_file.buffer().to_vec())];

//     let key_file = &mut BufReader::new(File::open("ca/localhost.key").unwrap());
//     let mut keys = rustls_pemfile::pkcs8_private_keys(key_file).unwrap();
//     let privateKey = rustls::PrivateKey(keys.remove(0));

//     let mut config = rustls::ServerConfig::builder()
//         .with_cipher_suites(&suites)
//         .with_safe_default_kx_groups()
//         .with_protocol_versions(&versions)
//         .expect("inconsistent cipher-suites/versions specified")
//         .with_client_cert_verifier(client_auth)
//         .with_single_cert_with_ocsp_and_sct(certs, privkey, vec![], vec![])
//         .expect("bad certificates/private key");

//     Arc::new(config)
// }

// struct TlsServer {
//     server: TcpListener,
//     connections: HashMap<mio::Token, OpenConnection>,
//     next_id: usize,
//     tls_config: Arc<rustls::ServerConfig>,
// }

// impl TlsServer {
//     fn new(server: TcpListener, cfg: Arc<rustls::ServerConfig>) -> Self {
//         TlsServer {
//             server,
//             connections: HashMap::new(),
//             next_id: 2,
//             tls_config: cfg,
//         }
//     }

//     fn accept(&mut self, registry: &mio::Registry) -> Result<(), io::Error> {
//         loop {
//             match self.server.accept() {
//                 Ok((socket, addr)) => {
//                     let tls_conn =
//                         rustls::ServerConnection::new(Arc::clone(&self.tls_config)).unwrap();

//                     let token = mio::Token(self.next_id);
//                     self.next_id += 1;

//                     let mut connection = OpenConnection::new(socket, token, tls_conn);
//                     connection.register(registry);
//                     self.connections.insert(token, connection);
//                 }
//                 Err(ref err) if err.kind() == io::ErrorKind::WouldBlock => return Ok(()),
//                 Err(err) => {
//                     println!(
//                         "encountered error while accepting connection; err={:?}",
//                         err
//                     );
//                     return Err(err);
//                 }
//             }
//         }
//     }

//     fn conn_event(&mut self, registry: &mio::Registry, event: &mio::event::Event) {
//         let token = event.token();

//         if self.connections.contains_key(&token) {
//             self.connections
//                 .get_mut(&token)
//                 .unwrap()
//                 .ready(registry, event);

//             if self.connections[&token].is_closed() {
//                 self.connections.remove(&token);
//             }
//         }
//     }
// }

// fn main() {
//     let config = server_config();
//     let mut addr: net::SocketAddr = "0.0.0.0:3030".parse().unwrap();
//     let mut listener = TcpListener::bind(addr).expect("cannot listen on port");
//     let mut poll = mio::Poll::new().unwrap();

//     poll.registry()
//         .register(&mut listener, LISTENER, mio::Interest::READABLE)
//         .unwrap();

//     let mut tlsserv = TlsServer::new(listener, config);

//     let mut events = mio::Events::with_capacity(256);

//     loop {
//         poll.poll(&mut events, None).unwrap();

//         for event in events.iter() {
//             match event.token() {
//                 LISTENER => {
//                     tlsserv
//                         .accept(poll.registry())
//                         .expect("error accepting socket");
//                 }
//                 _ => tlsserv.conn_event(poll.registry(), event),
//             }
//         }
//     }
// }
