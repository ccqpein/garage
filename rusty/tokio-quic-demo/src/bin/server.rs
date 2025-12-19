// use futures::{SinkExt as _, StreamExt as _};
// use tokio::net::UdpSocket;
// use tokio_quiche::{
//     ConnectionParams, ServerH3Controller, ServerH3Driver,
//     buf_factory::BufFactory,
//     http3::{
//         driver::{ClientH3Event, H3Event, IncomingH3Headers, OutboundFrame, ServerH3Event},
//         settings::Http3Settings,
//     },
//     listen,
//     metrics::DefaultMetrics,
//     quic::SimpleConnectionIdGenerator,
//     quiche::h3,
//     settings::{Hooks, TlsCertificatePaths},
// };

// const LISTEN_ADDR: &str = "0.0.0.0:4433";
// const CERTS_DIR: &str = "./vault"; // Assuming cert.pem and key.pem are in the project root

// async fn run_server() -> Result<(), Box<dyn std::error::Error>> {
//     println!("Starting server on {}", LISTEN_ADDR);

//     // 1. Setup TLS config (using the generated certs)
//     let tls_config = TlsCertificatePaths {
//         cert: &format!("{}/cert.pem", CERTS_DIR),
//         private_key: &format!("{}/key.pem", CERTS_DIR),
//         kind: tokio_quiche::settings::CertificateKind::X509,
//     };

//     // 2. Setup QUIC/Connection parameters
//     let conn_params =
//         ConnectionParams::new_server(Default::default(), tls_config, Hooks::default());

//     // 3. Create the listener
//     let socket = UdpSocket::bind(LISTEN_ADDR).await?;
//     let mut listeners = listen(
//         [socket],
//         conn_params,
//         SimpleConnectionIdGenerator,
//         DefaultMetrics,
//     )?;

//     let accept_stream = &mut listeners[0];

//     // 4. Accept incoming connections
//     while let Some(conn) = accept_stream.next().await {
//         println!("New connection accepted.");

//         // Setup H3Driver for the new connection
//         let (driver, controller) = ServerH3Driver::new(Http3Settings::default());
//         conn?.start(driver);

//         // Spawn a task to handle the HTTP/3 requests on this connection
//         tokio::spawn(handle_connection(controller));
//     }

//     Ok(())
// }

// // Handler for each individual connection
// async fn handle_connection(mut controller: ServerH3Controller) {
//     println!("Connection handler started.");
//     while let Some(ServerH3Event::Core(event)) = controller.event_receiver_mut().recv().await {
//         match event {
//             H3Event::IncomingSettings { settings } => {
//                 println!(
//                     "-> Received HTTP/3 Settings from client (Handshake OK). Waiting for request..."
//                 );
//             }

//             // This is how we process an incoming HTTP/3 request
//             H3Event::IncomingHeaders(IncomingH3Headers {
//                 mut send, headers, ..
//             }) => {
//                 println!("-> Server received request with headers: {:?}", headers);

//                 // Send the HTTP/3 response headers
//                 send.send(OutboundFrame::Headers(
//                     vec![h3::Header::new(b":status", b"200")],
//                     None,
//                 ))
//                 .await
//                 .unwrap();

//                 // Send the HTTP/3 response body and set 'fin' to true to close the stream
//                 send.send(OutboundFrame::body(
//                     BufFactory::buf_from_slice(b"Hello from tokio-quiche!"),
//                     true,
//                 ))
//                 .await
//                 .unwrap();

//                 println!("<- Server sent 'Hello' response.");
//             }

//             _ => {
//                 println!("event catch: {:?}", event);
//             }
//         }
//     }
//     println!("Connection handler finished.");
// }

// ================================================ new one

use std::default;
use std::error::Error;

use futures::{SinkExt as _, StreamExt as _};
use quiche::Config;
use tokio_quiche::buf_factory::BufFactory;
use tokio_quiche::http3::driver::{H3Event, IncomingH3Headers, OutboundFrame, ServerH3Event};
use tokio_quiche::http3::settings::Http3Settings;
use tokio_quiche::listen;
use tokio_quiche::metrics::DefaultMetrics;
use tokio_quiche::quic::SimpleConnectionIdGenerator;
use tokio_quiche::quiche::h3;
use tokio_quiche::settings::QuicSettings;
use tokio_quiche::{ConnectionParams, ServerH3Controller, ServerH3Driver};

async fn run_server() -> Result<(), Box<dyn Error>> {
    log::info!("binding Udp socket");
    let socket = tokio::net::UdpSocket::bind("0.0.0.0:4043").await?;
    log::info!("after binding Udp socket");

    let mut settings = QuicSettings::default();
    // settings.initial_max_streams_bidi = 100; // Allow client to open streams
    // settings.initial_max_streams_uni = 100;
    // settings.initial_max_data = 10_000_000; // Allow total connection data
    // settings.initial_max_stream_data_bidi_local = 1_000_000;
    // settings.initial_max_stream_data_bidi_remote = 1_000_000;
    // settings.initial_max_stream_data_uni = 1_000_000;
    // settings.alpn = vec![b"h3".to_vec()];

    log::info!(
        "DEBUG SETTINGS: Max Streams Bidi: {}, Max Data: {}",
        settings.initial_max_streams_bidi,
        settings.initial_max_data
    );

    let mut listeners = listen(
        [socket],
        ConnectionParams::new_server(
            //Default::default(),
            settings,
            tokio_quiche::settings::TlsCertificatePaths {
                cert: "./vault/cert.pem",
                private_key: "./vault/key.pem",
                kind: tokio_quiche::settings::CertificateKind::X509,
            },
            Default::default(),
        ),
        SimpleConnectionIdGenerator,
        DefaultMetrics,
    )?;

    let accept_stream = &mut listeners[0];
    log::info!("start listening");
    while let Some(conn) = accept_stream.next().await {
        let (driver, controller) = ServerH3Driver::new(Http3Settings::default());
        conn?.start(driver);
        tokio::spawn(handle_connection(controller));
    }
    Ok(())
}

async fn handle_connection(mut controller: ServerH3Controller) {
    while let Some(ServerH3Event::Core(event)) = controller.event_receiver_mut().recv().await {
        match event {
            H3Event::IncomingHeaders(IncomingH3Headers {
                mut send, headers, ..
            }) => {
                log::info!("incoming headers: {:?}", headers);
                send.send(OutboundFrame::Headers(
                    vec![h3::Header::new(b":status", b"200")],
                    None,
                ))
                .await
                .unwrap();

                send.send(OutboundFrame::body(
                    BufFactory::buf_from_slice(b"hello from TQ!"),
                    true,
                ))
                .await
                .unwrap();
            }
            H3Event::IncomingSettings { settings } => {
                log::info!(
                    "-> Received HTTP/3 Settings from client (Handshake OK). {settings:?} Waiting for request..."
                );
            }
            event => {
                log::info!("event: {event:?}");
            }
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();
    run_server().await.unwrap();
}
