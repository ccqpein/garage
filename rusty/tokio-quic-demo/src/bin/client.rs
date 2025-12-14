use std::default;

use futures::StreamExt; // For recv() on the controller
use tokio::net::UdpSocket;
use tokio_quiche::{
    ClientH3Driver, ConnectionParams,
    http3::{
        driver::{ClientH3Event, H3Event, IncomingH3Headers},
        settings::Http3Settings,
    },
    metrics::DefaultMetrics,
    quic::{SimpleConnectionIdGenerator, connect},
    quiche::{self, h3},
    settings::{Hooks, QuicSettings, TlsCertificatePaths},
    socket::Socket,
};

const SERVER_ADDR: &str = "127.0.0.1:4433";
const SERVER_NAME: &str = "localhost"; // Must match the cert CN
const CERTS_DIR: &str = "./vault"; // Assuming cert.pem and key.pem are in the project root

// --- Run this client ---
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    simple_logger::init_with_level(log::Level::Info).unwrap();
    run_client().await
}

async fn run_client() -> Result<(), Box<dyn std::error::Error>> {
    println!("Starting client connecting to {}", SERVER_ADDR);

    // Setup TLS config (using the generated certs)
    let tls_config = TlsCertificatePaths {
        cert: &format!("{}/cert.pem", CERTS_DIR),
        private_key: &format!("{}/key.pem", CERTS_DIR),
        kind: tokio_quiche::settings::CertificateKind::X509,
    };

    let mut quic_config: QuicSettings = Default::default();
    quic_config.verify_peer = false;
    let conn_params = ConnectionParams::new_client(quic_config, Some(tls_config), Hooks::default());

    // 3. Connect to the server
    let socket = UdpSocket::bind("0.0.0.0:0").await?;
    let remote_addr: std::net::SocketAddr = SERVER_ADDR.parse()?;

    // Connect using tokio-quiche
    let (connection, client3hCtrl) = connect(socket, Some(SERVER_NAME)).await.unwrap();

    println!("QUIC connection established.");

    // 4. Start the HTTP/3 driver
    let (driver, mut controller) = ClientH3Driver::new(Http3Settings::default());

    // Start the driver task to handle packet processing
    //connection.start(driver);

    // 5. Send HTTP/3 Request
    println!("Sending HTTP/3 request...");
    let headers = vec![
        h3::Header::new(b":method", b"GET"),
        h3::Header::new(b":path", b"/"),
        h3::Header::new(b":authority", SERVER_NAME.as_bytes()),
        h3::Header::new(b":scheme", b"https"),
    ];

    // Send the request. `true` means this is the end of the stream (no body).
    let request_sender = controller.request_sender();

    request_sender.send(headers, true).unwrap();

    // 6. Loop to handle events (receive response)
    println!("Waiting for response...");
    while let Some(ClientH3Event::Core(event)) = controller.event_receiver_mut().recv().await {
        match event {
            H3Event::IncomingHeaders(IncomingH3Headers { headers, .. }) => {
                println!("<- Client received headers: {:?}", headers);
            }
            H3Event::BodyBytesReceived { body, fin, .. } => {
                let text = std::str::from_utf8(&body).unwrap_or("<binary data>");
                println!("<- Client received body chunk: '{}'", text);

                if fin {
                    println!("Response complete. Client exiting.");
                    break;
                }
            }
            _ => {
                // Ignore other events
            }
        }
    }

    Ok(())
}
