use futures::{SinkExt as _, StreamExt as _};
use tokio::net::UdpSocket;
use tokio_quiche::{
    ConnectionParams, ServerH3Controller, ServerH3Driver,
    buf_factory::BufFactory,
    http3::{
        driver::{ClientH3Event, H3Event, IncomingH3Headers, OutboundFrame, ServerH3Event},
        settings::Http3Settings,
    },
    listen,
    metrics::DefaultMetrics,
    quic::SimpleConnectionIdGenerator,
    quiche::h3,
    settings::{Hooks, TlsCertificatePaths},
};

const LISTEN_ADDR: &str = "0.0.0.0:4433";
const CERTS_DIR: &str = "./vault"; // Assuming cert.pem and key.pem are in the project root

async fn run_server() -> Result<(), Box<dyn std::error::Error>> {
    println!("Starting server on {}", LISTEN_ADDR);

    // 1. Setup TLS config (using the generated certs)
    let tls_config = TlsCertificatePaths {
        cert: &format!("{}/cert.pem", CERTS_DIR),
        private_key: &format!("{}/key.pem", CERTS_DIR),
        kind: tokio_quiche::settings::CertificateKind::X509,
    };

    // 2. Setup QUIC/Connection parameters
    let conn_params =
        ConnectionParams::new_server(Default::default(), tls_config, Hooks::default());

    // 3. Create the listener
    let socket = UdpSocket::bind(LISTEN_ADDR).await?;
    let mut listeners = listen(
        [socket],
        conn_params,
        SimpleConnectionIdGenerator,
        DefaultMetrics,
    )?;

    let accept_stream = &mut listeners[0];

    // 4. Accept incoming connections
    while let Some(conn) = accept_stream.next().await {
        println!("New connection accepted.");

        // Setup H3Driver for the new connection
        let (driver, controller) = ServerH3Driver::new(Http3Settings::default());
        conn?.start(driver);

        // Spawn a task to handle the HTTP/3 requests on this connection
        tokio::spawn(handle_connection(controller));
    }

    Ok(())
}

// Handler for each individual connection
async fn handle_connection(mut controller: ServerH3Controller) {
    println!("Connection handler started.");
    while let Some(ServerH3Event::Core(event)) = controller.event_receiver_mut().recv().await {
        match event {
            // This is how we process an incoming HTTP/3 request
            H3Event::IncomingHeaders(IncomingH3Headers {
                mut send, headers, ..
            }) => {
                println!("-> Server received request with headers: {:?}", headers);

                // Send the HTTP/3 response headers
                send.send(OutboundFrame::Headers(
                    vec![h3::Header::new(b":status", b"200")],
                    None,
                ))
                .await
                .unwrap();

                // Send the HTTP/3 response body and set 'fin' to true to close the stream
                send.send(OutboundFrame::body(
                    BufFactory::buf_from_slice(b"Hello from tokio-quiche!"),
                    true,
                ))
                .await
                .unwrap();

                println!("<- Server sent 'Hello' response.");
            }
            // Ignore other events for this simple demo
            _ => {}
        }
    }
    println!("Connection handler finished.");
}

// --- Run the server first ---
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    simple_logger::init_with_level(log::Level::Info).unwrap();
    run_server().await
}
