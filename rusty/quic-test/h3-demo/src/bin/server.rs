//! trying to make some h3 demo

use std::collections::HashMap;
use std::net;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut config = quiche::Config::new(quiche::PROTOCOL_VERSION)?;

    // Fix: Pass as a slice of slices
    config.set_application_protos(&[quiche::h3::APPLICATION_PROTOCOL[0]])?;

    config.load_cert_chain_from_pem_file("cert.pem")?;
    config.load_priv_key_from_pem_file("key.pem")?;

    // Mandatory H3 Parameters
    config.set_initial_max_data(10_000_000);
    config.set_initial_max_streams_bidi(100);
    config.set_initial_max_streams_uni(100);
    config.set_initial_max_stream_data_bidi_local(1_000_000);
    config.set_initial_max_stream_data_bidi_remote(1_000_000);
    config.set_initial_max_stream_data_uni(1_000_000);

    let socket = net::UdpSocket::bind("127.0.0.1:4433")?;
    // We must use a Map to keep track of connection states
    let mut clients: HashMap<Vec<u8>, (quiche::Connection, Option<quiche::h3::Connection>)> =
        HashMap::new();
    let mut buf = [0; 65535];
    let mut out = [0; 1350];

    println!("Server listening on 127.0.0.1:4433");

    loop {
        let (len, src) = match socket.recv_from(&mut buf) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let hdr = match quiche::Header::from_slice(&mut buf[..len], quiche::MAX_CONN_ID_LEN) {
            Ok(v) => v,
            Err(_) => continue,
        };

        let dcid = hdr.dcid.to_vec();

        // Find existing connection or create a new one
        let (conn, h3_conn) = if !clients.contains_key(&dcid) {
            if hdr.ty != quiche::Type::Initial {
                continue;
            }

            // Create new QUIC connection
            let scid = quiche::ConnectionId::from_ref(&[0xba, 0xbe, 0xbe, 0xef]);
            let mut qc = quiche::accept(&scid, None, socket.local_addr()?, src, &mut config)?;

            println!("New QUIC connection from {}", src);
            clients.insert(dcid.clone(), (qc, None));
            clients.get_mut(&dcid).unwrap()
        } else {
            clients.get_mut(&dcid).unwrap()
        };

        // Process the packet
        let recv_info = quiche::RecvInfo {
            from: src,
            to: socket.local_addr()?,
        };
        if let Err(e) = conn.recv(&mut buf[..len], recv_info) {
            if e == quiche::Error::TlsFail {
                if let Some(err) = conn.local_error() {
                    eprintln!(
                        "TLS Error: {} - {}",
                        err.error_code,
                        String::from_utf8_lossy(&err.reason)
                    );
                }
            }
            continue;
        }

        // Initialize H3 if ready
        if conn.is_established() && h3_conn.is_none() {
            let h3_config = quiche::h3::Config::new()?;
            if let Ok(h3) = quiche::h3::Connection::with_transport(conn, &h3_config) {
                println!("HTTP/3 Layer Ready");
                *h3_conn = Some(h3);
            }
        }

        // Handle H3 Events
        if let Some(h3) = h3_conn {
            loop {
                match h3.poll(conn) {
                    Ok((id, quiche::h3::Event::Headers { .. })) => {
                        let h = vec![quiche::h3::Header::new(b":status", b"200")];
                        h3.send_response(conn, id, &h, false).ok();
                        h3.send_body(conn, id, b"Success", true).ok();
                    }
                    Err(quiche::h3::Error::Done) => break,
                    _ => break,
                }
            }
        }

        // Send all pending packets
        loop {
            let (write, send_info) = match conn.send(&mut out) {
                Ok(v) => v,
                Err(quiche::Error::Done) => break,
                Err(_) => break,
            };
            socket.send_to(&out[..write], &send_info.to).ok();
        }
    }
}
