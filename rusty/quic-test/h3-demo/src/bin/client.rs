use std::net;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = [0; 65535];
    let mut out = [0; 1350];

    // 1. QUIC Config
    let mut config = quiche::Config::new(quiche::PROTOCOL_VERSION)?;
    config.set_application_protos(quiche::h3::APPLICATION_PROTOCOL)?;
    config.verify_peer(false); // Ignore self-signed cert check

    // Match Server Transport Params
    config.set_initial_max_data(10_000_000);
    config.set_initial_max_streams_bidi(100);
    config.set_initial_max_streams_uni(100);
    config.set_initial_max_stream_data_uni(1_000_000);

    let socket = net::UdpSocket::bind("0.0.0.0:0")?;
    let server_addr: net::SocketAddr = "127.0.0.1:4433".parse()?;

    // 2. Connect
    let scid = quiche::ConnectionId::from_ref(&[0x11, 0x22, 0x33, 0x44]);
    let mut conn = quiche::connect(
        Some("localhost"),
        &scid,
        socket.local_addr()?,
        server_addr,
        &mut config,
    )?;

    // 3. Initial Packet
    let (write, send_info) = conn.send(&mut out)?;
    socket.send_to(&out[..write], &send_info.to)?;

    let mut h3_conn = None;
    let mut req_sent = false;

    println!("Connecting to H3 server...");

    loop {
        socket.set_read_timeout(Some(std::time::Duration::from_millis(100)))?;
        if let Ok((len, from)) = socket.recv_from(&mut buf) {
            conn.recv(
                &mut buf[..len],
                quiche::RecvInfo {
                    from,
                    to: socket.local_addr()?,
                },
            )
            .ok();
        }

        if conn.is_established() && h3_conn.is_none() {
            let h3_config = quiche::h3::Config::new()?;
            h3_conn = Some(quiche::h3::Connection::with_transport(
                &mut conn, &h3_config,
            )?);
            println!("H3 established!");
        }

        if let Some(h3) = &mut h3_conn {
            if !req_sent {
                let req = vec![
                    quiche::h3::Header::new(b":method", b"GET"),
                    quiche::h3::Header::new(b":scheme", b"https"),
                    quiche::h3::Header::new(b":authority", b"localhost"),
                    quiche::h3::Header::new(b":path", b"/"),
                ];
                h3.send_request(&mut conn, &req, true)?;
                req_sent = true;
                println!("Request sent.");
            }

            loop {
                match h3.poll(&mut conn) {
                    Ok((stream_id, quiche::h3::Event::Data)) => {
                        let mut body = [0; 1024];
                        if let Ok(n) = h3.recv_body(&mut conn, stream_id, &mut body) {
                            println!("Response body: {}", String::from_utf8_lossy(&body[..n]));
                        }
                    }
                    Ok((_, quiche::h3::Event::Finished)) => {
                        println!("Done!");
                        return Ok(());
                    }
                    Err(quiche::h3::Error::Done) => break,
                    _ => break,
                }
            }
        }

        // Flush handshake packets
        loop {
            let (write, send_info) = match conn.send(&mut out) {
                Ok(v) => v,
                Err(quiche::Error::Done) => break,
                Err(_) => break,
            };
            socket.send_to(&out[..write], &send_info.to).ok();
        }

        if conn.is_closed() {
            break;
        }
    }
    Ok(())
}
