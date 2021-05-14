use log::{debug, error, info, trace};

use quiche::{Config, ConnectionId};
use ring::rand::*;
use std::net;

fn main() -> ! {
    env_logger::builder().format_timestamp_nanos().init();

    // mio event poll
    let poll = mio::Poll::new().unwrap();
    let mut events = mio::Events::with_capacity(1024);

    let socket = net::UdpSocket::bind("127.0.0.1:4433").unwrap();

    let socket = mio::net::UdpSocket::from_socket(socket).unwrap();
    poll.register(
        &socket,
        mio::Token(0),
        mio::Ready::readable(),
        mio::PollOpt::edge(),
    )
    .unwrap();

    let mut config = quiche::Config::new(quiche::PROTOCOL_VERSION).unwrap();

    //:= don't know what are these
    let rng = SystemRandom::new();
    let conn_id_seed = ring::hmac::Key::generate(ring::hmac::HMAC_SHA256, &rng).unwrap();

    let mut pkt_count = 0;
    let mut buf = [0; 1024];
    let mut out = [0; 1024];

    loop {
        // wait until events coming
        poll.poll(&mut events, None).unwrap();

        let (len, src) = match socket.recv_from(&mut buf) {
            Ok(v) => v,

            Err(e) => {
                // There are no more UDP packets to read, so end the read
                // loop.
                if e.kind() == std::io::ErrorKind::WouldBlock {
                    info!("recv() would block");
                }

                panic!("recv() failed: {:?}", e);
            }
        };

        let pkt_buf = &mut buf[..len];
        info!("buf now {:?}", &pkt_buf);

        // parser header
        let hdr = match quiche::Header::from_slice(pkt_buf, quiche::MAX_CONN_ID_LEN) {
            Ok(v) => v,

            Err(e) => {
                panic!("Parsing packet header failed: {:?}", e);
            }
        };

        trace!("got packet {:?}", hdr);

        // conn id maker
        let conn_id = ring::hmac::sign(&conn_id_seed, &hdr.dcid);
        let conn_id = &conn_id.as_ref()[..quiche::MAX_CONN_ID_LEN];
        let conn_id: ConnectionId = conn_id.to_vec().into();

        // make new client
        if hdr.ty != quiche::Type::Initial {
            panic!("Packet is not Initial");
        }

        // header version
        if !quiche::version_is_supported(hdr.version) {
            info!("Doing version negotiation");

            let len = quiche::negotiate_version(&hdr.scid, &hdr.dcid, &mut out).unwrap();

            let out = &out[..len];

            if let Err(e) = socket.send_to(out, &src) {
                if e.kind() == std::io::ErrorKind::WouldBlock {
                    trace!("send() would block");
                }

                panic!("send() failed: {:?}", e);
            }
        }

        let mut scid = [0; quiche::MAX_CONN_ID_LEN];
        scid.copy_from_slice(&conn_id);

        let mut odcid: Option<ConnectionId> = None;

        //:= Don't know what is !args.no_retry

        let scid = quiche::ConnectionId::from_vec(scid.to_vec());

        //:= why there are two ids?
        debug!("New connection: dcid={:?} scid={:?}", hdr.dcid, scid);

        let mut conn = quiche::accept(&scid, odcid.as_ref(), &mut config).unwrap();

        let read = match conn.recv(pkt_buf) {
            Ok(v) => v,

            Err(e) => {
                panic!("{} recv failed: {:?}", conn.trace_id(), e);
            }
        };

        trace!("{} processed {} bytes", conn.trace_id(), read);
    }

    // let config = Config::new(quiche::PROTOCOL_VERSION)?;
    // let conn = quiche::accept(&scid, None, &mut config)?;
    // println!("Hello, world!");
}
