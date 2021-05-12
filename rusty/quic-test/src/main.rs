use log::{debug, error, info};

use quiche::{Config, ConnectionId};
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

    let mut pkt_count = 0;
    let mut buf = [0; 65535];

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
    }

    // let config = Config::new(quiche::PROTOCOL_VERSION)?;
    // let conn = quiche::accept(&scid, None, &mut config)?;
    // println!("Hello, world!");
}
