use quiche::{Config, ConnectionId};
use std::net;

// pub struct Client {
//     pub conn: std::pin::Pin<Box<quiche::Connection>>,

//     pub http_conn: Option<Box<dyn HttpConn>>,

//     pub siduck_conn: Option<SiDuckConn>,

//     pub app_proto_selected: bool,

//     pub partial_requests: std::collections::HashMap<u64, PartialRequest>,

//     pub partial_responses: std::collections::HashMap<u64, PartialResponse>,
// }

// pub type ClientMap = HashMap<ConnectionId<'static>, (net::SocketAddr, Client)>;

fn main() {
    env_logger::builder()
        .default_format_timestamp_nanos(true)
        .init();

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

    // let config = Config::new(quiche::PROTOCOL_VERSION)?;
    // let conn = quiche::accept(&scid, None, &mut config)?;
    // println!("Hello, world!");
}
