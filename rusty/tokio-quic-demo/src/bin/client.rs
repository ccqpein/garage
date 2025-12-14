use tokio_quiche::http3::driver::{ClientH3Event, H3Event, InboundFrame, IncomingH3Headers};
use tokio_quiche::quiche::h3;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let socket = tokio::net::UdpSocket::bind("0.0.0.0:0").await.unwrap();
    socket.connect("127.0.0.1:4433").await.unwrap();
    let (_, mut controller) = tokio_quiche::quic::connect(socket, None).await.unwrap();

    controller
        .request_sender()
        .send(tokio_quiche::http3::driver::NewClientRequest {
            request_id: 0,
            headers: vec![h3::Header::new(b":method", b"GET")],
            body_writer: None,
        })
        .unwrap();

    while let Some(event) = controller.event_receiver_mut().recv().await {
        match event {
            ClientH3Event::Core(H3Event::IncomingHeaders(IncomingH3Headers {
                stream_id,
                headers,
                mut recv,
                ..
            })) => {
                log::info!(
                    "incomming headers: stream_id={:?}, headers={:?}",
                    stream_id,
                    headers
                );
                'body: while let Some(frame) = recv.recv().await {
                    match frame {
                        InboundFrame::Body(pooled, fin) => {
                            log::info!(
                                "inbound body: {:?}, fin: {:?}, len: {}",
                                std::str::from_utf8(&pooled),
                                fin,
                                pooled.len()
                            );
                            if fin {
                                log::info!("received full body, exiting");
                                break 'body;
                            }
                        }
                        InboundFrame::Datagram(pooled) => {
                            log::info!("len" = pooled.len(); "inbound datagram");
                        }
                    }
                }
            }
            ClientH3Event::Core(H3Event::BodyBytesReceived { fin: true, .. }) => {
                log::info!("fin received");
                break;
            }
            ClientH3Event::Core(event) => log::info!("received event: {event:?}"),
            ClientH3Event::NewOutboundRequest {
                stream_id,
                request_id,
            } => log::info!(
                "stream_id" = stream_id,
                "request_id" = request_id;
                "sending outbound request"
            ),
        }
    }
    Ok(())
}
