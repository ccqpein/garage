// use tokio_quiche::http3::driver::{
//     ClientH3Event, H3Controller, H3Event, InboundFrame, IncomingH3Headers,
// };
// use tokio_quiche::quiche::h3;

// async fn wait_for_response(controller: &mut H3Controller<ClientHooks>, target_req_id: u64) {
//     let mut assigned_stream_id = None;

//     while let Some(event) = controller.event_receiver_mut().recv().await {
//         match event {
//             // 1. Map our request_id to the actual QUIC stream_id
//             ClientH3Event::NewOutboundRequest {
//                 request_id,
//                 stream_id,
//             } => {
//                 if request_id == target_req_id {
//                     log::info!(
//                         "Request {} assigned to Stream ID: {}",
//                         request_id,
//                         stream_id
//                     );
//                     assigned_stream_id = Some(stream_id);
//                 }
//             }

//             // 2. Handle Incoming Headers
//             ClientH3Event::Core(H3Event::IncomingHeaders(IncomingH3Headers {
//                 stream_id,
//                 headers,
//                 mut recv,
//                 ..
//             })) => {
//                 // Only process if this is the stream we are waiting for
//                 if assigned_stream_id == Some(stream_id) {
//                     log::info!("Received Headers for req {}: {:?}", target_req_id, headers);

//                     // Drain the body
//                     'body: while let Some(frame) = recv.recv().await {
//                         match frame {
//                             InboundFrame::Body(pooled, fin) => {
//                                 let text = std::str::from_utf8(&pooled).unwrap_or("<binary>");
//                                 log::info!("Received Body: {:?}, fin={}", text, fin);
//                                 if fin {
//                                     return; // <--- EXIT function when stream is finished
//                                 }
//                             }
//                             _ => {}
//                         }
//                     }
//                 }
//             }

//             // 3. Handle Body Bytes (Alternative event format depending on version)
//             ClientH3Event::Core(H3Event::BodyBytesReceived {
//                 stream_id,
//                 body,
//                 fin,
//                 ..
//             }) => {
//                 if assigned_stream_id == Some(stream_id) {
//                     log::info!(
//                         "Received Body Bytes: {:?}, fin={}",
//                         std::str::from_utf8(&body),
//                         fin
//                     );
//                     if fin {
//                         return;
//                     }
//                 }
//             }

//             // 4. Ignore other events (like Settings, Datagrams, or events for other streams)
//             ClientH3Event::Core(H3Event::IncomingSettings(_)) => {
//                 log::info!("Received Settings (Handshake)");
//             }
//             _ => {
//                 // log::debug!("Ignored event: {:?}", event);
//             }
//         }
//     }
// }

// #[tokio::main]
// async fn main() -> Result<(), Box<dyn std::error::Error>> {
//     let socket = tokio::net::UdpSocket::bind("0.0.0.0:0").await.unwrap();
//     socket.connect("127.0.0.1:4433").await.unwrap();
//     let (_, mut controller) = tokio_quiche::quic::connect(socket, None).await.unwrap();

//     // send the header first
//     controller
//         .request_sender()
//         .send(tokio_quiche::http3::driver::NewClientRequest {
//             request_id: 0,
//             headers: vec![h3::Header::new(b":method", b"GET")],
//             body_writer: None,
//         })
//         .unwrap();

//     wait_for_response(&mut controller, 0).await;

//     controller
//         .request_sender()
//         .send(tokio_quiche::http3::driver::NewClientRequest {
//             request_id: 1,
//             headers: vec![h3::Header::new(b":method", b"GET")],
//             body_writer: None,
//         })
//         .unwrap();

//     wait_for_response(&mut controller, 1).await;

//     // while let Some(event) = controller.event_receiver_mut().recv().await {
//     //     match event {
//     //         ClientH3Event::Core(H3Event::IncomingHeaders(IncomingH3Headers {
//     //             stream_id,
//     //             headers,
//     //             mut recv,
//     //             ..
//     //         })) => {
//     //             log::info!(
//     //                 "incomming headers: stream_id={:?}, headers={:?}",
//     //                 stream_id,
//     //                 headers
//     //             );
//     //             'body: while let Some(frame) = recv.recv().await {
//     //                 match frame {
//     //                     InboundFrame::Body(pooled, fin) => {
//     //                         log::info!(
//     //                             "inbound body: {:?}, fin: {:?}, len: {}",
//     //                             std::str::from_utf8(&pooled),
//     //                             fin,
//     //                             pooled.len()
//     //                         );
//     //                         if fin {
//     //                             log::info!("received full body, exiting");
//     //                             break 'body;
//     //                         }
//     //                     }
//     //                     InboundFrame::Datagram(pooled) => {
//     //                         log::info!("len" = pooled.len(); "inbound datagram");
//     //                     }
//     //                 }
//     //             }
//     //         }
//     //         ClientH3Event::Core(H3Event::BodyBytesReceived { fin: true, .. }) => {
//     //             log::info!("fin received");
//     //             break;
//     //         }
//     //         ClientH3Event::Core(event) => log::info!("received event: {event:?}"),
//     //         ClientH3Event::NewOutboundRequest {
//     //             stream_id,
//     //             request_id,
//     //         } => log::info!(
//     //             "stream_id" = stream_id,
//     //             "request_id" = request_id;
//     //             "sending outbound request"
//     //         ),
//     //     }
//     // }
//     Ok(())
// }

use std::error::Error;

use tokio_quiche::http3::driver::{ClientH3Event, H3Event, InboundFrame, IncomingH3Headers};
use tokio_quiche::quiche::h3;

async fn run_client() -> Result<(), Box<dyn Error>> {
    let socket = tokio::net::UdpSocket::bind("0.0.0.0:0").await?;
    socket.connect("127.0.0.1:4043").await?;
    log::info!("socket connected");
    let (_, mut controller) = tokio_quiche::quic::connect(socket, Some("127.0.0.1"))
        .await
        .unwrap();
    log::info!("quic connected");

    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    let headers = vec![
        h3::Header::new(b":method", b"GET"),   // Value can be uppercase
        h3::Header::new(b":scheme", b"https"), // Value MUST be lowercase "https" or "http"
        h3::Header::new(b":authority", b"127.0.0.1:4043"), // No "https://", just host:port
        h3::Header::new(b":path", b"/"),       // Must start with "/"
    ];

    controller
        .request_sender()
        .send(tokio_quiche::http3::driver::NewClientRequest {
            request_id: 0,
            headers: headers,
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
                    "incomming headers stream_id: {}, headers: {:?}",
                    stream_id,
                    headers
                );
                'body: while let Some(frame) = recv.recv().await {
                    match frame {
                        InboundFrame::Body(pooled, fin) => {
                            log::info!(
                                "inbound body: {:?}, fin: {:?}, len: {:?}",
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
                            log::info!("inbound datagram, len {:?}", pooled.len());
                        }
                    }
                }
            }
            ClientH3Event::Core(H3Event::BodyBytesReceived { fin: true, .. }) => {
                log::info!("fin received");
                break;
            }
            // ClientH3Event::Core(H3Event::ResetStream {
            //     stream_id,
            //     error_code,
            // }) => {
            //     log::error!(
            //         "Stream {} reset by server. Error Code: {:?}",
            //         stream_id,
            //         error_code
            //     );
            // }
            ClientH3Event::Core(event) => log::info!("received event: {event:?}"),
            ClientH3Event::NewOutboundRequest {
                stream_id,
                request_id,
            } => log::info!(
                "sending outbound request, stream_id: {}, request_id: {}",
                stream_id,
                request_id
            ),
        }
    }

    Ok(())
}

#[tokio::main]
async fn main() {
    env_logger::init();
    run_client().await.unwrap();
}
