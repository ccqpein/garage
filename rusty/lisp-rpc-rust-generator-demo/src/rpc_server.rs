use anyhow::{Result, anyhow};
use lisp_rpc_rust_serializer::lisp_rpc_from_str;
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::TcpListener;

/// Abstract transport layer trait
#[allow(async_fn_in_trait)]
pub trait Transport {
    async fn listen<H>(&self, handler: H) -> Result<()>
    where
        H: Fn(String) -> tokio::task::JoinHandle<Result<String>> + Send + Sync + 'static + Clone;
}

/// TCP Implementation
pub struct TcpTransport {
    addr: String,
}

impl TcpTransport {
    pub fn new(addr: &str) -> Self {
        Self {
            addr: addr.to_string(),
        }
    }
}

impl Transport for TcpTransport {
    async fn listen<H>(&self, handler: H) -> Result<()>
    where
        H: Fn(String) -> tokio::task::JoinHandle<Result<String>> + Send + Sync + 'static + Clone,
    {
        let listener = TcpListener::bind(&self.addr).await?;
        println!("TCP RPC Server listening on {}", self.addr);

        loop {
            let (mut stream, _) = listener.accept().await?;
            let h = handler.clone();

            tokio::spawn(async move {
                let mut buffer = [0; 4096];
                match stream.read(&mut buffer).await {
                    Ok(n) if n > 0 => {
                        let raw_data = String::from_utf8_lossy(&buffer[..n]).to_string();
                        // Call the dispatcher
                        let result_fut = h(raw_data);
                        match result_fut.await {
                            Ok(Ok(response)) => {
                                let _ = stream.write_all(response.as_bytes()).await;
                            }
                            _ => {
                                let _ = stream.write_all(b"error").await;
                            }
                        }
                    }
                    _ => {}
                }
            });
        }
    }
}

/// The type-erased handler trait
trait RpcHandler: Send + Sync {
    fn handle(&self, raw_data: &str) -> Result<String>;
}

/// A concrete handler that knows its own type T
struct TypedHandler<T, F> {
    func: F,
    _phantom: std::marker::PhantomData<T>,
}

impl<T, F> RpcHandler for TypedHandler<T, F>
where
    T: DeserializeOwned + Debug + Send + Sync,
    F: Fn(T) -> Result<String> + Send + Sync,
{
    fn handle(&self, raw_data: &str) -> Result<String> {
        // Here we deserialize the raw data into the specific type T
        let req: T =
            lisp_rpc_from_str(raw_data).map_err(|e| anyhow!("Deserialization failed: {}", e))?;
        (self.func)(req)
    }
}

/// RPCServer now handles multiple types via a Registry
pub struct RPCServer<Tr>
where
    Tr: Transport,
{
    transport: Tr,
    handlers: Arc<HashMap<String, Box<dyn RpcHandler>>>,
}

impl<Tr> RPCServer<Tr>
where
    Tr: Transport,
{
    pub fn new(transport: Tr) -> Self {
        Self {
            transport,
            handlers: Arc::new(HashMap::new()),
        }
    }

    /// Builder-like method to register handlers for different commands
    pub fn register<T, F>(mut self, command: &str, func: F) -> Self
    where
        T: DeserializeOwned + Debug + Send + Sync + 'static,
        F: Fn(T) -> Result<String> + Send + Sync + 'static,
    {
        let handler = TypedHandler {
            func,
            _phantom: std::marker::PhantomData,
        };
        Arc::get_mut(&mut self.handlers)
            .unwrap()
            .insert(command.to_string(), Box::new(handler));
        self
    }

    pub async fn run(self) -> Result<()> {
        //:= should I use lock? can I make it unlock?
        let handlers = self.handlers.clone();

        self.transport
            .listen(move |raw_data| {
                let handlers = handlers.clone();
                tokio::spawn(async move {
                    // 1. Extract the command name from the Lisp string (e.g., "(command-name ...)")
                    let command = extract_command_name(&raw_data)
                        .ok_or_else(|| anyhow!("Invalid RPC format"))?;

                    // 2. Find the registered handler
                    let handler = handlers
                        .get(&command)
                        .ok_or_else(|| anyhow!("Unknown command: {}", command))?;

                    // 3. Let the handler do its magic (deserializing and executing)
                    handler.handle(&raw_data)
                })
            })
            .await
    }
}

/// Helper to get the first symbol from "(symbol ...)"
fn extract_command_name(raw: &str) -> Option<String> {
    let trimmed = raw.trim().trim_start_matches('(');
    trimmed.split_whitespace().next().map(|s| s.to_string())
}

/// Demo of how it would look with Actix-like "Data" concept
/// Actix uses `web::Data<T>` to inject state.
/// We could implement a `FromRequest`-like trait for our RPC types.
pub mod actix_demo {
    // TODO: if we used actix-web as transport:
    // async fn handler(raw: String, server: web::Data<RPCServer>) -> impl Responder {
    //    let res = server.dispatch(raw).await;
    //    HttpResponse::Ok().body(res)
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rpc_libs::BookInfo;

    #[tokio::test]
    async fn test_multi_type_dispatch() {
        let transport = TcpTransport::new("127.0.0.1:0"); // Random port
        let server = RPCServer::new(transport).register::<BookInfo, _>("book-info", |info| {
            println!("Received BookInfo: {:?}", info);
            Ok(format!("Processed {}", info.title))
        });
        // We can register other types here too:
        // .register::<GetBook, _>("get-book", ...)

        let raw_data = r#"(book-info :lang (language-perfer :lang "english") :title "hello world" :version "1984" :id "123")"#;

        // Manual dispatch test
        let cmd = extract_command_name(raw_data).unwrap();
        let handler = server.handlers.get(&cmd).unwrap();
        let res = handler.handle(raw_data).unwrap();

        assert_eq!(res, "Processed hello world");
    }
}
