pub mod rpc_libs;
pub mod rpc_server;

#[derive(Debug)]
pub enum RPCType {
    Msg(String),
    RPC(String),
    Map,
    List,

    /// default value
    V,
}

/// need impl for struct
pub trait ToRPCType {
    fn to_rpc_type() -> RPCType {
        RPCType::V
    }
}
