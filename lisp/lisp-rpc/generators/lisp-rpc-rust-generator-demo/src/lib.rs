mod rpc_libs;

// macro_rules! impl_to_rpc_data {
//     ($($type:ty),*) => {
//         $(
//             impl ToRPCData for $type {
//                 fn to_rpc(&self) -> String {
//                     format!("{}", self)
//                 }
//             }
//         )*
//     };
// }

trait ToRPCData {
    fn to_rpc(&self) -> String;
}

impl ToRPCData for String {
    fn to_rpc(&self) -> String {
        format!("\"{}\"", self.to_string())
    }
}

impl ToRPCData for i64 {
    fn to_rpc(&self) -> String {
        self.to_string()
    }
}

trait FromRPCData {
    fn from_rpc(&self) -> String;
}
