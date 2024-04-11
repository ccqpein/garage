use learn_wgpu::*;
use log::info;
use tokio::*;

#[tokio::main]
async fn main() {
    env_logger::init();
    info!("start");
    //run1_1();
    run1_2().await
}
