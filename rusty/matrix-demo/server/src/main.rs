use matrix_sdk::{
    config::SyncSettings,
    ruma::{events::room::message::SyncRoomMessageEvent, user_id},
    Client,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let alice = user_id!("@alice:example.org");
    let client = Client::builder()
        .server_name(alice.server_name())
        .build()
        .await?;

    // First we need to log in.
    client
        .matrix_auth()
        .login_username(alice, "password")
        .send()
        .await?;

    client.add_event_handler(|ev: SyncRoomMessageEvent| async move {
        println!("Received a message {:?}", ev);
    });

    // Syncing is important to synchronize the client state with the server.
    // This method will never return unless there is an error.
    client.sync(SyncSettings::default()).await?;

    Ok(())
}
