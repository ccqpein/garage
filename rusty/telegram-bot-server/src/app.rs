use telegram_bot::{
    types::{requests::SendMessage, MessageKind, Update, UpdateKind},
    Api,
};
use tracing::info;

pub async fn update_router(update: Update, api: &Api) -> Result<(), ()> {
    match update.kind {
        UpdateKind::Message(message) => match message.kind {
            MessageKind::Text { ref data, .. } => {
                info!("Receive message data: {:?}", data);
                api.send(SendMessage::new(
                    message.chat,
                    format!(
                        "Hi, {}! You just wrote '{}'",
                        &message.from.first_name, data
                    ),
                ))
                .await
                .map_err(|_| ())?;
            }
            _ => {}
        },
        _ => {}
    };

    Ok(())
}
