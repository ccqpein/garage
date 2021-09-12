pub mod app;

pub mod deliver;
pub mod reminder;
pub mod watcher;

use telegram_bot::{Api, Message};
use tokio::sync::mpsc::{self, Sender};

use deliver::{Deliver, Msg2Deliver};
use reminder::{Msg2Reminder, Reminder};
use watcher::Watcher;

pub fn init(api: Api) -> (Watcher, Deliver, Reminder, Sender<Message>) {
    let (watcher_sender, watcher_receiver) = mpsc::channel::<Message>(32);
    let (deliver_sender, deliver_receiver) = mpsc::channel::<Msg2Deliver>(5);
    let (reminder_sender, reminder_receiver) = mpsc::channel::<Msg2Reminder>(5);

    (
        Watcher::new(
            api.clone(),
            watcher_receiver,
            deliver_sender.clone(),
            reminder_sender,
        ),
        Deliver::new(api, deliver_receiver),
        Reminder::new(reminder_receiver, deliver_sender),
        watcher_sender,
    )
}
