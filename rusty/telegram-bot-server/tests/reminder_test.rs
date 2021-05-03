use telegram_bot::ChatId;
use telegram_bot_server::{deliver::Msg2Deliver, reminder::*};
use tokio::{runtime, time::Duration};
use tokio::{
    sync::mpsc::{channel, Receiver, Sender},
    time::sleep,
};

fn init() -> (Reminder, Sender<Msg2Reminder>, Receiver<Msg2Deliver>) {
    let (reminder_sender, reminder_receiver) = channel::<Msg2Reminder>(5);
    let (deliver_sender, deliver_receiver) = channel::<Msg2Deliver>(5);
    (
        Reminder::new(reminder_receiver, deliver_sender),
        reminder_sender,
        deliver_receiver,
    )
}

#[test]
fn new_reminder_test() {
    let (mut re, snd, mut deliver_rcv) = init();
    let rt = runtime::Builder::new_multi_thread()
        .enable_time()
        .worker_threads(1) // just one thread
        .build()
        .unwrap();

    // make new reminder
    let msg = Msg2Reminder::new(
        ReminderComm::New,
        (ChatId::from(1), String::from("test")),
        String::from("content"),
        Duration::from_millis(500),
    );

    let cancel_msg = Msg2Reminder::new(
        ReminderComm::Cancel,
        (ChatId::from(1), String::from("test")),
        String::from(""),
        Duration::from_millis(0),
    );

    // run reminder
    rt.spawn(async move { re.run().await });

    // make new reminder
    let snd1 = snd.clone();
    rt.spawn(async move { snd1.send(msg).await });

    // cancel reminder
    rt.spawn(async move {
        // only stop before next reminder
        sleep(Duration::from_millis(900)).await;
        snd.send(cancel_msg).await
    });

    // check deliver channel
    // should have two message
    let handler = rt.spawn(async move {
        sleep(Duration::from_millis(1600)).await;
        deliver_rcv.close();
        let mut result = vec![];
        loop {
            match deliver_rcv.recv().await {
                Some(m) => result.push(m),
                None => break,
            }
        }
        result
    });

    rt.block_on(async move {
        let re = handler.await.unwrap();
        assert_eq!(re.len(), 2);
        println!("{:?}", re);
    });
}
