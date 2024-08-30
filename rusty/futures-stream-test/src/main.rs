use futures::stream::{self, StreamExt};

#[tokio::main]
async fn main() -> Result<(), String> {
    let mut stream = stream::iter(1..=10);
    let mut stream = Box::pin(stream.filter_map(|x| async move {
        if x % 2 == 0 {
            Some(x + 1)
        } else {
            println!("noop");
            None
        }
    }));

    // let mut stream = stream.filter_map(|x| async move {
    //     if x % 2 == 0 {
    //         Some(x + 1)
    //     } else {
    //         None
    //     }
    // });

    // the next method has to pin stream first

    println!("{:?}", stream.next().await);
    //println!("{:?}", stream.next().await);
    //println!("{:?}", stream.next().await);

    //assert_eq!(vec![3, 5, 7, 9, 11], events.collect::<Vec<_>>().await);

    Ok(())
}
