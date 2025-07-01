use dioxus::prelude::*;

fn Effect() -> Element {
    let mut count = use_signal(|| 0);

    use_effect(move || {
        // When we read count, it becomes a dependency of the effect
        let current_count = count();

        tracing::debug!("{current_count}");
    });

    rsx! {
        button { onclick: move |_| count += 1, "Increment" }

        div { "Count is {count}" }
    }
}

fn Memo() -> Element {
    let mut count = use_signal(|| 0);

    // use_memo creates a tracked value that is derived from count
    // Since we read count inside the closure, it becomes a dependency of the memo
    // Whenever count changes, the memo will rerun
    let half_count = use_memo(move || count() / 2);

    use_effect(move || {
        // half_count is itself a tracked value
        // When we read half_count, it becomes a dependency of the effect
        // and the effect will rerun when half_count changes
        tracing::debug!("{half_count}");
    });

    rsx! {
        button { onclick: move |_| count += 1, "Increment" }

        div { "Count is {count}" }
        div { "Half count is {half_count}" }
    }
}

fn Resource() -> Element {
    let mut count = use_signal(|| 0);

    // use_resource creates a tracked value that is derived from count
    // Since we read count inside the closure, it becomes a dependency of the resource
    // Whenever count changes, the resource will rerun
    let half_count = use_resource(move || async move {
        // You can do async work inside resources
        //gloo_timers::future::TimeoutFuture::new(100).await;
        count() / 2
    });

    use_effect(move || {
        // half_count is itself a tracked value
        // When we read half_count, it becomes a dependency of the effect
        // and the effect will rerun when half_count changes
        tracing::debug!("{:?}", half_count());
    });

    rsx! {
        button { onclick: move |_| count += 1, "Change Signal" }

        div { "Count is {count}" }
        div { "Half count is {half_count():?}" }
    }
}

fn Component() -> Element {
    let mut count = use_signal(|| 0);

    rsx! {
        button { onclick: move |_| count += 1, "Change Signal" }

        Count { count: count() }
    }
}

// The count reruns the component when it changes, but it is not a tracked value
#[component]
fn Count(count: i32) -> Element {
    // When you read count inside the memo, it does not subscribe to the count signal
    // because the value is not reactive

    //let double_count = use_memo(move || count * 2);

    // the below one is good
    // so that's means we need some

    // You can manually track a non-reactive value with the use_reactive hook
    let double_count = use_memo(
        // Use reactive takes a tuple of dependencies and returns a reactive closure
        use_reactive!(|(count,)| count * 2),
    );

    rsx! {
        div { "Double count: {double_count}" }
    }
}

// Or the below one can do the same thing
// use the `ReadOnlySignal` rather than the count: i32
// #[component]
// fn Count(count: ReadOnlySignal<i32>) -> Element {
//     // Then when you read count inside the memo, it subscribes to the count signal
//     let double_count = use_memo(move || count() * 2);

//     rsx! {
//         div { "Double count: {double_count}" }
//     }
// }
