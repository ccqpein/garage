use leptos::{ev::SubmitEvent, *};

fn main() {
    //mount_to_body(|cx| view! { cx,  <p>"Hello, world?"</p> })
    mount_to_body(|cx| view! { cx, <App/> })
}

// #[component]
// fn App(cx: Scope) -> impl IntoView {
//     let (count, set_count) = create_signal(cx, 0);

//     let double_count = move || count() * 2;

//     view! { cx,
//         <button
//             on:click=move |_| {
//                 set_count.update(|n| *n += 1);
//             }
//             // the class: syntax reactively updates a single class
//             // here, we'll set the `red` class when `count` is odd
//             class:red=move || count() % 2 == 1
//         >
//             "Click me"
//         </button>
//         // NOTE: self-closing tags like <br> need an explicit /
//         <br/>

//         // We'll update this progress bar every time `count` changes
//         <progress
//             // static attributes work as in HTML
//             max="50"

//             // passing a function to an attribute
//             // reactively sets that attribute
//             // signals are functions, so this <=> `move || count.get()`
//             value=count
//         >
//         </progress>
//         <br/>

//         // This progress bar will use `double_count`
//         // so it should move twice as fast!
//         <progress
//             max="50"
//             // derived signals are functions, so they can also
//             // reactive update the DOM
//             value=double_count
//         >
//         </progress>
//             //<p>"Count: " {count()}</p> // inreactive
//             <p>"Count: " {count}</p>
//         <p>"Double Count: " {double_count}</p>
//     }
// }

//==========================================================

// #[component]
// fn ProgressBar(cx: Scope, progress: ReadSignal<i32>) -> impl IntoView {
//     view! { cx,
//         <progress
//             max="50"
//             // now this works
//             value=progress
//         ></progress>
//     }
// }

// #[component]
// fn ProgressBar(
//     cx: Scope,
//     // mark this prop optional
//     // you can specify it or not when you use <ProgressBar/>
//     //#[prop(optional)] max: u16,
//     #[prop(default = 100)] max: u16,
//     //progress: ReadSignal<i32>,
//     #[prop(into)] progress: Signal<i32>,
// ) -> impl IntoView {
//     view! { cx, <progress max={max} value=progress></progress> }
// }

// #[component]
// fn App(cx: Scope) -> impl IntoView {
//     let (count, set_count) = create_signal(cx, 0);
//     let double_count = move || count() * 2;
//     view! { cx,
//         <button on:click=move |_| {
//             set_count.update(|n| *n += 1);
//         }>"Click me"</button>
//         // now we use our component!
//             <ProgressBar max=50 progress=count/>
//             <ProgressBar progress=count/>
//             <ProgressBar max=50 progress=Signal::derive(cx, double_count)/>
//     }
// }

//==========================================================

// #[component]
// fn Lists(cx: Scope) -> impl IntoView {
//     let values = vec![0, 1, 2];
//     view! { cx,
//         // this will just render "012"
//         <p>{values.clone()}</p>
//         // or we can wrap them in <li>
//         <ul>{values.iter().map(|n| view! { cx, <li>{n.clone()}</li> }).collect::<Vec<_>>()}</ul>

//         // or we can wrap them in <li>
//         <ul>{values.into_iter().map(|n| view! { cx, <li>{n}</li> }).collect_view(cx)}</ul>
//     }
// }

// #[component]
// fn N_signal_test(cx: Scope) -> impl IntoView {
//     // create a list of N signals
//     let counters = (1..=10).map(|idx| create_signal(cx, idx));

//     // each item manages a reactive view
//     // but the list itself will never change
//     let counter_buttons = counters
//         .map(|(count, set_count)| {
//             view! { cx,
//                 <li>
//                     <button on:click=move |_| set_count.update(|n| *n += 1)>{count}</button>
//                 </li>
//             }
//         })
//         .collect_view(cx);

//     view! { cx, <ul>{counter_buttons}</ul> }
// }

// #[component]
// fn DynamicList(
//     cx: Scope,
//     /// The number of counters to begin with.
//     initial_length: usize,
// ) -> impl IntoView {
//     // This dynamic list will use the <For/> component.
//     // <For/> is a keyed list. This means that each row
//     // has a defined key. If the key does not change, the row
//     // will not be re-rendered. When the list changes, only
//     // the minimum number of changes will be made to the DOM.

//     // `next_counter_id` will let us generate unique IDs
//     // we do this by simply incrementing the ID by one
//     // each time we create a counter
//     let mut next_counter_id = initial_length;

//     // we generate an initial list as in <StaticList/>
//     // but this time we include the ID along with the signal
//     let initial_counters = (0..initial_length)
//         .map(|id| (id, create_signal(cx, id + 1)))
//         .collect::<Vec<_>>();

//     // now we store that initial list in a signal
//     // this way, we'll be able to modify the list over time,
//     // adding and removing counters, and it will change reactively
//     let (counters, set_counters) = create_signal(cx, initial_counters);

//     let add_counter = move |_| {
//         // create a signal for the new counter
//         let sig = create_signal(cx, next_counter_id + 1);
//         // add this counter to the list of counters
//         set_counters.update(move |counters| {
//             // since `.update()` gives us `&mut T`
//             // we can just use normal Vec methods like `push`
//             counters.push((next_counter_id, sig))
//         });
//         // increment the ID so it's always unique
//         next_counter_id += 1;
//     };

//     view! { cx,
//         <div>
//             <button on:click=add_counter>
//                 "Add Counter"
//             </button>
//             <ul>
//                 // The <For/> component is central here
//                 // This allows for efficient, key list rendering
//                 <For
//                     // `each` takes any function that returns an iterator
//                     // this should usually be a signal or derived signal
//                     // if it's not reactive, just render a Vec<_> instead of <For/>
//                     each=counters
//                     // the key should be unique and stable for each row
//                     // using an index is usually a bad idea, unless your list
//                     // can only grow, because moving items around inside the list
//                     // means their indices will change and they will all rerender
//                     key=|counter| counter.0
//                     // the view function receives each item from your `each` iterator
//                     // and returns a view
//                     view=move |cx, (id, (count, set_count))| {
//                         view! { cx,
//                             <li>
//                                 <button
//                                     on:click=move |_| set_count.update(|n| *n += 1)
//                                 >
//                                     {count}
//                                 </button>
//                                 <button
//                                     on:click=move |_| {
//                                         set_counters.update(|counters| {
//                                             counters.retain(|(counter_id, _)| counter_id != &id)
//                                         });
//                                     }
//                                 >
//                                     "Remove"
//                                 </button>
//                             </li>
//                         }
//                     }
//                 />
//             </ul>
//         </div>
//     }
// }

// #[component]
// fn App(cx: Scope) -> impl IntoView {
//     view! { cx,
//             <Lists/>
//             <N_signal_test/>
//             <DynamicList initial_length=10/>
//     }
// }

//==========================================================

// #[component]
// fn App(cx: Scope) -> impl IntoView {
//     view! { cx,
//         <h2>"Controlled Component"</h2>
//         <ControlledComponent/>
//         <h2>"Uncontrolled Component"</h2>
//         <UncontrolledComponent/>
//     }
// }

// #[component]
// fn ControlledComponent(cx: Scope) -> impl IntoView {
//     // create a signal to hold the value
//     let (name, set_name) = create_signal(cx, "Controlled".to_string());

//     view! { cx,
//         <input type="text"
//             // fire an event whenever the input changes
//             on:input=move |ev| {
//                 // event_target_value is a Leptos helper function
//                 // it functions the same way as event.target.value
//                 // in JavaScript, but smooths out some of the typecasting
//                 // necessary to make this work in Rust
//                 set_name(event_target_value(&ev));
//             }

//             // the `prop:` syntax lets you update a DOM property,
//             // rather than an attribute.
//             //
//             // IMPORTANT: the `value` *attribute* only sets the
//             // initial value, until you have made a change.
//             // The `value` *property* sets the current value.
//             // This is a quirk of the DOM; I didn't invent it.
//             // Other frameworks gloss this over; I think it's
//             // more important to give you access to the browser
//             // as it really works.
//             //
//             // tl;dr: use prop:value for form inputs
//             prop:value=name
//         />
//         <p>"Name is: " {name}</p>
//     }
// }

// #[component]
// fn UncontrolledComponent(cx: Scope) -> impl IntoView {
//     // import the type for <input>
//     use leptos::html::Input;

//     let (name, set_name) = create_signal(cx, "Uncontrolled".to_string());

//     // we'll use a NodeRef to store a reference to the input element
//     // this will be filled when the element is created
//     let input_element: NodeRef<Input> = create_node_ref(cx);

//     // fires when the form `submit` event happens
//     // this will store the value of the <input> in our signal
//     let on_submit = move |ev: SubmitEvent| {
//         // stop the page from reloading!
//         ev.prevent_default();

//         // here, we'll extract the value from the input
//         let value = input_element()
//             // event handlers can only fire after the view
//             // is mounted to the DOM, so the `NodeRef` will be `Some`
//             .expect("<input> to exist")
//             // `NodeRef` implements `Deref` for the DOM element type
//             // this means we can call`HtmlInputElement::value()`
//             // to get the current value of the input
//             .value();
//         set_name(value);
//     };

//     view! { cx,
//         <form on:submit=on_submit>
//             <input type="text"
//                 // here, we use the `value` *attribute* to set only
//                 // the initial value, letting the browser maintain
//                 // the state after that
//                 value=name

//                 // store a reference to this input in `input_element`
//                 node_ref=input_element
//             />
//             <input type="submit" value="Submit"/>
//         </form>
//         <p>"Name is: " {name}</p>
//     }
// }

//==========================================================

// #[component]
// fn App(cx: Scope) -> impl IntoView {
//     let (value, set_value) = create_signal(cx, 0);
//     let is_odd = move || value() & 1 == 1;
//     let odd_text = move || if is_odd() { Some("How odd!") } else { None };

//     view! { cx,
//         <h1>"Control Flow"</h1>

//         // Simple UI to update and show a value
//         <button on:click=move |_| set_value.update(|n| *n += 1)>
//             "+1"
//         </button>
//         <p>"Value is: " {value}</p>

//         <hr/>

//         <h2><code>"Option<T>"</code></h2>
//         // For any `T` that implements `IntoView`,
//         // so does `Option<T>`

//         <p>{odd_text}</p>
//         // This means you can use `Option` methods on it
//         <p>{move || odd_text().map(|text| text.len())}</p>

//         <h2>"Conditional Logic"</h2>
//         // You can do dynamic conditional if-then-else
//         // logic in several ways
//         //
//         // a. An "if" expression in a function
//         //    This will simply re-render every time the value
//         //    changes, which makes it good for lightweight UI
//         <p>
//             {move || if is_odd() {
//                 "Odd"
//             } else {
//                 "Even"
//             }}
//         </p>

//         // b. Toggling some kind of class
//         //    This is smart for an element that's going to
//         //    toggled often, because it doesn't destroy
//         //    it in between states
//         //    (you can find the `hidden` class in `index.html`)
//         <p class:hidden=is_odd>"Appears if even."</p>

//         // c. The <Show/> component
//         //    This only renders the fallback and the child
//         //    once, lazily, and toggles between them when
//         //    needed. This makes it more efficient in many cases
//         //    than a {move || if ...} block
//         <Show when=is_odd
//             fallback=|cx| view! { cx, <p>"Even steven"</p> }
//         >
//             <p>"Oddment"</p>
//         </Show>

//         // d. Because `bool::then()` converts a `bool` to
//         //    `Option`, you can use it to create a show/hide toggled
//         {move || is_odd().then(|| view! { cx, <p>"Oddity!"</p> })}

//         <h2>"Converting between Types"</h2>
//         // e. Note: if branches return different types,
//         //    you can convert between them with
//         //    `.into_any()` (for different HTML element types)
//         //    or `.into_view(cx)` (for all view types)
//         {move || match is_odd() {
//             true if value() == 1 => {
//                 // <pre> returns HtmlElement<Pre>
//                 view! { cx, <pre>"One"</pre> }.into_any()
//             },
//             false if value() == 2 => {
//                 // <p> returns HtmlElement<P>
//                 // so we convert into a more generic type
//                 view! { cx, <p>"Two"</p> }.into_any()
//             }
//             _ => view! { cx, <textarea>{value()}</textarea> }.into_any()
//         }}
//     }
// }

//==========================================================

// #[component]
// fn NumericInput(cx: Scope) -> impl IntoView {
//     let (value, set_value) = create_signal(cx, Ok(0));

//     // when input changes, try to parse a number from the input
//     let on_input = move |ev| set_value(event_target_value(&ev).parse::<i32>());

//     view! { cx,
//         <label>
//             "Type a number (or not!)"
//             <input type="number" on:input=on_input/>
//             <p>
//                 "You entered "
//                 <strong>{value}</strong>
//             </p>
//         </label>
//     }
// }

#[component]
fn NumericInput(cx: Scope) -> impl IntoView {
    let (value, set_value) = create_signal(cx, Ok(0));

    let on_input = move |ev| set_value(event_target_value(&ev).parse::<i32>());

    view! { cx,
        <h1>"Error Handling"</h1>
        <label>
            "Type a number (or something that's not a number!)"
            <input type="number" on:input=on_input/>
            <ErrorBoundary // the fallback receives a signal containing current errors
            fallback=|cx, errors| {
                view! { cx,
                    <div class="error">
                        <p>"Not a number! Errors: "</p>
                        // we can render a list of errors as strings, if we'd like
                        <ul>
                            {move || {
                                errors
                                    .get()
                                    .into_iter()
                                    .map(|(_, e)| view! { cx, <li>{e.to_string()}</li> })
                                    .collect_view(cx)
                            }}
                        </ul>
                    </div>
                }
            }>
                <p>"You entered " <strong>{value}</strong></p>
            </ErrorBoundary>
        </label>
    }
}

#[component]
fn App(cx: Scope) -> impl IntoView {
    view! { cx, <NumericInput/> }
    // let (value, set_value) = create_signal(cx, Ok(0));

    // // when input changes, try to parse a number from the input
    // let on_input = move |ev| set_value(event_target_value(&ev).parse::<i32>());

    // view! { cx,
    //     <h1>"Error Handling"</h1>
    //     <label>
    //         "Type a number (or something that's not a number!)"
    //         <input type="number" on:input=on_input/>
    //         // If an `Err(_) had been rendered inside the <ErrorBoundary/>,
    //         // the fallback will be displayed. Otherwise, the children of the
    //         // <ErrorBoundary/> will be displayed.
    //         <ErrorBoundary
    //             // the fallback receives a signal containing current errors
    //             fallback=|cx, errors| view! { cx,
    //                 <div class="error">
    //                     <p>"Not a number! Errors: "</p>
    //                     // we can render a list of errors
    //                     // as strings, if we'd like
    //                     <ul>
    //                         {move || errors.get()
    //                             .into_iter()
    //                             .map(|(_, e)| view! { cx, <li>{e.to_string()}</li>})
    //                             .collect::<Vec<_>>()
    //                         }
    //                     </ul>
    //                 </div>
    //             }
    //         >
    //             <p>
    //                 "You entered "
    //                 // because `value` is `Result<i32, _>`,
    //                 // it will render the `i32` if it is `Ok`,
    //                 // and render nothing and trigger the error boundary
    //                 // if it is `Err`. It's a signal, so this will dynamically
    //                 // update when `value` changes
    //                 <strong>{value}</strong>
    //             </p>
    //         </ErrorBoundary>
    //     </label>
    // }
}
