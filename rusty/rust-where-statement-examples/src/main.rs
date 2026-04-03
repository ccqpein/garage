use std::fmt::{Debug, Display};

// 1. Standard "New Line" format (The one we fixed in Emacs)
// This is preferred when the function signature is long.
pub fn process_data<T, K>(data: T, key: K) -> String
where
    T: Debug + Clone,
    K: Display,
{
    format!("{:?} with key {}", data, key)
}

// 2. Inline "where" clause
// Used for simpler bounds where everything fits on one or two lines.
fn simple_check<T>(item: T)
where
    T: PartialEq,
{
    // ...
}

// 3. Multi-line complex bounds
// When a single type has many requirements, they are often split
// for readability.
pub fn complex_handler<F, T>(func: F, value: T)
where
    F: Fn(T) -> bool + Send + Sync + 'static,
    T: Debug + Display + Default + Clone,
{
    if func(value.clone()) {
        println!("Value is valid: {}", value);
    }
}

// 4. In an "impl" block
// 'where' clauses are very common here to constrain the whole implementation.
struct Container<T>(T);

impl<T> Container<T>
where
    T: Default + Debug,
{
    fn new_and_print() {
        let item = T::default();
        println!("{:?}", item);
    }

    fn some_thing<S>()
    where
        S: Debug + Eq + PartialEq,
    {
    }
}

// 5. Associated types and HRTBs (Higher-Rank Trait Bounds)
// This is the most complex 'where' clause you'll likely see.
trait Transformer {
    type Output;
    fn transform(&self) -> Self::Output;
}

fn execute_transformation<T>(t: T)
where
    T: Transformer,
    for<'a> &'a T::Output: Debug, // Higher-Rank Trait Bound
{
    let result = t.transform();
    println!("{:?}", &result);
}
