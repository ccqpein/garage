#![feature(min_specialization)] // This feature flag is required!

#[derive(Debug)]
pub struct TracedError {
    #[allow(dead_code)] // For the example, this field might not be directly used
    inner: Box<dyn std::error::Error + Send + Sync + 'static>,
    #[allow(dead_code)]
    context: Option<String>, // Added for specialization example
}

impl TracedError {
    pub fn new(error: Box<dyn std::error::Error + Send + Sync + 'static>) -> Self {
        TracedError {
            inner: error,
            context: None,
        }
    }
}

pub trait IntoDynTracedError<T> {
    fn traced_dyn(self) -> T;
}

// custom error type
#[derive(Debug)]
struct MyError {
    message: String,
    code: u32,
}

impl std::fmt::Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "MyError: {} (Code: {})", self.message, self.code)
    }
}

impl std::error::Error for MyError {}

// default implementation
impl<E> IntoDynTracedError<TracedError> for E
where
    E: std::error::Error + Send + Sync + 'static,
{
    default fn traced_dyn(self) -> TracedError {
        println!("Using default traced_dyn for {:?}", self); // Indicator
        TracedError::new(Box::new(self))
    }
}

// The more specific implementation for `MyError`
// if comment this function, both structs in main function will print the
impl IntoDynTracedError<TracedError> for MyError {
    fn traced_dyn(self) -> TracedError {
        println!("*** Using specialized traced_dyn for MyError! ***"); // Indicator
        // Here, we can add MyError-specific logic, like adding context
        let mut traced = TracedError::new(Box::new(self));
        traced.context = Some("Error from My specific module".to_string());
        traced
    }
}

fn main() {
    let general_err = std::io::Error::new(std::io::ErrorKind::Other, "error");
    let my_specific_err = MyError {
        message: "Something went wrong in my subsystem".to_string(),
        code: 1001,
    };

    // When we call traced_dyn on general_err, the 'default' impl is chosen
    let _traced_general = general_err.traced_dyn();

    // When we call traced_dyn on my_specific_err, the specialized impl is chosen
    let _traced_my_error = my_specific_err.traced_dyn();
}
