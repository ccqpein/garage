use emacs::{defun, Env, Result, Value};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module(name = "emacs-greeting")]
fn init(env: &Env) -> Result<()> {
    env.message("Done loading!")?;
    Ok(())
}

// Define a function callable by Lisp code.
#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}
