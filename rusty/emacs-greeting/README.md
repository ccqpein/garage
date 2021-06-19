# README #

`ln -fs libemacs_greeting.dylib emacs-greeting.so`

```elisp
(add-to-list 'load-path "/path/to/garage/rusty/emacs-greeting/target/debug")

(require 'emacs-greeting)

(emacs-greeting-say-hello "aa")
```
