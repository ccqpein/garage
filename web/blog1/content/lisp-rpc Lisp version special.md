---
title: "Lisp-RPC example: Lisp version special"
date: "2026-09-01"
slug: "lisp-rpc-example-lisp-version-special"
tags: ["lisp-rpc", "lisp"]
---

I made the [lisp-rpc example accountant app](https://ccqpein.me/posts/lisp-rpc-example-accountant). The accountant development process mostly focused on the Rust lib and how to make a backend app. This post is a bit different from that. In the Lisp version, lisp-rpc has some special features, thanks to Lisp's homoiconic magic.

# Homoiconic

Since lisp-rpc is valid Common Lisp syntax from day 1, lisp-rpc automatically gets Lisp's homoiconic magic.

For example, assume I have data like `(hello-world :from "USA")`. This is a valid s-expression, so I can do `(eval (read-from-string data))`.

```lisp
;;; I define the function `hello-world`
(defun hello-world (&rest args &key my-name from)
  (declare (ignore args))
  (format nil "Hello~@[ to ~A~]!~@[ Friend from ~A!~]" my-name from))


;; Then run code below
(let ((pure-data-0 "(hello-world)")
      (pure-data-1 "(hello-world :from \"USA\")")
      (pure-data-2 "(hello-world :from \"Japan\" :my-name \"Mikasa\")"))
  (pprint (eval (read-from-string pure-data-0)))
  (pprint (eval (read-from-string pure-data-1)))
  (pprint (eval (read-from-string pure-data-2))))
;; The results are:
;; "Hello!"
;; "Hello! Friend from USA!"
;; "Hello to Mikasa! Friend from Japan!"
```

And this feature doesn't need any lisp-rpc lib code at all. It just needs a Common Lisp implementation.

# Other general features are still here in Lisp

Other general features in the Rust lib are still here too.

## Raw data

After installing lisp-rpc (I installed it in Quicklisp's local projects path), `(ql:quickload "lisp-rpc")` is all I need to do in the REPL.

I'll still use the lisp-rpc data example from above:

```lisp
(defun run ()
  (let ((data '("(hello-world)"
                "(hello-world :from \"USA\")"
                "(hello-world :from \"Japan\" 
:my-name \"Mikasa\")"
                "(hello-world :from '(\"Japan\" \"Tokyo\") 
:my-name \"Mikasa\")"))
        (data1 "(nest-hello :from (hello-world :from \"USA\"))"))

    (loop for d in data
          do (let ((rd (lisp-rpc-raw-data:parse-data d)))
               (format t "name: ~a, from: ~a, name: ~a~%"
                       (lisp-rpc-raw-data:get-name rd)
                       (lisp-rpc-raw-data:data-get rd :from)
                       (lisp-rpc-raw-data:data-get rd :name))))

    (let* ((rd (lisp-rpc-raw-data:parse-data data1))
           (nest-rd (lisp-rpc-raw-data:data-get rd :from))) ;; nesting data
      (format t "name: ~a~%" (lisp-rpc-raw-data:get-name rd))
      (format t "in nest data's from, name: ~a, from: ~a"
              (lisp-rpc-raw-data:get-name nest-rd)
              (lisp-rpc-raw-data:data-get nest-rd :from)))
    ))
```

Run this function in the REPL:

```lisp
CL-USER> (run)
name: HELLO-WORLD, from: NIL, name: NIL
name: HELLO-WORLD, from: USA, name: NIL
name: HELLO-WORLD, from: Japan, name: NIL
name: HELLO-WORLD, from: #S(RAW-DATA-LIST :L (Japan Tokyo)), name: NIL
name: NEST-HELLO
in nest data's from, name: HELLO-WORLD, from: USA
```

## Spec lib generator

Like the Rust repo, the Lisp version also has a binary executable generator. After running `./make.sh`, there will be a `lisp-rpc-gen` executable in the current path. Then I can run it with `./lisp-rpc-gen gen spec.lisp` like:

```lisp
(def-rpc-package demo)

(def-msg language-preference :lang 'string)

(def-msg book-info
  :lang 'language-preference
  :title 'string
  :version 'string
  :id 'string)

(def-msg authors :names (list 'string))

(def-rpc get-book
    '(:title 'string :version 'string
      :lang '(:lang 'string :encoding 'number)
      :authors 'authors)
  'book-info)

(def-rpc ping-no-pong
    '(:nothing 'string))
```

In the target folder `output`:

```
┌─ demo.asd
├─ lib.lisp
output
```

In `demo.asd`:

```
;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :demo-sys
  (:use :cl :asdf))

(in-package :demo-sys)

(defsystem :demo
  :description "Generated Lisp RPC System for demo"
  :version "0.0.1"
  :depends-on ("lisp-rpc")
  :components ((:file "lib")))
```

And `lib.lisp` contains everything we need. Then we can start a web server and use this lib:

```lisp
(load "./output/demo.asd")

(ql:quickload '("lisp-rpc" "demo"))

(use-package :lisp-rpc-server)
(use-package :demo)

;; 1. Initialize server
(defparameter *server* (make-rpc-server))

;; 2. Register handler (validated against rpc-endpoint-p)
(register-rpc-handler *server*
                      'demo:get-book
                      (lambda (req)
                        ;; req is automatically deserialized into a
                        ;; #S(DEMO:GET-BOOK ...) struct
                        (demo:make-book-info
                         :lang (make-language-preference :lang "en")
                         :title (demo:get-book-title req)
                         :id "12345")))

;; 3. Start server
(start-server *server*)
```

Then I can call the API with this:

```shell
curl -X POST http://localhost:5432 \
     -H "Content-Type: text/plain" \
     -d '(GET-BOOK :TITLE "Land of Lisp" :VERSION "1.0" :LANG (:LANG "en" :ENCODING 1))'
```

Response: 

```
(BOOK-INFO :LANG (LANGUAGE-PREFERENCE :LANG "en") :TITLE "Land of Lisp" :VERSION "" :ID "12345")
```
