;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :demo-sys
  (:use :cl :asdf))

(in-package :demo-sys)

(defsystem :demo
  :description "Generated Lisp RPC System for demo"
  :version "0.0.1"
  :depends-on ("lisp-rpc")
  :components ((:file "lib")))
