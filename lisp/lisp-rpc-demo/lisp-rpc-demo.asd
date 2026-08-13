(defpackage :lisp-rpc-demo-sys
  (:documentation "define the lisp-rpc-demo system")
  (:use :cl :asdf))

(in-package :lisp-rpc-demo-sys)

(defsystem :lisp-rpc-demo
  :description "Demos for Lisp RPC"
  :version "0.0.1"
  :depends-on ("lisp-rpc")
  :components ((:file "homoiconic")
               (:file "raw-data")))
