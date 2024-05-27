;;;; -*- Mode: Lisp -*-

(defpackage :jkl-sys
  (:use :CL :asdf))

(in-package :jkl-sys)

(defsystem jkl
  :defsystem-depends-on ("str")
  :components ((:file "options")
               (:file "core"
                :depends-on ("options")))
  :in-order-to ((test-op (test-op "jkl/tests"))))

(defsystem jkl/tests
  :depends-on ("jkl" "fiveam")
  :components ((:file "options-test")
               )
  :perform (test-op
            (op c)
            (uiop:symbol-call :fiveam :run!
                              (uiop:find-symbol* :options :jkl-options-test))
            (uiop:symbol-call :fiveam :run!
                              (uiop:find-symbol* :core :jkl-core-test))))
