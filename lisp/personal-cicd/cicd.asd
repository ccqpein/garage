;;;; -*- Mode: Lisp -*-
(defpackage cicd-sys
  (:use #:CL #:asdf))

(in-package cicd-sys)

(defsystem cicd
  :name "ccq-cicd"
  :defsystem-depends-on ("str"
                         "alexandria"
                         "cl-threadpool"
                         )
  :components ((:file "event")
               (:file "rules")
               (:file "jobs"
                :depends-on ("rules")))
  )
