;;;; -*- Mode: Lisp -*-
(defpackage cicd-sys
  (:use #:CL #:asdf))

(in-package cicd-sys)

(defsystem ccq-cicd
  :name "ccq-cicd"
  :components ((:file "event"))
  )
