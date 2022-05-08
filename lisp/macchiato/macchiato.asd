;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:macchiato-sys
  (:use #:CL #:asdf))

(in-package #:macchiato-sys)

(defsystem "macchiato"
  :version (:read-file-form "version")
  :author "ccQpein"
  :licence "MIT"
  :defsystem-depends-on (
                         "trivial-download"
                         "str"
                         "chipz"
                         "archive"
                         )
  :components ((:module "lisp"
                :components
                ((:file "init"))))
  )
