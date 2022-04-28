;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpacakge #:macchiato-sys
  (:use #:CL #:asdf))

(in-package #:macchiato-sys)

(defsystem "macchiato"
  :version (:read-file-form "version")
  :author "ccQpein"
  :licence "MIT"
  :defsystem-depends-on ()
  )
