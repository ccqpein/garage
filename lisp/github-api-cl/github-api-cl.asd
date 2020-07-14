;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:github-api-cl-sys
  (:use #:CL #:asdf))

(in-package #:github-api-cl-sys)

(defsystem github-api-cl
  :name "github-api-cl"
  :version (:read-file-form "version")
  :author "ccQpein"
  :maintainer "ccQpein"

  :defsystem-depends-on ("str" "yason" "dexador" "lisp-unit" "woo" "clack")

  ;;:= TODO: maybe test have different way to load
  :components ((:file "api-doc")

               (:file "api-doc-test"
                :depends-on ("api-doc"))

               (:file "client"
                :depends-on ("api-doc"))

               (:file "client-test"
                :depends-on ("client"))))
