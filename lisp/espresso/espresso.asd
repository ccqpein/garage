;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:espresso-sys
  (:use #:CL #:asdf))

(in-package #:espresso-sys)

(defsystem espresso
  :name "espresso"
  :author "ccQpein"
  :maintainer "ccQpein"
  :defsystem-depends-on ("uiop")
  :serial t
  :components ((:module "libs"
						:components ((:file "commands")
									 (:file "shell-util")
									 (:file "plugin" :depends-on ("commands"))
									 (:file "package")))))
