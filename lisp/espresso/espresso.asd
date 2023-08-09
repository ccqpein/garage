;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:espresso-sys
  (:use #:CL #:asdf))

(in-package #:espresso-sys)

(defsystem espresso
  :name "espresso"
  :author "ccQpein"
  :maintainer "ccQpein"
  :defsystem-depends-on ("uiop" "str" "alexandria" "serapeum")
  :components ((:module "libs"
				:serial t
				:components ((:file "commands")
							 (:file "shell-util")
							 (:file "plugin" :depends-on ("commands"))
							 (:file "fs" :depends-on ("commands"))
							 ))
			   (:module "plugins" :depends-on ("libs") :components ((:file "homebrew")))
			   (:file "config")
			   (:file "receipts" :depends-on ("libs" "config"))
			   (:file "commands" :depends-on ("libs" "plugins" "receipts" "config"))
			   ))
