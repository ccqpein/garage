;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:telegram-bot-sys
  (:use #:CL #:asdf))

(in-package #:telegram-bot-sys)

(defsystem telegram-bot
  :name "telegram-bot"
  :version (:read-file-form "version")
  :author "ccQpein"
  :maintainer "ccQpein"

  :defsystem-depends-on ("yason")

  :components ((:file "api-doc")))