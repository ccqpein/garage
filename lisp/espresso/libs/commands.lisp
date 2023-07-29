(defpackage #:libs/command
  (:documentation
   "libs codes for commands")
  (:use #:CL)
  (:export #:command
		   #:make-command)
  )

(in-package #:libs/command)

(defstruct command
  comm
  doc
  func)
