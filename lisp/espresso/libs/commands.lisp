(defpackage #:espresso/libs/command
  (:documentation
   "libs codes for commands")
  (:use #:CL)
  (:export #:command
		   #:make-command)
  )

(in-package #:espresso/libs/command)

(defstruct command
  comm
  doc
  func)

(defun set-command-function (comm func)
  "set current command's function"
  (setf (command-func comm) func)
  )

(defun run-command (comm &rest args)
  (apply command-func args))
