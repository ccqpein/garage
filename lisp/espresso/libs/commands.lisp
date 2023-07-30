(defpackage #:espresso/libs/command
  (:documentation
   "libs codes for commands")
  (:use #:CL)
  (:export #:command
		   #:make-command
		   #:command-comm
		   #:run-command)
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

(defun run-command (comm args)
  (apply (command-func comm) args))
