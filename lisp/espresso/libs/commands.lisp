(defpackage #:libs/command
  (:documentation
   "libs codes for commands")
  (:use #:CL)
  )

(in-package #:libs/command)

(defstruct command
  comm
  doc)
