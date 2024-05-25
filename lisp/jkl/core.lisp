(defpackage :jkl-core
  (:use :CL :jkl-options)
  )

(in-package :jkl-core)

(defparameter *executor*
  #+sbcl
  #'sb-ext:run-program
  #-(or allegro clasp clisp clozure cmucl ecl gcl lispworks mkcl sbcl scl xcl)
  (error "no implemented")
  "the executor of command")

(defclass command ()
  ((name
    :initarg :name
    :accessor name)
   (options
    :initarg options
    :initform '()
    :accessor options)))

;;:= run command
(defmethod run ((comm command) &rest args &key &allow-other-keys)
  )
