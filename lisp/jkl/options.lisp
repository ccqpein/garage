(defpackage :options
  (:use :cl))

(in-package :options)

(defclass option ()
  ((short-option
    :initarg :short-option
    :initform ""
    :accessor short-option)
   (long-option
    :initarg :long-option
    :initform ""
    :accessor long-option)
   (arg
    :initarg :arg
    :initform ""
    :accessor arg)
   (description
    :initarg :description
    :initform ""
    :accessor description))
  (:documentation "option class"))

(defmethod print-object ((opt option) stream)
  (format t "short-option: ~a~%long-option: ~a~%argument: ~a~%description: ~a~%"
          (short-option opt)
          (long-option opt)
          (arg opt)
          (description opt)))

(defclass option1 (option)
  ()
  (:documentation "curl option style:

-d, --data <data> HTTP POST data
"))

(defmethod option-match-string ((opt option1) input &key &allow-other-keys)
  (declare (string input))
  (str:match input
    (("-" short-name ", " "--" long-name " <" arg ">\\s+" des)
     (setf (short-option opt) short-name
           (long-option opt) long-name
           (arg opt) arg
           (description opt) des))
    (("-" short-name ", " "--" long-name "\\s+" des)
     (setf (short-option opt) short-name
           (long-option opt) long-name
           (description opt) des))
    ))

(defclass option2 (option)
  ()
  (:documentation "wget option style:

  -A,  --accept=LIST               comma-separated list of accepted extensions

"))

;;:= need more tests
