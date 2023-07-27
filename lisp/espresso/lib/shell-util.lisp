;;; shell-run package
(defpackage #:shell-util
  (:use #:CL)
  (:export #:sbcl-run-command)
  )

(in-package #:shell-util)

(defun sbcl-run-command (comm &rest args)
  "run shell command and return the output string stream"
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program
	 comm
     args
     :search t
     :output out)
    out))
