;;; shell-run package
(defpackage #:espresso/libs/shell-util
  (:use #:CL)
  (:export #:sbcl-run-command)
  )

(in-package #:espresso/libs/shell-util)

(defun sbcl-run-command (output comm &rest args)
  "run shell command and return the output string stream"
  (let ((out (if output
				 output
				 (make-string-output-stream))))
    (sb-ext:run-program
	 comm
     args
     :search t
     :output out)
    (if output nil out)))
