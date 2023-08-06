(defpackage #:espresso/receipts/echo
  (:use #:CL #:espresso/receipts)
  
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  
  (:import-from #:espresso/libs/shell-util
				#:shell-run-program)

  (:export *RECEIPTS*))

(in-package #:espresso/receipts/echo)

(defparameter *RECEIPTS* nil
  "receipts of this pakcage")

(defun echo ()
  (format *receipts-output* "inside hello~%~%")
  (shell-run-program "echo hello && sleep 1 && echo awfe && sleep 1 && echo fefeff" :output *receipts-output*))

(setf *RECEIPTS*
	  (list
	   (make-instance 'standard-receipt
					  :receipt-version "master"
					  :install-func #'echo)))
