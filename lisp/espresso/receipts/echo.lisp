(defpackage #:espresso/receipts/echo
  (:use #:CL #:espresso/receipts)
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  (:import-from #:espresso/libs/shell-util
				#:run-program))

(in-package #:espresso/receipts/echo)

(defun echo ()
  (format *receipts-output* "inside hello~%~%")
  (run-program "echo hello" :output *receipts-output*))

(register-receipt "echo"
				  (make-instance 'standard-receipt
								 :install-func #'echo))
