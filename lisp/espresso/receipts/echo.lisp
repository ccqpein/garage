(defpackage #:espresso/receipts/echo
  (:use #:CL #:espresso/receipts)
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  (:import-from #:espresso/libs/shell-util
				#:sbcl-run-command))

(in-package #:espresso/receipts/echo)

(defun echo ()
  (format *receipts-output* "inside hello~%~%")
  (sbcl-run-command *receipts-output*
					"echo" "hello hello"))

(register-receipt "echo"
				  (make-instance 'standard-receipt
								 :install-func #'echo))
