(defpackage #:espresso/receipts/echo
  (:use #:CL #:espresso/receipts)
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  (:import-from #:espresso/libs/shell-util
				#:shell-run-program))

(in-package #:espresso/receipts/echo)

(defun echo ()
  (format *receipts-output* "inside hello~%~%")
  (shell-run-program "echo hello && sleep 1 && echo awfe && sleep 1 && echo fefeff" :output *receipts-output*))

(register-receipt "echo"
				  (make-instance 'standard-receipt
								 :install-func #'echo))
