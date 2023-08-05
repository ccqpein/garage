(defpackage #:espresso/receipts/build-emacs
  (:use #:CL #:espresso/receipts)

  (:import-from #:espresso/libs/fs
				#:if-file-exist)

  (:import-from #:espresso/libs/shell-util
				#:shell-run-program))

(in-package #:espresso/receipts/build-emacs)

;; cache folder
(defparameter *download-folder* "~/Downloads")

(defun download-and-build-souce-code ()
  (let ((download-path (pathname (format nil
										 "~a/build-emacs-for-macos"
										 *download-folder*))))
	(format t "download-path: ~a~%" download-path)
	(unless (if-file-exist download-path)
	  (shell-run-program
	   (format nil
			   "git clone https://github.com/jimeh/build-emacs-for-macos.git ~a"
			   download-path)
	   :output *receipts-output*
	   :error-output *receipts-error*))

	(uiop:with-current-directory (download-path)
	  (format t "jump inside ~a~%" download-path)
	  (shell-run-program "./build-emacs-for-macos"
				   :output *receipts-output*
				   :error-output *receipts-error*)
	  )))

(register-receipt "emacs"
				  (make-instance 'standard-receipt
								 :install-func #'download-and-build-souce-code)
				  )
