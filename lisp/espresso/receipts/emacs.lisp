(defpackage #:espresso/receipts/emacs
  (:use #:CL #:espresso/receipts)

  (:import-from #:espresso/libs/fs
				#:if-file-exist)

  (:import-from #:espresso/libs/shell-util
				#:shell-run-program)

  (:export *RECEIPTS*))

(in-package #:espresso/receipts/emacs)

(defparameter *RECEIPTS* nil
  "receipts of this pakcage")

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

(push (make-instance 'standard-receipt
					 :receipt-version "master"
					 :install-func #'download-and-build-souce-code)
	  *receipts*)

