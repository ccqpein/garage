(defpackage #:espresso/receipts/build-emacs
  (:use #:CL #:espresso/receipts)
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  (:import-from #:espresso/libs/shell-util
				#:sbcl-run-command))

(in-package #:espresso/receipts/build-emacs)

;; cache folder
(defparameter *download-folder* "~/Downloads")

(defun download-and-build-souce-code ()
  (let ((output (make-string-output-stream))
		(download-path (pathname (format nil
										"~a/build-emacs-for-macos"
										*download-folder*))))
	(format t "download-path: ~a~%" download-path)
	(unless (if-file-exist download-path)
	  (sbcl-run-command
	   output
	   "git" "clone" "https://github.com/jimeh/build-emacs-for-macos.git" "download-path"))

	(uiop:with-current-directory (download-path)
	  (format t "jump inside ~a~%" download-path)
	  (sbcl-run-command output
						"./build-emacs-for-macos")
	  )))

(register-receipt "emacs"
				  (make-instance 'standard-receipt
								 :install-func #'download-and-build-souce-code)
				  )
