(defpackage #:espresso/receipts/emacs
  (:use #:CL)
  (:import-from #:espresso/receipts
				#:*receipts-output*
				#:*receipts-error*

				#:standard-receipt)
  
  (:import-from #:espresso/libs/fs
				#:if-file-exist)

  (:import-from #:espresso/libs/shell-util
				#:shell-run-program)

  (:export *RECEIPTS*))

(in-package #:espresso/receipts/emacs)

(defparameter *RECEIPTS* nil
  "receipts of this pakcage")

;; cache folder
(defparameter *download-folder* (pathname "~/Downloads/build-emacs-for-macos/"))

;; (defun download-and-build-souce-code ()
;;   (let ((download-path (pathname (format nil
;; 										 "~a/build-emacs-for-macos"
;; 										 *download-folder*))))
;; 	(format t "download-path: ~a~%" download-path)
;; 	(unless (if-file-exist download-path)
;; 	  (shell-run-program
;; 	   (format nil
;; 			   "git clone https://github.com/jimeh/build-emacs-for-macos.git ~a"
;; 			   download-path)
;; 	   :output *receipts-output*
;; 	   :error-output *receipts-error*))

;; 	(uiop:with-current-directory (download-path)
;; 	  (format t "jump inside ~a~%" download-path)
;; 	  (shell-run-program "./build-emacs-for-macos"
;; 				   :output *receipts-output*
;; 				   :error-output *receipts-error*)
;; 	  )))

(defun download-souce-code ()
  (format t "download-path: ~a~%" *download-folder*)
  (unless (if-file-exist *download-folder*)
	(shell-run-program
	 (format nil
			 "git clone https://github.com/jimeh/build-emacs-for-macos.git ~a"
			 *download-folder*)
	 :output *receipts-output*
	 :error-output *receipts-error*)))

(defun install (version &rest rest)
  (declare (ignore rest))
  (download-souce-code)
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program (format nil "./build-emacs-for-macos ~a" version)
					   :output *receipts-output*
					   :error-output *receipts-error*)
	))

(defun update (&rest rest)
  (declare (ignore rest))
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 "git pull"
	 :output *receipts-output*
	 :error-output *receipts-error*)
	))

(defun upgrade (&rest rest)
  (declare (ignore rest))
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program "./build-emacs-for-macos"
					   :output *receipts-output*
					   :error-output *receipts-error*)
	))

(setf *receipts*
	  (list (make-instance 'standard-receipt
						   :receipt-version "master"
						   :install-func (lambda (&rest rest)
										   (declare (ignore rest))
										   (install ""))
						   :update-func #'update
						   :upgrade-func #'upgrade)
			(make-instance 'standard-receipt
						   :receipt-version "29"
						   :install-func (lambda (&rest rest)
										   (declare (ignore rest))
										   (install "emacs-29"))
						   :update-func #'update
						   :upgrade-func #'upgrade)

			))
