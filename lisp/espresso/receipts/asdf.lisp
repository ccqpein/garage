(defpackage #:espresso/receipts/asdf
  (:use #:CL
		#:espresso/receipts
		#:espresso/libs/fs
		#:espresso/libs/shell-util
		#:espresso/config)
  (:export *RECEIPTS*))

(in-package #:espresso/receipts/asdf)

(defparameter *RECEIPTS* nil
  "receipts of this pakcage")

;;; your things below

(defparameter *download-folder* (pathname (format nil "~a~a" *espresso-cache-folder* "asdf")))

(defun download-asdf-git-repo ()
  (unless (if-file-exist *download-folder*)
	(shell-run-program
	 (format nil
			 "git clone https://github.com/fare/asdf.git ~a"
			 *download-folder*)
	 :output *receipts-output*
	 :error-output *receipts-error*)

	;; install dependcies after download
	(uiop:with-current-directory (*download-folder*)
	  (shell-run-program
	   "git submodule update --init"
	   :output *receipts-output*
	   :error-output *receipts-error*))
	))

(defun checkout-git-version (version)
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 (format nil
			 "git checkout ~a"
			 version)
	 :output *receipts-output*
	 :error-output *receipts-error*)))

(defun install (version &rest rest)
  (declare (ignore rest))
  (download-asdf-git-repo)
  
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 "git pull"
	 :output *receipts-output*
	 :error-output *receipts-error*)
	
	(checkout-git-version version)
	
	(shell-run-program
	 "tools/asdf-tools install-asdf sbcl"
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
	
	(shell-run-program
	 "tools/asdf-tools install-asdf sbcl"
	 :output *receipts-output*
	 :error-output *receipts-error*)
	))

(setf *RECEIPTS*
	  (list
;;; add your receipts inside here
	   (make-instance 'standard-receipt
					  :receipt-version "master"
					  :install-func (lambda () (install "master"))
					  :update-func #'update)

	   ))
