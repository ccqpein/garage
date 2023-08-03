(defpackage #:espresso/receipts
  (:use #:CL)
  (:export #:*cache-folder*

		   #:install
		   #:standard-receipt
		   #:register-receipt
		   #:look-up-receipt
		   ))

(in-package #:espresso/receipts)

;:= need default and need the config
(defparameter *cache-folder*
  #P"~/Desktop"
  "cache folder to download stuffs the receipt need")

(defparameter *receipts-folder* (format nil "~a/receipts" (uiop/os:getcwd)))

(defparameter *receipts-table* (make-hash-table :test 'equal))

(defclass root-receipt () nil)

(defclass standard-receipt (root-receipt)
  ((install-func :initarg :install-func
				 :accessor install-func)))

(defun look-up-receipt (name)
  (let ((r (gethash name *receipts-table*))
		)
	(if r
		(install r)
		(progn (load (format nil "~a/~a.lisp" *receipts-folder* name))
			   (install (gethash name *receipts-table*))))))

;;:= need more args maybe
(defmethod install ((r standard-receipt))
  (funcall (install-func r))
  )

(defun register-receipt (name receipt)
  (if (gethash name *receipts-table*)
	  nil ;;:= should return error
	  (setf (gethash name *receipts-table*) receipt)
	  ))
