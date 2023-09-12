(defpackage #:espresso/receipts
  (:use #:CL #:espresso/config)
  (:export #:*receipts-output*
		   #:*receipts-error*

		   #:install
		   #:update
		   #:upgrade
		   #:uninstall
		   
		   #:standard-receipt
		   
		   #:register-receipt
		   #:look-up-receipt
		   ))

(in-package #:espresso/receipts)

(defvar *receipts-table* (make-hash-table :test 'equal))

(defvar *receipts-output* t)

(defvar *receipts-error* t)

(defclass root-receipt () nil)

(defclass standard-receipt (root-receipt)
  ((receipt-version :initarg :receipt-version
					:accessor receipt-version)
   (install-func :initarg :install-func
				 :accessor install-func)
   (update-func :initarg :update-func
				:accessor update-func)
   (upgrade-func :initarg :upgrade-func
				 :accessor upgrade-func)
   (uninstall-func :initarg :uninstall-func
				   :accessor uninstall-func)))

(defun look-up-receipt (filename &optional (version filename))
  "filename:version should be emacs:emacs or emacs:master"
  (declare (string filename version))
  (let ((r (gethash (str:concat filename ":" version) *receipts-table*))
		)
	(or	r
		(progn
		  ;;:= need to load fasl file too
		  (load (format nil "~a/~a.lisp" *espresso-receipts-folder* filename))

		  ;; 
		  (load-all-receipts-from-package
		   (find-package (read-from-string
						  (format nil "#:espresso/receipts/~a" filename)))
		   filename)
			   
		  ;; 
		  (gethash (str:concat filename ":" version) *receipts-table*)))))

(defun load-all-receipts-from-package (package filename)
  (let ((all-rs (eval (find-symbol "*RECEIPTS*" package))))
	;; the first one will be the default one like: emacs:emacs
	(register-receipt (str:concat filename ":" filename)
					  (car all-rs))

	;; then register with the version
	(dolist (r all-rs)
	  (register-receipt (str:concat filename ":" (receipt-version r))
						r))))

(defmethod install ((r standard-receipt) &rest args &key (output *receipts-output*) (error-output *receipts-error*) &allow-other-keys)
  (let ((*receipts-output* output)
		(*receipts-error* error-output))
	(apply (install-func r) args)))

(defmethod update ((r standard-receipt) &rest args &key (output *receipts-output*) (error-output *receipts-error*) &allow-other-keys)
  (let ((*receipts-output* output)
		(*receipts-error* error-output))
	(apply (update-func r) args)))

(defmethod upgrade ((r standard-receipt) &rest args &key (output *receipts-output*) (error-output *receipts-error*) &allow-other-keys)
  (let ((*receipts-output* output)
		(*receipts-error* error-output))
	(apply (upgrade-func r) args)))

(defmethod uninstall ((r standard-receipt) &rest args &key (output *receipts-output*) (error-output *receipts-error*) &allow-other-keys)
  (let ((*receipts-output* output)
		(*receipts-error* error-output))
	(apply (uninstall-func r) args)))

(defun register-receipt (name receipt)
  (setf (gethash name *receipts-table*) receipt))
