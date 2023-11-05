(defpackage #:espresso/recipes
  (:use #:CL #:espresso/config)
  (:export #:*recipes-output*
		   #:*recipes-error*

		   #:install
		   #:update
		   #:upgrade
		   #:uninstall
		   
		   #:standard-recipe
		   
		   #:register-recipe
		   #:look-up-recipe
		   ))

(in-package #:espresso/recipes)

(defvar *recipes-table* (make-hash-table :test 'equal))

(defvar *recipes-output* t)

(defvar *recipes-error* t)

(defclass root-recipe () nil)

(defclass standard-recipe (root-recipe)
  ((recipe-version :initarg :recipe-version
				   :accessor recipe-version)
   (install-func :initarg :install-func
				 :accessor install-func)
   (update-func :initarg :update-func
				:accessor update-func)
   (upgrade-func :initarg :upgrade-func
				 :accessor upgrade-func)
   (uninstall-func :initarg :uninstall-func
				   :accessor uninstall-func)
   (installed-verify :initarg :installed-verify
					 :accessor installed-verify)))

(defun look-up-recipe (filename &optional (version filename))
  "filename:version should be emacs:emacs or emacs:master"
  (declare (string filename version))
  (let ((r (gethash (str:concat filename ":" version) *recipes-table*))
		)
	(or	(if r (progn (format t "Find recipe ~a on version ~a~%" filename version)
					 r)
			nil)
		(progn
		  ;;:= need to load fasl file too
		  (load (format nil "~a/~a.lisp" *espresso-recipes-folder* filename))

		  ;; 
		  (load-all-recipes-from-package
		   (find-package (read-from-string
						  (format nil "#:espresso/recipes/~a" filename)))
		   filename)
			   
		  ;; 
		  (alexandria:if-let (r (gethash (str:concat filename ":" version) *recipes-table*))
			(progn (format t "Find recipe ~a on version ~a~%" filename version)
				   r)
			(format t "Cannot find recipe ~a on version ~a~%" filename version))))))

(defun load-all-recipes-from-package (package filename)
  (let ((all-rs (eval (find-symbol "*RECIPES*" package))))
	;; the first one will be the default one like: emacs:emacs
	(register-recipe (str:concat filename ":" filename)
					  (car all-rs))

	;; then register with the version
	(dolist (r all-rs)
	  (register-recipe (str:concat filename ":" (recipe-version r))
						r))))

(defmethod install ((r standard-recipe) &rest args &key (output *recipes-output*) (error-output *recipes-error*) &allow-other-keys)
  (let ((*recipes-output* output)
		(*recipes-error* error-output))
	(apply (install-func r) args)))

(defmethod update ((r standard-recipe) &rest args &key (output *recipes-output*) (error-output *recipes-error*) &allow-other-keys)
  (let ((*recipes-output* output)
		(*recipes-error* error-output))
	(apply (update-func r) args)))

(defmethod upgrade ((r standard-recipe) &rest args &key (output *recipes-output*) (error-output *recipes-error*) &allow-other-keys)
  (let ((*recipes-output* output)
		(*recipes-error* error-output))
	(apply (upgrade-func r) args)))

(defmethod uninstall ((r standard-recipe) &rest args &key (output *recipes-output*) (error-output *recipes-error*) &allow-other-keys)
  (let ((*recipes-output* output)
		(*recipes-error* error-output))
	(if (apply (installed-verify r) args)
		(apply (uninstall-func r) args))))

(defun register-recipe (name recipe)
  (setf (gethash name *recipes-table*) recipe))
