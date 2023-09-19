(defpackage #:espresso/recipes/asdf
  (:use #:CL
		#:espresso/libs/fs
		#:espresso/libs/shell-util
		#:espresso/config
		)
  
  (:import-from #:espresso/recipes
				#:*recipes-output*
				#:*recipes-error*

				#:standard-recipe)
  
  (:export *RECIPES*))

(in-package #:espresso/recipes/asdf)

(defparameter *RECIPES* nil
  "recipes of this pakcage")

;;; your things below

(defparameter *download-folder* (pathname (format nil "~a~a" *espresso-cache-folder* "asdf/")))

(defun download-asdf-git-repo ()
  (unless (if-file-exist *download-folder*)
	(shell-run-program
	 (format nil
			 "git clone https://github.com/fare/asdf.git ~a"
			 *download-folder*)
	 :output *recipes-output*
	 :error-output *recipes-error*)

	;; install dependcies after download
	(uiop:with-current-directory (*download-folder*)
	  (shell-run-program
	   "git submodule update --init"
	   :output *recipes-output*
	   :error-output *recipes-error*))
	))

(defun checkout-git-version (version)
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 (format nil
			 "git checkout ~a"
			 version)
	 :output *recipes-output*
	 :error-output *recipes-error*)))

(defun install (version &rest rest)
  (declare (ignore rest))
  (download-asdf-git-repo)
  
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 "git pull"
	 :output *recipes-output*
	 :error-output *recipes-error*)
	
	(checkout-git-version version)
	
	(shell-run-program
	 "tools/asdf-tools install-asdf sbcl"
	 :output *recipes-output*
	 :error-output *recipes-error*)
	))

(defun update (&rest rest)
  (declare (ignore rest))
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program
	 "git pull"
	 :output *recipes-output*
	 :error-output *recipes-error*)	
	))

(defun upgrade (&rest rest)
  (declare (ignore rest))
  (uiop:with-current-directory (*download-folder*)
   	(shell-run-program
	 "tools/asdf-tools install-asdf sbcl"
	 :output *recipes-output*
	 :error-output *recipes-error*)
	))

(defun uninstall (&rest rest)
  (declare (ignore rest))
  (uiop:delete-directory-tree *download-folder* :validate t))

(setf *RECIPES*
	  (list
;;; add your recipes inside here
	   (make-instance 'standard-recipe
					  :recipe-version "master"
					  :install-func (lambda (&rest rest)
									  (declare (ignore rest))
									  (install "master"))
					  :update-func #'update
					  ;;:upgrade-func #'upgrade
					  :uninstall-func #'uninstall)))
