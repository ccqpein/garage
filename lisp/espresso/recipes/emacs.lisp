(defpackage #:espresso/recipes/emacs
  (:use #:CL)
  (:import-from #:espresso/recipes
				#:*recipes-output*
				#:*recipes-error*

				#:standard-recipe)
  
  (:import-from #:espresso/libs/fs
				#:if-file-exist)

  (:import-from #:espresso/libs/shell-util
				#:shell-run-program)

  (:export *RECIPES*))

(in-package #:espresso/recipes/emacs)

(defparameter *RECIPES* nil
  "recipes of this pakcage")

;; cache folder
(defparameter *download-folder* (pathname "~/Downloads/build-emacs-for-macos/"))

(defun download-souce-code ()
  (format t "download-path: ~a~%" *download-folder*)
  (unless (if-file-exist *download-folder*)
	(shell-run-program
	 (format nil
			 "git clone https://github.com/jimeh/build-emacs-for-macos.git ~a"
			 *download-folder*)
	 :output *recipes-output*
	 :error-output *recipes-error*)))

(defun install (version &rest rest)
  (declare (ignore rest))
  (download-souce-code)
  (uiop:with-current-directory (*download-folder*)
	(shell-run-program (format nil "./build-emacs-for-macos ~a" version)
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
	(shell-run-program "./build-emacs-for-macos"
					   :output *recipes-output*
					   :error-output *recipes-error*)
	))

(defun installed-verify (&rest rest))

(setf *recipes*
	  (list (make-instance 'standard-recipe
						   :recipe-version "master"
						   :install-func (lambda (&rest rest)
										   (declare (ignore rest))
										   (install ""))
						   :update-func #'update
						   :upgrade-func #'upgrade)
			(make-instance 'standard-recipe
						   :recipe-version "29"
						   :install-func (lambda (&rest rest)
										   (declare (ignore rest))
										   (install "emacs-29"))
						   :update-func #'update
						   :upgrade-func #'upgrade)
			))
