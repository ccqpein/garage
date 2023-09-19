(defpackage #:espresso/recipes/echo
  (:use #:CL #:espresso/recipes)
  
  (:import-from #:espresso/libs/fs
				#:if-file-exist)
  
  (:import-from #:espresso/libs/shell-util
				#:shell-run-program)

  (:export *RECIPES*))

(in-package #:espresso/recipes/echo)

(defparameter *RECIPES* nil
  "recipes of this pakcage")

(defun echo (&rest rest)
  (declare (ignore rest))
  (format *recipes-output* "inside hello~%~%")
  (shell-run-program "echo hello && sleep 1 && echo awfe && sleep 1 && echo fefeff" :output *recipes-output*))

(setf *RECIPES*
	  (list
	   (make-instance 'standard-recipe
					  :recipe-version "master"
					  :install-func #'echo)))
