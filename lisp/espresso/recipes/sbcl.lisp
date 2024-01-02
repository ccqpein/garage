(defpackage #:espresso/recipes/sbcl
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

(in-package #:espresso/recipes/sbcl)

(defparameter *RECIPES* nil
  "recipes of this pakcage")

;;; your things below

(defun install-sbcl ()

  )

(setf *RECIPES*
	  (list
;;; add your recipes inside here
	   ))
