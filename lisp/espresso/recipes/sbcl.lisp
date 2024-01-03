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

(defun download-sbcl-source-code ()
  (shell-run-program
   (format nil "wget -P ~a ~a"
           *espresso-cache-folder*
           "https://downloads.sourceforge.net/project/sbcl/sbcl/2.4.0/sbcl-2.4.0-source.tar.bz2")))

(defun install-sbcl ()
  )

(setf *RECIPES*
	  (list
;;; add your recipes inside here
	   ))
