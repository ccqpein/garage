;;; sbcl recipes for installing sbcl on mac
;; (in-package #:cl-user)

;; (defpackage #:sbcl-recipe
;;   (:use #:CL #:macchiato.recipe))

(in-package #:recipe)

(defun sbcl-recipe ()
  (let ((recipe (make-instance 'recipe
                               :name "sbcl-recipe"
                               :version "0.1.0")))
    
    ))
