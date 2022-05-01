;;; sbcl recipes for installing sbcl on mac
(in-package #:cl-user)

(defpackage #:sbcl-recipe
  (:use #:CL #:macchiato.recipe))

(in-package #:sbcl-recipe)

;;; inherit the super class "recipe" and make a instance named sbcl-recipe
;;; then export it.
;;; then I can call it outside
(defclass sbcl-recipe (recipe))
