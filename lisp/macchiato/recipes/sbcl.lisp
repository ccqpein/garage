;;; sbcl recipes for installing sbcl on mac
(defpackage #:sbcl-recipe
  (:use #:CL))

(in-package #:sbcl-recipe)

;;; inherit the super class "recipe" and make a instance named sbcl-recipe
;;; then export it.
;;; then I can call it outside
