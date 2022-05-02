(defpackage #:macchiato.recipe
  (:use #:CL)
  (:export #:recipe))

(in-package #:macchiato.recipe)

(defclass recipe ()
  ((name
    :initarg :name
    :type string)
   (version
    :initarg :version)))
