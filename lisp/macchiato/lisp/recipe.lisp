(in-package #:macchiato)

(defpackage #:macchiato.recipe
  (:use #:CL)
  (:export #:recipe))

(in-package #:macchiato.recipe)

;; name -> table of versions
(defvar *all-recipes* (make-hash-table :test 'equal))

(defclass recipe ()
  ((name
    :initarg :name
    :type string)
   (version
    :initarg :version
    :type string)))

(defun check-recipe-version (name version)
  "if this recipe name doesn't has this version, return true"
  (cond ((not (gethash name *all-recipes*))
         (setf (gethash name *all-recipes*) (make-hash-table :test 'equal))
         t)
        ((not (gethash version (gethash name *all-recipes*)))
         t)
        (t nil)))

(defmethod initialize-instance :after ((r recipe) &key)
  "after make-instance of recipt, register it inside all recipes"
  (with-slots (name version) r
    (assert (check-recipe-version name version)
            nil
            "~a version ~a has existed in *all-recipes*" name version)
    (setf (gethash version (gethash name *all-recipes* (make-hash-table))) t)))
