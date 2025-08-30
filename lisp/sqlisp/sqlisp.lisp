;;; some tiny file that write the sql in lisp
;;; just the translate, all s-exp just return the sql statement

(defpackage :sqlisp
  (:use :CL)
  )

(in-package #:sqlisp)

(defparameter *symbols-tables* ())

(defun create (noum &rest args)
  
  )
