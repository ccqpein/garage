(defpackage A
   (:use #:CL)
   (:export #:a-test
            #:*a-sp-v
            ))

(in-package A)

;; if only declaim without defvar: export means nothing; b has to bind to a::*a-sp-v*
;;(declaim (special *a-sp-v*))

;; if (defvar *a-sp-v*) without init value, b also cannot bind it.

(defvar *a-sp-v* 1)
(defun a-test (&key (v *a-sp-v*))
    (print v)
)
