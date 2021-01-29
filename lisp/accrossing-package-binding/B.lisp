(defpackage B
   (:use #:CL #:A)
   )
(in-package B)

(defun b-test ()
    (let ((*a-sp-v* 1)) ;; cannot let a-test knows
        (a-test))
)
