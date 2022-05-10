;;; https://mstmetent.blogspot.com/2022/04/using-lisp-libraries-from-other.html

(defun add (x y)
  (+ x y))

(defun a-sum (l)
  (apply #'+ l))

;;; in cl-user in sbcl
(sb-alien:define-alien-callable calc-add int ((x sb-alien:int) (y sb-alien:int) (result (* (* t))))
  (handler-case
      (progn
        ;; deref:
        ;; sap-alien:
        ;; int-sap:
        (setf (deref result)
              (sb-alien:sap-alien (sb-sys:int-sap (sb-kernel:get-lisp-obj-address (add x y))) (* t)))
         0)
    (t (condition) (declare (ignore condition)) 0)))
