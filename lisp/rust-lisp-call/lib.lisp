;;; https://mstmetent.blogspot.com/2022/04/using-lisp-libraries-from-other.html

(defun add (x y)
  (+ x y))

(defun a-sum (l)
  (apply #'+ l))

;;; in cl-user in sbcl
(define-alien-callable calc-parse int ((x c-int) (y c-int) (result (* (* t))))
  (handler-case
      (progn
         ;; The following needs to use SBCL internal functions to
         ;; coerce a Lisp object into a raw pointer value. This is
         ;; unsafe and will be fixed in the next section.
        (setf (deref result)
              (sap-alien (int-sap (get-lisp-obj-address (parse source))) (* t)))
         0)
    (t (condition) (declare (ignore condition)) 1)))
