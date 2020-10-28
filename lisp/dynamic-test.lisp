(defun test ()
  (declare (special a)) ; has to declare like this
  (+ a 2))

;; (let ((a 1)) (declare (special a)) (test)) ; call like this

(defparameter *b* 1) ; as special var after compiled

(defun test2 ()
  (+ *b* 2))

(let ((*b* 2)) ; rebind
  (assert (= 4 (test2))))

(assert (= 3 (test2)))
