(defun fibonacci (n)
  (declare (fixnum n)
           (optimize (speed 3)))
  (if (< n 2)
      n
      (the fixnum (+ (fibonacci (1- n)) (fibonacci (- n 2))))))


(defun fibonacci2 (n)
  (declare (fixnum n)
           (optimize (speed 3)))
  (labels ((inner (a b n)
             (declare (fixnum a b n)
                      (optimize (speed 3)))
             (if ( > n 0)
                 (inner b (+ a b) (1- n))
                 a)))
    (inner 0 1 n)))
