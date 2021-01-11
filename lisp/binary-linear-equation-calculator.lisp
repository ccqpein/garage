(defparameter *x-range* 10) ;; (-10, 10)
(defparameter *y-range* 10)

(defun random-num-can-less-0 (limit)
  (if (evenp (random 2))
      (- (random limit))
      (random limit)))

(defun random-num-can-less-0-but-0 (limit)
  (loop for x = (random-num-can-less-0 limit)
        until (/= 0 x)
        finally (return x)))

(defun main ()
  (labels ((get-a (l1 l2)
             (apply #'+
                    (mapcar #'*
                            l1
                            l2))))
    (let* ((x (random-num-can-less-0 *x-range*))
           (y (random-num-can-less-0 *y-range*))
           (matrix (loop
                     repeat 2
                     collect (loop
                               repeat 2
                               collect (random-num-can-less-0-but-0 5))))
           (a1 (get-a (nth 0 matrix) (list x y)))
           (a2 (get-a (nth 1 matrix) (list x y))))
      
      (loop for (xx yy) in matrix
            for a in (list a1 a2)
            do (format t "~dx~@dy = ~a~%" xx yy a))

      (let ((an-x (progn (format t "x is: ") (read)))
            (an-y (progn (format t "y is: ") (read))))
        (if (and (= x an-x) (= y an-y))
            (format t "oh yeah")
            (format t "you are wrong, x is ~a, and y is ~a~%" x y)))
      ;;(values x y matrix a1 a2)
      )))
