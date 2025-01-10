(ql:quickload "alexandria")

(defun provide-value ()
  (if (zerop (mod (random 2) 2))
      t
      nil))

(defun test0 ()
  (alexandria:if-let (x (provide-value))
    (format t "x has value")
    (format t "x no value")))

(defun test1 ()
  (alexandria:if-let
      ((x (provide-value))
       (y (provide-value)))
    (format t "x and y have value")
    (format t "x = ~a, y = ~a" x y)))

(defun test2 ()
  (alexandria:when-let (x (provide-value))
    (format t "x has value~%")
    (format t "run with x has value")))

;; (defun test3 ()
;;   (alexandria:if-let* ;; wrong, no if-let*
;;       ((x (provide-value))
;;        (y (provide-value))
;;        (yy y))
;;     (format t "x and y have value")
;;     (format t "x = ~a, y = ~a" x y)))

(defun test4 ()
  (alexandria:when-let*
      ((x (provide-value))
       (y (provide-value))
       (yy y))
    (format t "x has value ~a~%" x)
    (format t "y has value ~a~%" y)
    (format t "run with all have values ~a~%" yy)))
