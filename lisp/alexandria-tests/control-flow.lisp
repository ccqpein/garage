(ql:quickload "alexandria")

(defun nth-value-or-test0 ()
  (alexandria:nth-value-or 1
    (values 0 nil 2)
    (values 0 1 3)))

(pprint
 (macroexpand-1
  '(alexandria:nth-value-or 1
    (values 0 nil 2)
    (values 0 1 2))))


(defun xor-test0 ()
  (assert (equal '(nil nil) (multiple-value-list (alexandria:xor 1 2))))
  (assert (equal '(2 t) (multiple-value-list (alexandria:xor nil 2))))
  (assert (equal '(1 t) (multiple-value-list (alexandria:xor 1 nil nil))))
  (assert (equal '(nil nil) (multiple-value-list (alexandria:xor 1 2 3))))
  (assert (equal '(3 t) (multiple-value-list (alexandria:xor nil nil 3 nil))))
  (assert (equal '(nil nil) (multiple-value-list (alexandria:xor nil nil 3 4))))
  (assert (equal '(nil nil) (multiple-value-list (alexandria:xor nil 2 3 4)))))

(pprint
 (macroexpand-1
  '(alexandria:xor 1 2 3)))

(defun switch ())
