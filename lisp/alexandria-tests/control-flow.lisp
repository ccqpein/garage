;;; https://gitlab.common-lisp.net/alexandria/alexandria/-/blob/master/alexandria-1/control-flow.lisp
(ql:quickload "alexandria")

(defun nth-value-or-test0 ()
  (alexandria:nth-value-or 1
    (values 0 nil 2)
    (values 0 1 3)
    (values 0 1 4)))

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

(defun switch-test0 ()
  (alexandria:switch (10 :test 'equal)
    (1 (format t "1") 2)
    (2 (format t "2") 3)
    (3 (format t "3") 4)
    ((1+ 9) (format t "match") 11)
    ))

(pprint (macroexpand-1 '(alexandria:eswitch (10 :test 'equal)
    (1 (format t "hello"))
    (2)
    (3))))
