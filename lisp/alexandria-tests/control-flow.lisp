(ql:quickload "alexandria")

(defun nth-value-or-test0 ()
  (alexandria:nth-value-or 1
    (values 0 nil 2)
    (values 0 1 3)))

(pprint (macroexpand-1 '(alexandria:nth-value-or 1
                         (values 0 nil 2)
                         (values 0 1 2))))


(defun xor-test0 ()
  (alexandria:xor 1 2))

(pprint
 (macroexpand-1
  '(alexandria:xor 1 2 3)))
