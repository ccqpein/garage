(defmacro macro1 ()
  (sleep 3)
  (format t "hello")
  `(print "expand")
  )

(defun func1 ()
  (macro1))
