(ql:quickload "str")

(defmacro inner (i)
  `(+ 1 ,i))

(defmacro outer ()
  `(progn ,@(loop for i from 1 to 5
				  for name = (read-from-string (str:concat  "outer-" (write-to-string i)))
				  collect `(defun ,name ()
							 (inner ,i)))))
