(load "./main.lisp")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream "{hero {name}}")))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )
