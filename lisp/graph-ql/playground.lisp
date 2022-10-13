(load "./main.lisp")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream "{hero {name}}")))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )

(defvar *case0* "{
  human(id: \"1000\") {
    name
    height
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case0*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )


(defvar *case1* "{
  human(id: \"1000\") {
    name
    height(unit: FOOT)
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case1*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )


(defvar *case2* "{
 leftComparison: hero(episode: EMPIRE) {
    ...comparisonFields
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case2*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )
