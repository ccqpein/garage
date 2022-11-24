(ql:quickload '("lisp-unit"))

(defpackage #:scanner-test
  (:use #:CL #:lisp-unit)
  (:import-from #:scanner
				#:block-scanner
				#:tokens
				#:scan))

(in-package #:scanner-test)

(define-test block-scanner-test
	(let* ((testcase "{
  human(id: \"1000\") {
    name
    height
  }
    }")
		   (scanner (make-instance 'block-scanner))
		   (ss (make-string-input-stream testcase))
		   )
	  
	  (scan scanner ss)
	  (format t "~a" (tokens scanner))
	  (assert-equal (tokens scanner)
					'())
	  )
  )

(define-test parenthesis-scanner-test)

(define-test plain-scanner-test)

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :scanner-test))
