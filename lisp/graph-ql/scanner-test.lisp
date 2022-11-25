(ql:quickload '("lisp-unit"))

(defpackage #:scanner-test
  (:use #:CL #:lisp-unit)
  (:import-from #:scanner
				#:block-scanner
				#:parenthesis-scanner
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
	(format t "a:~{~a~}" (tokens scanner))
	(assert-true (equal-scanner
				  scanner
				  (make-instance 'block-scanner
								 :tokens (list
										  (make-instance 'block-scanner
														 :tokens
														 (list "human"
															   (make-instance 'parenthesis-scanner
																			  :tokens (list "id" ":" "\"1000\""))
															   (make-instance 'block-scanner
																			  :tokens (list "name" "height"))))))))
	)
  )

(define-test parenthesis-scanner-test)

(define-test plain-scanner-test)

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :scanner-test))
