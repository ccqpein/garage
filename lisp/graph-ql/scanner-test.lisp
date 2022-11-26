(ql:quickload '("lisp-unit"))

(defpackage #:scanner-test
  (:use #:CL #:lisp-unit)
  (:import-from #:scanner
				#:block-scanner
				#:parenthesis-scanner
				#:tokens
				#:scan
				#:equal-scanner))

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

  (let* ((testcase "{
  human(id: \"1000\") {
    name
    height(unit: FOOT)
  }
  human2(id: \"1000\",              hhhhh : fff) {
    name
    height(unit: FOOT)
  }
}")
		 (scanner (make-instance 'block-scanner))
		 (ss (make-string-input-stream testcase))
		 )

	(read-char ss) ;; read the first {
	(scan scanner ss)	
	(assert-true (equal-scanner
				  scanner
				  (make-instance 'block-scanner
								 :tokens
								 (list "human"
									   (make-instance 'parenthesis-scanner
													  :tokens (list "id" ":" "\"1000\""))
									   (make-instance 'block-scanner
													  :tokens (list "name" "height"
																	(make-instance 'parenthesis-scanner
																				   :tokens (list "unit" ":" "FOOT"))))
									   "human2"
									   (make-instance 'parenthesis-scanner
													  :tokens (list "id" ":" "\"1000\"" "," "hhhhh" ":" "fff"))
									   (make-instance 'block-scanner
													  :tokens (list "name" "height"
																	(make-instance 'parenthesis-scanner
																				   :tokens (list "unit" ":" "FOOT"))))))
				  ))
	)
  )

(define-test parenthesis-scanner-test
  (let* ((testcase "(id: \"1000\",              hhhhh : fff)")
		 (scanner (make-instance 'parenthesis-scanner))
		 (ss (make-string-input-stream testcase)))
	(read-char ss)
	(scan scanner ss)
	(assert-true (equal-scanner
				  scanner
				  (make-instance 'parenthesis-scanner
								 :tokens (list "id" ":" "\"1000\"" "," "hhhhh" ":" "fff"))))
	)
  )

(define-test plain-scanner-test)

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :scanner-test)
  ;;(run-tests '(parenthesis-scanner-test) :scanner-test)
  )
