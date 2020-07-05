(ql:quickload "lisp-unit")

(defpackage #:client-test
  (:use #:CL  #:lisp-unit))

(in-package #:client-test)

(define-test token-p-test
  (assert-false (github-client::token-p
                 (make-instance 'github-client::api-client)))
  
  (assert-true (github-client::token-p
                (make-instance 'github-client::api-client :token "123"))))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :client-test))
