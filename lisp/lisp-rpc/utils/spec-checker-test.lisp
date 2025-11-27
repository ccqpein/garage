(ql:quickload '("str" "alexandria" "fiveam"))

(defpackage test-lisp-rpc
  (:use #:cl #:lisp-rpc-checker)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is
                #:signals
                #:def-fixture
                #:with-fixture
                #:run-all-tests
                #:run!))

(in-package :test-lisp-rpc)

(def-suite test-lisp-rpc
  :description "Top level test suite")

(def-suite test-def-msg
  :in test-lisp-rpc)

(test list-type-checker-test
  (is (lisp-rpc-checker::list-type-checker 'list 'number))
  (is (not (lisp-rpc-checker::list-type-checker)))
  (is (not (lisp-rpc-checker::list-type-checker 'failed 'a)))
  (is (not (lisp-rpc-checker::list-type-checker 'list '()))))

(test def-msg-format-checker-test
  (is (lisp-rpc-checker::def-msg-checker "a" :a 'string :a '()))
  (signals error (lisp-rpc-checker::def-msg-checker "a" :a 'string :a '(:a 1))))
