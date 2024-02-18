(defpackage load-thru-git/tests/main
  (:use :cl
        :load-thru-git
        :rove))
(in-package :load-thru-git/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :load-thru-git)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
