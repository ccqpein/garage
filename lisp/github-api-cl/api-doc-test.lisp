(defpackage #:api-doc-test
  (:use #:CL  #:lisp-unit))

(in-package #:api-doc-test)

(define-test coerce-parameter-type-test
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "string"))
  (assert-equal "aa" (github-api-doc::coerce-parameter-type "aa" "string"))

  ;; from (read-line) or keywords
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "boolean"))
  (assert-equal "false" (github-api-doc::coerce-parameter-type "false" "boolean"))
  (assert-equal "true" (github-api-doc::coerce-parameter-type "true" "boolean"))

  ;; from keyword
  (assert-equal 1 (github-api-doc::coerce-parameter-type 1 "integer"))
  ;; from (read-line)
  (assert-equal 12 (github-api-doc::coerce-parameter-type "12" "integer"))
  (assert-equal "" (github-api-doc::coerce-parameter-type "" "integer")) ;; empty (read-line)
  )

(define-test make-call-parameters
  (let ((api-doc (make-instance 'api-doc
                                :api "POST /user/repos"
                                :parameters '(("name" "string")
                                              ("private" "boolean")
                                              ("team_id" "integer")))))
    (assert-equal )))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :api-doc-test))
