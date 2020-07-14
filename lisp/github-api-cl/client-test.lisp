(defpackage #:client-test
  (:use #:CL  #:lisp-unit))

(in-package #:client-test)

(define-test token-p-test
  (assert-false (github-client::token-p
                 (make-instance 'github-client::api-client)))
  
  (assert-true (github-client::token-p
                (make-instance 'github-client::api-client :token "123"))))

;;:= TODO: make fake http server and receive http-call
(define-test http-call-test
  (let (handler
        env)
    (declare (special env))
    (unwind-protect
         (progn (setf handler
                      (clack:clackup
                       (lambda (env)
                         (setf (locally (declare (special env))
                                 env)
                               env)
                         ;;(format t "~s~%" env)
                         (maphash (lambda (k v) (format t "~a: ~a~%" k v)) (car (last env)))
                         (list 200
                               '(:content-type "text/plain")
                               (list (gethash "method" (car (last env))))))
                       :server :woo))
                (assert-equal "get"
                              (github-client::http-call
                               (make-instance 'github-client::api-client :token "123")
                               "http://localhost:5000"))
                (format t "~s" env)
                )
      (clack:stop handler))))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :client-test))
