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
        env
        (clt (make-instance 'github-client::api-client
                            :token "123")))
    (unwind-protect
         (progn (setf handler
                      (clack:clackup
                       (lambda (request)
                         (setf env request) ;; update env
                         (list 200
                               '(:content-type "text/plain")
                               (list (gethash "authorization" (car (last env))))))
                       :server :woo))

                ;; default method is :GET
                (assert-equal :GET
                              (progn (github-client::http-call clt "http://localhost:5000")
                                     (alexandria:doplist (k v env)
                                       (if (eq k :REQUEST-METHOD)
                                           (return v)))))

                ;; give some method
                (assert-equal :DELETE
                              (progn (github-client::http-call clt
                                                               "http://localhost:5000"
                                                               :method "delete")
                                     (alexandria:doplist (k v env)
                                       (if (eq k :REQUEST-METHOD)
                                           (return v)))))

                ;;:= TEST: give token

                ;;:= TEST: don't give token, use client token

                ;;:= TEST: give username and passd

                ;;(format t "~s" env)
                (clack:stop handler)))))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests :all :client-test))
