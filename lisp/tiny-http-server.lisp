(ql:quickload '("woo" "swank"))

(load "./check-if-contribute-today.lisp")

(defparameter *router-map* (make-hash-table :test 'equal))
(defparameter *argvs-map* (make-hash-table :test 'equal))

(defun register-func (url func)
  (if (symbolp func)
      (setf (gethash url *router-map*) func)
      ;; support lambda func
      (setf (gethash url *router-map*) (eval func))))

(defun register-argv (url argvs)
  (setf (gethash url *argvs-map*) argvs))

(defun register (ll)
  "((url . (func argvs*)))*"
  (loop
    for (url . func-call) in ll
    do (register-func url (car func-call))
    do (register-argv url (cdr func-call))
    ))

(defun check-contribute (&key token-path)
  (let ((out-stream (make-string-output-stream)))
    (check-contribution:if-I-commit-today-with-log
     :token-file token-path
     :out-stream out-stream)
    (list 200
          '(:content-type "text/plain")
          (list (get-output-stream-string out-stream)))))

(defparameter *app*
  (lambda (env)
    (let ((url (getf env :REQUEST-URI)))
      (if (gethash url *router-map*)
          (apply (gethash url *router-map*)
                 (gethash url *argvs-map*))
          '(404 (:content-type "text/plain") ("Not Found"))
          ))))

;; need add this token
(register '(("/" . (check-contribute :token-path ""))
            ("/hello" . ((lambda () '(200 (:content-type "text/plain") ("yoyoyo")))))))

(defun server-start ()
  (swank:create-server :port 4005  :dont-close t)
  (woo:run *app* :address "0.0.0.0" :port 9527))

(server-start)
