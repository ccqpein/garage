(ql:quickload :woo)
(load "./check-if-contribute-today.fasl")

(defun check-contribute (&key token-path)
  (let ((out-stream (make-string-output-stream)))
    (check-contribution:if-I-commit-today-with-log
     :token-file token-path
     :out-stream out-stream)
    (list 200
          '(:content-type "text/plain")
          (list (get-output-stream-string out-stream)))))

(defun make-handler (&rest url-handles)
  (let* ((table (make-hash-table :test 'equal))
         (argv-table (make-hash-table :test 'equal)))
    (loop
      for (url handle argvs) in url-handles
      do (setf (gethash url table) handle
               (gethash url argv-table) argvs))
    (lambda (env)
      (let ((url (getf env :REQUEST-URI)))
        (if (gethash url table)
            (apply (gethash url table)
                   (gethash url argv-table))
            '(404 (:content-type "text/plain") ("Not Found"))
            )))))

(defparameter *app* (make-handler '("/" check-contribute (:token-path "")))) ;; need add this token

(defun server-start ()
  (woo:run *app* :address "0.0.0.0" :port 9527))

(server-start)
