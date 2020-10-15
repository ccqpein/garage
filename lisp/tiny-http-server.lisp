(ql:quickload :woo)

(defun check-contribute (&key token-path)
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program "./check-if-contribute-today.ros"
                        (list token-path)
                        :search t
                        :output out)
    (list 200
          '(:content-type "text/plain")
          (list (get-output-stream-string out)))))

(defun make-handler (&rest url-handles)
  (let* ((table (make-hash-table :test 'equal))
         (argv-table (make-hash-table :test 'equal)))
    (loop
      for (url handle argvs) in url-handles
      do (setf (gethash url table) handle
               (gethash url argv-table) argvs))
    (lambda (env)
      (let ((url (getf env :REQUEST-URI)))
        (apply (gethash url table)
               (gethash url argv-table))))))

(defparameter *app* (make-handler '("/" check-contribute (:token-path "")))) ;; need add this token

(defun server-start ()
  (woo:run *app*))

(server-start)
