(ql:quickload '("woo"))

(declaim (optimize (speed 3) (safety 0)))

(defun server-start ()
  (woo:run
   (lambda (env)
     (declare (ignore env))
     '(200 (:content-length 0) ("")))
   :address "127.0.0.1" :port 3001
   :worker-num 4))

