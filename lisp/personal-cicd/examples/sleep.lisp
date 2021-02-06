;;;; this file for testing threadpool number

(sleep 5)
(show "hello")

#|
;; how to test this file
(let ((handler (let ((j (make-job)) 
                     (*jobs-pool* (cl-threadpool:make-threadpool 1)))
                 (run j "./examples/sleep.lisp")
                 (run j "./examples/sleep.lisp"))))
  (assert (not (cl-threadpool:job-done-p handler)))
  (sleep 5)
  (assert (not (cl-threadpool:job-done-p handler)))
  (sleep 6)
  (assert (cl-threadpool:job-done-p handler))  
)
|#
