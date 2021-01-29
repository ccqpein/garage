(defpackage jobs
  (:use #:CL)
  (:import-from alexandria #:copy-hash-table))

(in-package jobs)

;;;; Runtime receive event, then generate a job.

;;(defparameter *job-env-table* env::make-jobenv)
(defparameter *init-env-table* (make-hash-table :test 'equal))

(defclass job ()
  ((job-id
    :initarg :id
    :reader id)
   (env
    :initarg :env
    :accessor job-env)))

(defun make-job (&key (env *init-env-table*))
  (make-instance 'job
                 :id (random 1024)
                 :env (copy-hash-table env)
                 ))

(defmethod run ((j job))
  (let ((*job-env-table* (env j)))
    (eval (with-open-file (s "demo-rules.lisp")
            (read s)))))
