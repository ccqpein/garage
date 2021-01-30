(defpackage jobs
  (:use #:CL #:rules)
  (:import-from alexandria #:copy-hash-table))

(in-package jobs)

;;;; Runtime receive event, then generate a job.

;;(defparameter *job-env-table* env::make-jobenv)
(defparameter *init-env-table* (make-hash-table :test 'equal))

(defclass job ()
  ((job-id
    :initarg :id
    :reader job-id)
   (env
    :initarg :env
    :accessor job-env)))

(defun make-job (&key (env *init-env-table*))
  (make-instance 'job
                 :id (random 1024)
                 :env (copy-hash-table env)
                 ))

(defmethod run ((j job))
  (let ((*job-env-table* (job-env j)) ;;:= problem here
        (*job-id* (job-id j)))
    ;; (env '((current-workplace . "./")
    ;;        (build-number . 123)))
    ;; (show-env 'current-workplace)
    (declare (special *job-env-table* *job-id*))
    (with-open-file (s "demo-rules.lisp")
      (loop
        for expr = (read s nil)
        if expr
          do (eval expr)
        else
          return nil))
    ))
