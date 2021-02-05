(defpackage rules
  (:use #:CL)
  (:import-from uiop #:run-program)
  (:export #:show
           #:env
           #:show-env
           #:get-env
           #:shell-command
           #:shell-commands
           #:check
           #:file-exist
           
           #:*job-env-table*
           #:*job-show-log*)
  )

(in-package rules)

(defparameter *job-env-table* nil)
(defparameter *job-show-log* t)

;;;; general part
(defun show (v &key (output *job-show-log*) (format-s "~a~%"))
  (format output format-s v))

;;;; env part
(defun env (v &key (env-var *job-env-table*))
  "env-var is a hashtable
default value is job scope dynamic *job-env-table*"
  (ctypecase v
    ((cons (cons * (not cons)) *) ;; if x is list
     (loop
       for a in v
       do (setf (gethash (car a) env-var) (cdr a))))
    ((cons * (not cons)) ;; if x is single alist
     (setf (gethash (car v) env-var) (cdr v)))))

(defun get-env (v &key (env-var *job-env-table*))
  (gethash v env-var))

(defun show-env (&rest args)
  "sub-command of show of env"
  (let (syms keyws)
    (do ((rest args))
        ((not rest)
         (setf syms (reverse syms)
               keyws (reverse keyws)))
      (if (typep (car rest) 'keyword)
          (progn (push (car rest) keyws)
                 (push (cadr rest) keyws)
                 (setf rest (cddr rest))
                 )
          (progn (push (car rest) syms)
                 (setf rest (cdr rest))
                 )))
    
    (destructuring-bind
        (&key (env-var *job-env-table*)
         &allow-other-keys)
        keyws

      (if (not syms)
          (loop
            for k being the hash-keys
              using (hash-value v) of env-var
            do (show (list k v)
                     :format-s "Env value ~{~a~^ is ~}~%"))
          ;; else 
          (do ((x (car syms) (car syms)))
              ((not syms))
            (show (list x (gethash x env-var))
                  :format-s "Env value ~{~a~^ is ~}~%")
            (setf syms (cdr syms))))
      )))

;;;; commands part
(defun shell-command (&rest args)
  ;;(print args) ;; debug
  (run-program (str::join " " args) :output t))

(defun shell-commands (coms)
  (dolist (comm coms)
    (apply #'shell-command comm)))

;;;; check part
(defun check (form)
  (assert form)) ;; need error handle

;;;; others
(defun file-exist (filespec)
  (probe-file filespec))
