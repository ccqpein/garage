(defpackage rules
  (:use #:CL))

(in-package rules)

;;;; general part
(defun show (v &key (output t) (format-s "~a"))
  (format output format-s v))

;;;; env part
(defun env (v &key (env-var *env-table*))
  "env-var is a hashtable
default value is job scope dynamic *env-table*"
  (ctypecase v
    ((cons (cons * (not cons)) *) ;; if x is list
     (loop
       for a in v
       do (setf (gethash (car a) env-var) (cdr a))))
    ((cons * (not cons)) ;; if x is single alist
     (setf (gethash (car v) env-var) (cdr v)))))

(defun show-env (&rest syms)
  (destructuring-bind
      (&key (env-var *env-table* env-var-p)
       &allow-other-keys)
      syms
    
    (if (or (and env-var-p (= 2 (length syms)))
            (not syms))
        ;; if there are no symbols or only have env-var keywords
        ;; print all env var
        (loop
          for k being the hash-keys
            using (hash-value v) of env-var
          do (show (list k v)
                   :format-s "Env value ~{~a~^ is ~}~%"))
        ;; else 
        (do ((x (car syms) (car syms)))
            ((not syms))
          (if (equal x :env-var)
              (setf syms (cdr syms))
              (show (list x (gethash x env-var))
                    :format-s "Env value ~{~a~^ is ~}~%"))
          (setf syms (cdr syms))))))

;;;; commands part


;;;; check part
