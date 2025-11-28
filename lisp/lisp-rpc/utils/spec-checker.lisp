(ql:quickload '("str" "alexandria"))

(defpackage lisp-rpc-checker
  (:use #:cl))

(in-package :lisp-rpc-checker)

(defparameter *example* "(def-msg language-perfer :lang 'string)

  (def-msg book-info
    :lang 'language-perfer
    :title 'string
    :version 'string
    :id 'string)

  (def-rpc get-book
      '(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))
    'book-info)

  (def-msg language-perfers :langs '(list 'string))")

(defparameter *checker-map*
  (mapcar #'cons
       '("def-msg" "def-rpc")
       '(def-msg-checker def-rpc-checker)))

(defun spec-check-one (expr)
  "get one expr, check it roughly and try to eval real checker it"
  (if (< (length expr) 2)
      (error "spec expr at least have two elements inside"))
  (let ((x (first expr))        
        checker)
    (loop for (sx . c) in *checker-map*
          when (or (string= x sx) (string= x (str:upcase sx)))
            do (setf checker c)
            and return nil
          finally
             (error (format nil
                            "spec expr only support the ~{~a~^, ~}"
                            (mapcar #'car *checker-map*))))
    (apply checker (cdr expr))))

(defun def-msg-checker (name &rest args)
  (declare (ignore name))
  (if (zerop (length args))
      ;; it can be the empty definations
      t
      ;; the rest should be some map data format
      (map-data-type-checker args)))

(defun type-checker (ty)
  "check the type defination"
  (ctypecase ty
    (symbol (unless (not ty) t))
    (cons (or
           ;; for ''string, because quoted type inside
           (if (equal 'quote (first ty))
               (type-checker (second ty)))
           (map-data-type-checker ty)
           (apply #'list-type-checker ty)))))

(defun map-data-type-checker (eles)
  "check map data format. 
can be used to check the data and the type defination"
  (if (zerop (length eles)) (return-from map-data-type-checker nil))
  (loop for (k v) on eles by #'cddr
        unless (and (typep k 'keyword)
                    (type-checker v))
          return nil
        finally (return t)))

(defun list-type-checker (&rest args)
  "check list type *defination*. 
list type defination should be '(list 'other-type)"
  (if (/= (length args) 2) (return-from list-type-checker nil))
  (and (eq (first args) 'list)
       (type-checker (second args))))

(defun list-data-type-checker (eles)
  "this one check the list data. list type defination should use the list-type-checker"
  (every (lambda (e) (eq (type-of (first eles))
                         (type-of e)))
         eles))

(defun def-rpc-checker (name &rest args)
  (declare (ignore name))
  (if (zerop (length args))
      ;; it can be the empty definations
      t
      ;; the rest should be some map data format
      (map-data-type-checker args)))
