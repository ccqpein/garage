;;; some tiny file that write the sql in lisp
;;; just the translate, all s-exp just return the sql statement

(ql:quickload '("trivia" "str" "alexandria"))

(defpackage :sqlisp
  (:use :CL)
  )

(in-package #:sqlisp)

;;(setf (readtable-case *readtable*) :invert)

(defparameter *symbols-tables* ())

(defun create (noum &rest args)
  (declare (ignore args))
  (trivia:match noum
    ;; database
    ((trivia:property! :database database-name)
     (format nil "CREATE ~a" (database database-name)))

    ;; table 
    ((trivia:property! :table table-name)
     (format nil "CREATE ~a" (table table-name
                                    :columns (getf noum :columns)
                                    :as (getf noum :as)
                                    )))
    ))

(defun database (name) (format nil "DATABASE ~a" name))

(create '(:table "NewTable"
          :as
          (select ("column1" "column2") :from "ExistingTable" :where ())))

(defun table (name &key columns as)
  (cond (columns (format nil "TABLE ~a (~{~#[~:;~@{~a~^, ~}~]~});" name (table-columns columns)))
        (as (format nil "TABLE ~a AS ~{~#[~:;~@{~a~^, ~}~]~};" name '()))
        (t (error "not enough data for table"))))

(defun table-columns (table-columns)
  (let ((pk nil))
    (loop for c in table-columns
          for (cc pkk) = (multiple-value-list (column-spec c))
          ;; check the primary key
          if (and pkk pk)
            do (error "one more primary key")
          if pkk
            do (setf pk pkk)
          collect (str:join " " cc))))

(defun column-spec (column)
  (let (pk)
    (values (loop for a in column
                  collect (typecase a
                            (symbol (string a))
                            (string a)
                            (list (column-spec-type a)))
                  if (eq :primary-key a)
                    do (setf pk t))
            pk)))

(defun column-spec-type (ct)
  (ccase (first ct)
    (varchar (format nil "varchar(~a)" (second ct)))
    (int (format nil "INT(~a)" (second ct)))))
