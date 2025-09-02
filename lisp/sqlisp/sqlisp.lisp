;;; some tiny file that write the sql in lisp
;;; just the translate, all s-exp just return the sql statement

(ql:quickload '("trivia" "str" "alexandria"))

(defpackage :sqlisp
  (:use :CL)
  )

(in-package #:sqlisp)

;;(setf (readtable-case *readtable*) :invert)

(defparameter *symbols-tables* ())

;;; create

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
    ;; index
    ((trivia:property! :index index-name)
     (format nil "CREATE ~a" (index index-name
                                    :on (getf noum :on)
                                    :columns (getf noum :columns)
                                    )))
    ))

(defun database (name) (format nil "DATABASE ~a" name))

(create '(:table "NewTable"
          :as
          (select ("column1" "column2") :from "ExistingTable" :where ())
          ))

(defun table (name &key columns as)
  (cond (columns (format nil "TABLE ~a (~{~#[~:;~@{~a~^, ~}~]~});" name (table-columns columns)))
        (as (format nil "TABLE ~a AS ~a" name (ccase (first as)
                                                 (select (apply #'select (cdr as))))))
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

(defun index (name &key on columns)
  (format nil "CREATE INDEX ~a ON ~a (~{~a~^, ~})"
          name on columns))

;;; select 

(defun select (columns &key from where)
  (format nil "SELECT ~{~a~^, ~} FROM ~a~@[ WHERE ~a~];"
          columns from
          (if where (where-condition where) nil)))

;;; where

(defun where-condition (condition)
  "condition => '(< \"aa\" 10)
=> '(and (<> \"ss\" 10) (> \"cdc\" 40))
=> '(between \"ss\" \"2023-01-01\" \"2023-01-31\")
=> '(like \"ss\" \"A%\")
=> '(in \"ss\" (\"A\" \"b\"))
=> '(null \"ss\")
=> '(not-null \"ss\")"
  (ccase (first condition)
    ((< > = != <> <= >=)
     (format nil "~a ~a ~a" (second condition) (first condition) (third condition)))
    ((and or)
     (format nil "~a ~a ~a"
             (where-condition (second condition))
             (first condition)
             (where-condition (third condition))))
    ((not)
     (format nil "NOT ~a"
             (where-condition (second condition))))
    ((like)
     (format nil "~a LIKE ~a" (second condition) (third condition)))
    ((in)
     (format nil "~a IN (~{~a~^, ~})" (second condition) (third condition)))
    (null
     (format nil "~a IS NULL" (second condition)))
    (not-null
     (format nil "~a IS NOT NULL" (second condition)))
    (between
     (format nil "~a BETWEEN '~a' AND '~a'"
             (second condition)
             (third condition)
             (fourth condition)))))
