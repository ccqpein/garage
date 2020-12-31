(defpackage #:tele-api-doc
  (:use #:CL))

(in-package #:tele-api-doc)

(defstruct data-fields-pairs
  field
  type
  description)

(defstruct api-datatype
  (name "" :type string)
  (fields nil :type (cons data-fields-pairs))
  (doc "" :type string)
  )

(defstruct method-fields-pairs
  parameter
  type
  required
  description)

(defstruct api-method
  (name "" :type string)
  (fields nil :type (cons method-fields-pairs))
  (doc "" :type string)
  )

;;; datatype.json and methods.json
(defun read-api-doc (filepath)
  "read api doc"
  (with-open-file (s filepath)
    (let ((data (make-string (file-length s))))
      (read-sequence data s)
      (yason:parse data))))

(defun make-api-datatype-from (table)
  (declare (hash-table table))
  (let ((types (gethash "types" table))
        (fields (gethash "fields" table))
        (descriptions (gethash "descriptions" table)))
    (make-api-datatype :name (gethash "name" table)
                       :fields (loop
                                 for f in fields
                                 for ty in types ;;:= MAYBE: maybe need to clean types
                                 for des in descriptions
                                 collect (make-data-fields-pairs :field f
                                                                 :type ty
                                                                 :description des))
                       :doc (gethash "doc" table))))

(defun make-api-method-from (table)
  (declare (hash-table table))
  (let ((types (gethash "types" table))
        (parameters (gethash "parameters" table))
        (descriptions (gethash "descriptions" table))
        (requireds (gethash "requireds" table)))
    (make-api-method :name (gethash "name" table)
                     :fields (loop
                               for pa in parameters
                               for ty in types ;;:= MAYBE: maybe need to clean types
                               for des in descriptions ;;:= MAYBE: make it clean
                               for req in requireds 
                               collect (make-method-fields-pairs :parameter pa
                                                                 :required req
                                                                 :type ty
                                                                 :description des))
                     :doc (gethash "doc" table))))

(defun if-exist-or-ask (ff)
  (let ((re (probe-file ff)))
    (if re
        re
        (progn (format t "cannot find file ~s, input the path of ~s:~%" ff ff)
               (read-line)))))

(defun refresh-api-datatypes (&key (datatypes "./datatype.json"))
  (let ((path (if-exist-or-ask datatypes)))
    (loop for dd in (read-api-doc path)
          collect (make-api-datatype-from dd))))

(defun refresh-api-methods (&key (methods "./methods.json"))
  (let ((path (if-exist-or-ask methods)))
    (loop for dd in (read-api-doc path)
          collect (make-api-method-from dd))))

(defun refresh-api-docs (&key (datatypes "./datatype.json") (methods "./methods.json"))
  (values (refresh-api-datatypes :datatypes datatypes)
          (refresh-api-methods :methods methods)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *source-directory*
    (directory-namestring (asdf:system-source-directory (asdf:find-system :telegram-bot)))))

(defvar *static-api-doc-path* (format nil "~a~a" *source-directory* "api-doc-static.lisp"))
(defvar *static-ignore-path* (format nil "~a~a" *source-directory* "__ignore-api-doc-static.lisp"))

(defparameter *has-static-code*
  (when (and (probe-file *static-api-doc-path*)
             (not (probe-file *static-ignore-path*)))
    (probe-file *static-api-doc-path*))) 

;;; have to have these two nil value for loading static datatype/methods
(defparameter *all-data-types-static* nil)
(defparameter *all-methods-static* nil)

;;; load here
(if *has-static-code* (load *has-static-code*))

;;; define global datatypes and methods
(defparameter *all-data-types*
  (if *has-static-code*
      (progn
        (format t "Load static code from ~a~%" *has-static-code*)
        *all-data-types-static*)
      (refresh-api-datatypes)))

(defparameter *all-data-types-table*
  (let ((table (make-hash-table :test 'equal)))
    (loop for d in *all-data-types*
          do (setf (gethash (api-datatype-name d) table) d))
    table))

(defparameter *all-methods*
  (if *has-static-code*
      (progn
        (format t "Load static code from ~a~%" *has-static-code*)
        *all-methods-static*)
      (refresh-api-methods)))

(defparameter *all-methods-table*
  (let ((table (make-hash-table :test 'equal)))
    (loop for d in *all-methods*
          do (setf (gethash (api-method-name d) table) d))
    table))

(defun generate-static-api-doc-code ()
  (let ((filepath *static-api-doc-path*)
        )
    
    (when (probe-file *static-ignore-path*)
      (format t "find ignore file placeholder, delete it")
      (delete-file *static-ignore-path*))
    
    (with-open-file (s filepath :direction :output :if-exists :supersede)
      (format t "Generating static code file of datatypes and methods in ~s~%" filepath)
      (loop
        initially (format s "(in-package #:tele-api-doc) ~%(defparameter *all-data-types-static* '(")
        for d in *all-data-types*
        do (format s "~s ~%" d)
        finally (format s "))~%"))
      
      (loop
        initially (format s "(defparameter *all-methods-static* '(")
        for d in *all-methods*
        do (format s "~s ~%" d)
        finally (format s "))")))))

;;; the last ask if wanna create static code
(progn
  (if (and (not (probe-file *static-api-doc-path*))
           (not (probe-file *static-ignore-path*)))
      (if (yes-or-no-p "Wanna create static code for api doc? (instead of parsing from json everytime)")
             (generate-static-api-doc-code)
             (if (yes-or-no-p "Ignore this message in future? (you can run (generate-static-api-doc-code) by yourself all the time)")
                 (with-open-file (s *static-ignore-path* :direction :output)
                   (format s "this file is placeholder for ignoring static code generater"))))))


(defun get-method (name)
  (gethash name *all-methods-table*))

(defun get-datatype (name)
  (gethash name *all-data-types-table*))
