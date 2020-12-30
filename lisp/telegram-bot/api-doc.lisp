(defpackage #:tele-api-doc
  (:use #:CL))

(in-package #:tele-api-doc)

(ql:quickload "yason")

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
  (declare (type (or string pathname) filepath))
  (setf filepath (if (stringp filepath)
                     (pathname filepath)
                     filepath))
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
               (pathname (read-line))))))

(defun refresh-api-datatypes (&key (datatypes "./datatype.json"))
  (let ((path (if-exist-or-ask datatypes)))
    (loop for dd in (read-api-doc path)
          collect (make-api-datatype-from dd))))

(defun refresh-api-methods (&key (methods "./methods.json"))
  (let ((path (if-exist-or-ask methods)))
    (loop for dd in (read-api-doc path)
          collect (make-api-method-from dd))))

(defun refresh-api-docs (&key (datatypes "./datatype.json") (methods "./methods.json"))
  (refresh-api-datatypes :datatypes datatypes)
  (refresh-api-methods :methods methods))

(defparameter *all-data-types* (refresh-api-datatypes))

(defparameter *all-methods* (refresh-api-methods))

;;;:= TODO: generate static lisp code,...
;;;:= should ask at the first time and load automaticly after that,...
;;;:= also can update
