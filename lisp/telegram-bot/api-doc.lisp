(defpackage #:tele-api-doc
  (:use #:CL))

(in-package #:tele-api-doc)

(defstruct data-fields-pairs
  "data fields pairs"
  field
  type
  description)

(defstruct api-datatype
  (name "" :type string)
  (fields nil :type (cons data-fields-pairs))
  (doc "" :type string)
  )

(defstruct method-fields-pairs
  "method fields pairs"
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
  "generate api datatype from hashtable of json"
  (declare (hash-table table))
  (let ((types (gethash "types" table))
        (fields (gethash "fields" table))
        (descriptions (gethash "descriptions" table))
        )
    (make-api-datatype
     :name
     (gethash "name" table)
     :fields (loop
               for f in fields
               for ty in types
               for des in descriptions
               collect (make-data-fields-pairs
                        :field f
                        :type ty
                        :description des))
     :doc (gethash "doc" table))))

(defun make-api-method-from (table)
  "generate api method from hashtable of json"
  (declare (hash-table table))
  (let ((types (gethash "types" table))
        (parameters (gethash "parameters" table))
        (descriptions (gethash "descriptions" table))
        (requireds (gethash "requireds" table)))
    (make-api-method
     :name (gethash "name" table)
     :fields (loop
               for pa in parameters
               for ty in types 
               for des in descriptions
               for req in requireds 
               collect (make-method-fields-pairs
                        :parameter pa
                        :required req
                        :type ty 
                        :description des))
     :doc (gethash "doc" table))))

(defun if-exist-or-ask (ff)
  "help function of file reading"
  (let ((re (probe-file ff)))
    (if re
        re
        (progn (format t "cannot find file ~s, input the path of ~s:~%" ff ff)
               (read-line)))))

;; (loop
;;   with re = '()
;;   for m in *all-methods*
;;   do (loop
;;        for field in (api-method-fields m)
;;        do (pushnew (method-fields-pairs-type field) re :test #'string=))
;;   finally (return re))

;; (loop
;;   with re = '()
;;   for m in *all-data-types*
;;   do (loop
;;        for field in (api-datatype-fields m)
;;        do (pushnew (data-fields-pairs-type field) re :test #'string=))
;;   finally (return re))

;; ;;;:= TODO: finish this function
;; (defun type-maker (str)
;;   (let ((sl (str:split " " str)))
;;     (ccase (length sl)
;;       (1 ) ;; like String
;;       (2 ) ;; like "Float number"
;;       (3 ) ;; like "Array of String"
;;       )))

(defun refresh-api-datatypes (&key (datatypes "./datatype.json"))
  "return new api datatypes list from json file (default is ./datatype.json)"
  (the (cons api-datatype)
       (let ((path (if-exist-or-ask datatypes)))
         (loop for dd in (read-api-doc path)
               collect (make-api-datatype-from dd)))))

(defun refresh-api-methods (&key (methods "./methods.json"))
  "return new api method list from json file (default is ./methods.json)"
  (the (cons api-method)
       (let ((path (if-exist-or-ask methods)))
         (loop for dd in (read-api-doc path)
               collect (make-api-method-from dd)))))

(defun refresh-api-docs (&key (datatypes "./datatype.json") (methods "./methods.json"))
  "call refresh-api-datatypes and refresh-api-methods, return values of them"
  (values (refresh-api-datatypes :datatypes datatypes)
          (refresh-api-methods :methods methods)))

;;;;
;;;; below are making static code instead of parse json everytime
;;;;

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

;;;; 
;;;; global api doc
;;;;

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

(defun get-datatype (name)
  (gethash name *all-data-types-table*))

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

(defun get-method (name)
  (gethash name *all-methods-table*))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun api-keys (api)
  "return datatypes' or methods' fields pairs detail"
  (ctypecase api
    (api-datatype (loop
                    for d in (api-datatype-fields api)
                    collect (list (intern (string-upcase
                                           (data-fields-pairs-field d))
                                          "KEYWORD")
                                  (data-fields-pairs-type d))))
    (api-method (loop
                  for d in (api-method-fields api)
                  collect (list (intern (string-upcase
                                         (method-fields-pairs-parameter d))
                                        "KEYWORD")
                                (method-fields-pairs-type d)
                                (method-fields-pairs-required d))))))

;;; return like this:
;;; (((:OFFSET "Integer" "Optional") 2) ((:LIMIT "Integer" "Optional") 1))
(defun api-keywords-parser (api args)
  "parse args to this api's keypairs"
  (assert (evenp (length args)))
  (loop
    with kps = (api-keys api)
    and result = '()
    
    for k in kps
    for v = (getf args (car k)) ;; if this field is given in args
    do  (cond ((and (not v)     ;; v is nil
                    (= 3 (length k))              ;; it is method
                    (string= (third k) "Yes")) ;; and it is required
               (error "Required argument ~S cannot found" (car k)))
              (v (push (list k v) result)))
    finally (return (reverse result))))

