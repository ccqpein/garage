(defclass query-schema ()
  (
   (fields
	:initarg :fields
	:accessor fields)
   )
  (:documentation "the root query schema class"))

(defclass mutation-schema ()
  (
   (fields
	:initarg :fields
	:accessor fields)
   )
  (:documentation "the root mutation schema class")
  )

(defmacro defstruct-with-query-schema (name-and-options &rest slot-descriptions)
  "defstruct and generate the query schema"
  (let (name fields)
	(if (consp name-and-options)
		(setf name (car name-and-options))
		(setf name name-and-options))

	(loop for f in slot-descriptions
		  if (consp f)
			do (push (car f) fields)
		  else
			do (push f fields))
	;;:= DEL: (print (type-of (string (car fields))))
	(print fields)
	(let ((schema-class-name (read-from-string (str:concat (symbol-name name) "-QUERY-SCHEMA"))))
	  `(progn
		 (defstruct ,name-and-options ,@slot-descriptions)
	   
		 (defclass ,schema-class-name (query-schema)
		   (
			(all-fields-names
			 :initarg :all-fields-names
			 :initform '(,@(mapcar #'string fields))
			 :accessor all-fields-names)

			(fields-schemas
			 :initarg :fields-schemas
			 :initform (let ((table (make-hash-table :test 'equal)))
						 ,@(loop
							 for f in fields
							 collect `(setf (gethash ,(string f) table)
											(make-instance ',(read-from-string
															  (str:concat (symbol-name name)
																		  "--" (symbol-name f)
																		  "-query-schema")))))
						 table)
			 :accessor fields-schemas))
		   (:documentation ,(format nil "autogenerated query-schema for ~a" name)))

		 (defmethod schema-name ((s ,schema-class-name))
		   ,(string name))

		 ,@(loop for f in fields
				 for class-name = (read-from-string (str:concat (symbol-name name) "--" (symbol-name f) "-query-schema"))
				 collect `(defclass ,class-name (query-schema) ()) into x
				 collect `(defmethod schema-name ((s ,class-name)) ,(string f)) into x
				 finally (return x))

		 
		 ))))


(defmacro generate-mutation-schema ()
  "generate the mutation schema for structure"
  ;;:= todo
  )

(define-condition resolver-wrong-schema (error)
  ((suppose-name :initarg :suppose-name :accessor suppose-name)
   (actually-name :initarg :actually-name :accessor actually-name))
  (:report
   (lambda (condition stream)
	 (format stream "suppose resolve the ~a schema, but receive ~a"
			 (suppose-name condition) (actually-name condition)))))

;; (define-condition is-not-struct (error)
;;   ((incorrect-sym :initarg :incorrect-sym
;; 				  :initform nil
;; 				  :accessor incorrect-sym))
;;   (:report (lambda (condition stream)
;; 			 (format stream "the symbol '~a isn't a structure" (incorrect-sym condition)))))

;; (defun check-if-symbol-is-struct (sym)
;;   (if (not (subtypep sym 'structure-object))
;; 	  (error 'is-not-struct :incorrect-sym sym)
;; 	  t))
