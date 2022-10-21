(defclass query-schema ()
  (
   (fields :accessor fields)
   )
  (:documentation "the root query schema class"))

(defclass mutation-schema ()
  (
   fields
   )
  (:documentation "the root mutation schema class")
  )

(defmacro generate-query-schema (s)
  "generate the query schema for structure"
  `(progn
	 (print ,s)
	 (check-if-symbol-is-struct ,s))
  )

(defmacro generate-mutation-schema ()
  "generate the mutation schema for structure"
  ;;:= todo
  )

(define-condition is-not-struct (error)
  ((incorrect-sym :initarg :incorrect-sym
				  :initform nil
				  :accessor incorrect-sym))
  (:report (lambda (condition stream)
			 (format stream "the symbol '~a isn't a structure" (incorrect-sym condition)))))

(defun check-if-symbol-is-struct (sym)
  (if (not (subtypep sym 'structure-object))
	  (error 'is-not-struct :incorrect-sym sym)
	  t))
