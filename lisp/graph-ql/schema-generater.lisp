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

;; (defmacro generate-query-schema (s)
;;   "generate the query schema for structure"
;;   `(progn
;; 	 (check-if-symbol-is-struct ,s)
	 
;; 	 )
;;   )

(defmacro defstruct-with-query-schema (name-and-options &rest slot-descriptions)
  ;;(format t "~a ~{~a~}" name-and-options slot-descriptions)
  (let (name fields)
	(if (consp name-and-options)
		(setf name (car name-and-options))
		(setf name name-and-options))

	(loop for f in slot-descriptions
		  if (consp f)
			do (push (car f) fields)
		  else
			do (push f fields))
	
	(format t "~a ~{~a,~}" name fields)

	`(progn
	   (defstruct ,name-and-options ,@slot-descriptions)

	   ;;:= todo: make schema here
	   )
	)
  
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
