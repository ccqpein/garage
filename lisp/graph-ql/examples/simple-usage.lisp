;;(load "../schema-generator.lisp")

;;
(defstruct-with-query-schema hero
  name
  ago
  (super-power "rich"))


(defparameter *query-request-0* "{
hero(super-power: \"rich\")
{
name(nickname: true)
ago
} 
}")

(defparameter *query-request-1* "{
hero(super-power: \"rich\")
{
name
ago
super-power
} 
}")

(defparameter *query-request-2* "{
hero(super-power: \"rich\")
{
name(nickname: false)
ago
} 
}")

(defmethod query ((s hero--name-query-schema) arguments sub-sentences &rest keys &key id &allow-other-keys)
  (destructuring-bind
	  (&key nickname &allow-other-keys)
	  arguments
	(cond ((= id 1) (if (map-graphql-value nickname) "iron man" "tony stark"))
		  ((= id 2) (if (map-graphql-value nickname) "batman" "Bruce Wayne")))
	)
  )

(defmethod query ((s hero--super-power-query-schema) arguments sub-sentences &rest keys &key id &allow-other-keys)
  "rich"
  )

;;:= maybe need to add to macro generator
(defun get-sub-sentence (name sentences)
  (find-if (lambda (s) (string= (name s) name)) sentences))

(defmethod query ((s hero-query-schema) arguments sub-sentences &key upstream-data &allow-other-keys)
  (let (result)
	(destructuring-bind
		(&key super-power &allow-other-keys)
		arguments
	  
	  (if (string= super-power "\"rich\"")
		  (progn
			(push (make-hero
				   :name (apply #'query
								(get-field-schema s "name")
								(append
								 (multiple-value-list (parse (get-field-schema s "name")
															 (get-sub-sentence "name" sub-sentences)))
								 '(:id 1)))
				   :ago 20
				   :super-power (if (get-sub-sentence "super-power" sub-sentences)
									(apply #'query
										   (get-field-schema s "super-power")
										   (multiple-value-list (parse (get-field-schema s "super-power")
																	   (get-sub-sentence "super-power" sub-sentences))))
									nil))
				  result)
			(push (make-hero
				   :name (apply #'query
								(get-field-schema s "name")
								(append
								 (multiple-value-list (parse (get-field-schema s "name")
															 (get-sub-sentence "name" sub-sentences)))
								 '(:id 2)))
				   :ago 30
				   :super-power (if (get-sub-sentence "super-power" sub-sentences)
									(apply #'query
										   (get-field-schema s "super-power")
										   (multiple-value-list (parse (get-field-schema s "super-power")
																	   (get-sub-sentence "super-power" sub-sentences))))
									nil))
				  result))
		  (return-from query nil)))
	result
	)
  )

(defparameter *ss* nil)

(let ((scanner (make-instance 'block-scanner))
	  (schema (make-instance 'hero-query-schema)))
  (scan scanner (make-string-input-stream *query-request-0*))
  (setf *ss* scanner)
  ;; return the argument and 
  (multiple-value-bind (arg sub-sentences)
	  (parse schema
			 (nth 0 (sub-sentences (car (schema-values scanner)))))
	(format t "args: ~a, sub-s: ~a~%" arg sub-sentences)
	(format t "query result: ~a~%" (query schema arg sub-sentences))
	)
  )

(let ((scanner (make-instance 'block-scanner))
	  (schema (make-instance 'hero-query-schema)))
  (scan scanner (make-string-input-stream *query-request-1*))
  (setf *ss* scanner)
  ;; return the argument and 
  (multiple-value-bind (arg sub-sentences)
	  (parse schema
			 (nth 0 (sub-sentences (car (schema-values scanner)))))
	(format t "args: ~a, sub-s: ~a~%" arg sub-sentences)
	(format t "query result: ~a~%" (query schema arg sub-sentences))
	)
  )
