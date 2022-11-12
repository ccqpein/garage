;;(load "../schema-generator.lisp")

;;
(defstruct-with-query-schema hero
  name
  ago
  (super-power "rich"))


(defparameter *query-request-0* "{
hero(super-powert: \"rich\")
{
name(nickname: true)
ago
} 
}")

(defmethod query ((s hero--name-query-schema) arguments sub-sentences &key id &allow-other-keys)
  (destructuring-bind
	  (&key nickname &allow-other-keys)
	  arguments
	(cond ((= id 1) (if nickname "iron man" "tony stark"))
		  ((= id 2) (if nickname "batman" "Bruce Wayne")))
	)
  )

;;:= maybe need to add to macro generator
(defun get-sub-sentence (name sentences)
  (find-if (lambda (s) (string= (name s) name)) sentences))

(defmethod query ((s hero-query-schema) arguments sub-sentences &key upstream-data &allow-other-keys)
  (let (result)
	;;:= maybe: add destructuring-bind to macro generator too
	(destructuring-bind
		(&key super-power &allow-other-keys)
		arguments
	  (if (string= super-power "rich")
		  (progn (push (make-hero
						:name (apply #'query
									 (get-field-schema s "name") ;;:= todo: change to get field schema method
									 ;;:= find sub sentence maybe can added to macro generator
									 (parse (field-query s :name "name") (get-sub-sentence "name" sub-sentences))
									 :id 1)
						:ago 20
						:super-power (get-sub-sentence "super-power" sub-sentences))
					   result)
				 (push (make-hero
						:name (apply #'query
									 (get-field-schema s "name")
									 (parse (field-query s :name "name") (get-sub-sentence "name" sub-sentences))
									 :id 2)
						:ago 30
						:super-power (get-sub-sentence "super-power" sub-sentences))
					   result))
		  (return-from query nil)))
	result
	)
  )


(defparameter *ss* nil)
(let ((scanner (make-instance 'block-scanner)))
  (scan scanner (make-string-input-stream *query-request-0*))
  (setf *ss* scanner)
  (schema-values scanner)
  )
