(load "../schema-generator.lisp")

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

(defmethod query ((s hero--name-query-schema) id arguments sub-sentences &allow-other-keys)
  
  )

;;:= maybe need to add to macro generator
(defun get-sub-sentence (name sentences)
  (find-if (lambda (s) (string= (name s) name)) sentence))

(defmethod query ((s hero-query-schema) arguments sub-sentences &key upstream-data &allow-other-keys)
  (let (result)
	(destructuring-bind
		(&key super-power &allow-other-keys)
		arguments
	  (if (string= super-power "rich")
		  (progn (push (make-hero
						:name (apply #'query
									 (field-query s :name "name") 1
									 (parse (field-query s :name "name") (get-sub-sentence "name" sub-sentences)))
						:ago 20
						:super-power (get-sub-sentence "super-power" sub-sentences))
					   result)
				 (push (make-hero
						:name (apply #'query
									 (field-query s :name "name") 2
									 (parse (field-query s :name "name") (get-sub-sentence "name" sub-sentences)))
						:ago 30
						:super-power (get-sub-sentence "super-power" sub-sentences)))
				 result))
	  (return-from query nil))
	result
	)
  )

