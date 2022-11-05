(ql:quickload '("str" "closer-mop"))
(load "./schema-generater.lisp")
(load "./scaner.lisp")


(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream "{hero {name}}")))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )

(defvar *case0* "{
  human(id: \"1000\") {
    name
    height
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case0*)))
  (scan bs ss)
  ;;(format t "~a~%" (tokens bs))
  (format t "~a~%" (schema-values (cadr (tokens (car (tokens bs))))))
  )


(defvar *case1* "{
  human(id: \"1000\") {
    name
    height(unit: FOOT)
  }
  human2(id: \"1000\",              hhhhh : fff) {
    name
    height(unit: FOOT)
  }
}")

(defparameter *bs* nil)

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case1*)))
  (read ss) ;; clean the first {
  (scan bs ss)
  (format t "~%----------------------------------~%")
  (format t "~{~a~%~}" (tokens bs))
  (format t "~a~%" (tokens bs))
  (format t "schema-values:~%~a~%" (schema-values bs))
  ;;(format t "~a~%" (schema-values (nth 4 (tokens bs))))
  (format t "~a" (type-of (key (car (arguments (car (schema-values bs)))))))
  (format t "~a" (type-of (val (car (arguments (car (schema-values bs)))))))
  (setf *bs* bs)
  )


(defvar *case2* "{
 leftComparison: hero(episode: EMPIRE) {
    ...comparisonFields
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case2*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )

(defvar *case3* "fragment comparisonFields on Character {
  name
  appearsIn
  friends {
    name
  }
}")

(let ((bs (make-instance 'plain-scanner))
	  (ss (make-string-input-stream *case3*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )


(defvar *case4* (str:concat "{
  leftComparison: hero(episode: EMPIRE) {
    ...comparisonFields
  }
  rightComparison: hero(episode: JEDI) {
    ...comparisonFields
  }
}" *case3*))

(let ((bs (make-instance 'plain-scanner))
	  (ss (make-string-input-stream *case4*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )


(defvar *case5* "query HeroComparison($first: Int = 3) {
  leftComparison: hero(episode: EMPIRE) {
    ...comparisonFields
  }
  rightComparison: hero(episode: JEDI) {
    ...comparisonFields
  }
}

fragment comparisonFields on Character {
  name
  friendsConnection(first: $first) {
    totalCount
    edges {
      node {
        name
      }
    }
  }
}")

(let ((bs (make-instance 'plain-scanner))
	  (ss (make-string-input-stream *case5*)))
  (scan bs ss)
  (format t "~%----------------------------------~%")
  (format t "~a~%" (tokens bs))
  (format t "~%----------------------------------~%")
  (format t "~{~a~%~}" (mapcar #'schema-values
							(remove-if-not (lambda (c)
											 (c2mop:subclassp (class-of c) 'scanner)
											 )
										   (tokens bs)))))


(defvar *case6* "query HeroNameAndFriends($episode: Episode) {
  hero(episode: $episode) {
    name
    friends {
      name
    }
  }
}")

(let ((bs (make-instance 'plain-scanner))
	  (ss (make-string-input-stream *case6*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
  )

(defstruct hero
  name
  ago
  super-power)

(defstruct-with-query-schema hero
  name
  ago
  (super-power "rich"))

(let ((instance (make-instance
				 'hero-query-schema
				 :data-fetcher
				 (lambda () (make-hero :name "batman"
									   :ago 30))
				 :filter (lambda (d) (make-hero
									  :name (hero-name d)
									  :ago (hero-ago d)
									  :super-power
									  (format nil
											  "what's your super power again, ~a? \"~a\""
											  (hero-name d)
											  (hero-super-power d)))))))
  (format t "~a~%" (query instance)))


;;;;;;;;; workflow

(defstruct hero
  name
  ago
  (super-power "rich"))


(defclass hero-query-schema (query-schema)
  (
   ;; (default-data-fetcher
   ;; 	:initarg :default-data-fetcher
   ;; 	:accessor default-data-fetcher
   ;; 	:documentation "function for fetching the data")

   ;; (default-filter
   ;; 	:initarg :default-filter
   ;; 	:accessor default-filter
   ;; 	:documentation "function for filtering data after fetched")

   
   )
  )

(defmethod schema-name ((s hero-query-schema))
  "hero"
  )

(defmethod parser? ((s hero-query-schema) sentence)
  (assert (c2mop:subclassp (class-of sentence) 'struct-sentence)
		  (sentence)
		  "this schema cannot accept ~a sentence" sentence)

  (if (string/= (schema-name s) (name sentence))
	  (error 'resolver-wrong-schema :suppose-name (schema-name s)
									:actually-name (name sentence)))
  (values
   (if (arguments sentence)
	   (loop for a in (arguments sentence)
			 append (to-keys a) into args))

   ;; return the fileds
   )
  )

;; define by user themselves
(defmethod query ((s hero-query-schema) &key &allow-other-keys)
  
  )
