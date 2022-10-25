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
  (format t "~a" (tokens bs))
  )


(defvar *case1* "{
  human(id: \"1000\") {
    name
    height(unit: FOOT)
  }
}")

(let ((bs (make-instance 'block-scanner))
	  (ss (make-string-input-stream *case1*)))
  (scan bs ss)
  (format t "~a" (tokens bs))
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
  (format t "~a" (tokens bs))
  )


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
