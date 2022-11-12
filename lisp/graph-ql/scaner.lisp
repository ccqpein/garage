(ql:quickload '("closer-mop" "str"))

(defclass scanner () ())

#|
block-scanner class below
|#

(defclass block-scanner (scanner)
  (
   (tokens
	:initform nil
	:accessor tokens)
   )
  (:documentation "scanner for {} block"))

(defmethod print-object ((bs block-scanner) stream)
  (format stream "{block: tokens: ~{~a~^, ~}}" (tokens bs)))

(defmethod scan ((s block-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((or (not c) (char= c #\}))
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token)))))))
	(case c
	  (#\{ (setf (tokens s)
				 (append (tokens s)
						 (list (let ((sub-block-scanner (make-instance 'block-scanner)))
								 (scan sub-block-scanner stream)
								 sub-block-scanner)))))
	  
	  (#\( (setf (tokens s)
				 (append (tokens s)
						 (list
						  (concatenate 'string (reverse word-token))
						  (let ((sub-block-scanner (make-instance 'parenthesis-scanner)))
							(scan sub-block-scanner stream)
							sub-block-scanner)))
				 word-token
				 nil))
	  
	  ((#\  #\, #\newline #\#) ;; ignore tokens 
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))
				 word-token
				 nil)
		   ))
	  ((#\. #\:) ;; : is special
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))
										  c))
				 word-token nil)
		   (setf (tokens s) (append (tokens s) (list c)))))
	  (otherwise (push c word-token))
	  )))

(defmethod schema-values ((s block-scanner))
  "scanner pre-processed tokens return the result for schema resolver"
  (do* ((tokens (tokens s) (cdr tokens))
		(this-token (car tokens) (car tokens))
		cache-sentence
		result
		last-colon
		)
	   
	   ((not tokens)
		(if cache-sentence (push cache-sentence result))
		;;:= DEL: (format t "final: ~a~%" result)
		(reverse result))

	;;:= DEL: (format t "~a~%" (class-of this-token))
	;;:= DEL: (format t "~a~%" this-token)
	;;:= DEL: (format t "~a~%" cache-sentence)
	(ctypecase this-token
	  (string
	   (if (not last-colon)
		   (if cache-sentence
			   (progn (push cache-sentence result)
					  (setf cache-sentence (make-instance 'struct-sentence :name this-token)))
			   (setf cache-sentence (make-instance 'struct-sentence :name this-token)))
		   ))
	  (scanner
	   (if (not cache-sentence) ;; when only block body
		   (setf cache-sentence (make-instance 'struct-sentence)))
	   (if (c2mop:subclassp (class-of this-token) 'parenthesis-scanner)
		   (setf (arguments cache-sentence) (schema-values this-token))
		   (setf (sub-sentences cache-sentence) (schema-values this-token))))
	  )	
	))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parenthesis-scanner (scanner)
  ((tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of () block")
  )

(defmethod print-object ((ps parenthesis-scanner) stream)
  (format stream "{parenthesis block: tokens: ~{~a~^, ~}}" (tokens ps)))

(defmethod scan ((s parenthesis-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((or (not c) (char= c #\)))
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token)))))))
	(case c
	  (#\ 
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))))
				 word-token nil)))
	  (#\:
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))
										  #\:))
				 word-token nil)
		   (setf (tokens s) (append (tokens s) '(#\:)))))
	  (#\,
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))
										  #\,))
				 word-token nil)
		   (setf (tokens s) (append (tokens s) '(#\,)))))
	  (otherwise (push c word-token))
	  )
	)
  )


(defmethod schema-values ((s parenthesis-scanner))
  "scanner pre-processed tokens return the result for schema resolver"
  (do* ((tokens (tokens s))
	   (this-token (car tokens) (car tokens))
	   cache-sentence
	   result)
	   ((not tokens)
		(if cache-sentence (push cache-sentence result))
		(reverse result))
	;;(format t "~a~%" tokens)
	;;(format t "~a~%" this-token)
	(ctypecase this-token
	  (string
	   (if (not cache-sentence)
		   (setf cache-sentence (make-instance 'arguments-sentence :key this-token))
		   (progn (push cache-sentence result)
				  (setf cache-sentence (make-instance 'arguments-sentence :key this-token))))
	   (setf tokens (cdr tokens)))
	  (STANDARD-CHAR
	   (ccase this-token
		 (#\:
		  (setf (val cache-sentence) (cadr tokens)
				tokens (cdr tokens))
		  )
		 (#\,
		  (push cache-sentence result)
		  (setf cache-sentence nil)))
	   (setf tokens (cdr tokens)))))
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plain-scanner (scanner)
  ((tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of plain text (like fragment)")
  )

(defmethod scan ((s plain-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((not c)
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token)))))))
	(case c
	  (#\{ (setf (tokens s)
				 (append (tokens s)
						 (list (let ((sub-block-scanner (make-instance 'block-scanner)))
								 (scan sub-block-scanner stream)
								 sub-block-scanner)))))

	  (#\( (setf (tokens s)
				 (append (tokens s)
						 (list
						  (concatenate 'string (reverse word-token))
						  (let ((sub-block-scanner (make-instance 'parenthesis-scanner)))
							(scan sub-block-scanner stream)
							sub-block-scanner)))
				 word-token
				 nil))
	  
	  ((#\  #\, #\newline #\#) ;; ignore tokens 
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))
				 word-token
				 nil)
		   ))

	  (otherwise (push c word-token)))))

(defmethod print-object ((ps plain-scanner) stream)
  (format stream "{plain block: tokens: ~{~a~^, ~}}" (tokens ps)))

(defmethod schema-values ((s plain-scanner))
  ;;:= todo
  )

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass sentence ()
  ())

(defclass struct-sentence (sentence)
  ((name
	:initarg :name
	:initform nil
	:accessor name)
   (arguments ;; parenthesis schema values
	:initarg :arguments
	:initform nil
	:accessor arguments) 
   (sub-sentences ;; sub block sentence
	:initarg :sub-sentences
	:initform nil
	:accessor sub-sentences))
  )

(defmethod print-object ((ss struct-sentence) stream)
  (format stream "{name: ~a, arguments: ~a, sub-sentences: ~a}" (name ss) (arguments ss) (sub-sentences ss)))
  
(defclass arguments-sentence (sentence)
  ((key
	:initarg :key
	:accessor key)
   (val
	:initarg :val
	:accessor val))
  )

(defmethod print-object ((as arguments-sentence) stream)
  (format stream "{key: ~a, val: ~a}" (key as) (val as)))

(defmethod to-keys ((as arguments-sentence))
  (list (read-from-string (str:concat ":" (key as)))
		(val as))
  )
