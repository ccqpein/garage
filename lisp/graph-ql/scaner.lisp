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

;; return list of list of each fields of block
(defmethod schema-values ((s block-scanner))
  "scanner pre-processed tokens return the result for schema resolver"
  (do* ((tokens (tokens s) (cdr tokens))
		(this-token (car tokens) (car tokens))
		cache
		result
		last-colon)
	   ((not tokens)
		(if cache (push (reverse cache) result))
		(reverse result))
	
	(ctypecase this-token
	  (string
	   (if (not last-colon)
		   (if cache
			   (progn (push (reverse cache) result)
					  (setf cache nil)))
		   (setf last-colon nil))
	   (push this-token cache)
	   )
	  (scanner (push this-token cache))
	  (STANDARD-CHAR
	   (ccase this-token
		 (#\:
		  (push #\: cache)
		  (setf last-colon t) ;; colon make next token escaped
		  )
		 )))
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
  (do* ((tokens (tokens s) (cdr tokens))
		(this-token (car tokens) (car tokens))
		cache
		result)
	   ((not tokens)
		(if cache (push (reverse cache) result))
		(reverse result))
	
	(ctypecase this-token
	  ((or scanner string) 
	   (push this-token cache)
	   )
	  (STANDARD-CHAR
	   (ccase this-token
		 (#\:
		  ;; pass
		  )
		 (#\,
		  (if cache (push (reverse cache) result))
		  (setf cache nil)))))
	))
  
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

(defstruct sentence
  name
  arguments ;; parenthesis schema values
  sub-sentences ;; sub block sentence
  )
