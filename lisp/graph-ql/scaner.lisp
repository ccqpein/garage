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
	  (otherwise (push c word-token))
	  )
	)
  )

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
