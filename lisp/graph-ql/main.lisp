;; (defun check-line-terminators (s)
;;   "check line terminator \\n and \\t"
;;   (declare (stream s) )
  
;;   )

(deftype status ()
  '(member :in-the-comment
	))

(defclass scanner ()
  ()
  (:documentation "adam scanner class"))

(defgeneric clear (obj)
  (:documentation "clear all the content of this scanner"))

(defclass block-scanner (scanner)
  (
   ;;(sub-scanners :accessor sub-scanners)
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
	(ccase c
	  (#\{ (setf (tokens s)
				 (append (tokens s)
						 (list (let ((sub-block-scanner (make-instance 'block-scanner)))
								 (scan sub-block-scanner stream)
								 sub-block-scanner)))))
	  (#\(  ) ;;:= todo parenthesis-scanner
	  ((#\  #\, #\newline #\#) ;; ignore tokens 
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))))
				 word-token nil)
		   ))
	  (otherwise (push c word-token))
	  )))

(defclass parenthesis-scanner (scanner)
  ((tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of () block")
  )

(defmethod scan ((s parenthesis-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((or (not c) (char= c #\)))
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token)))))))
	(ccase c
	  (#\:
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))))
				 word-token nil)
		   ))
	  (otherwise (push c word-token))
	  )
	)
  )

(defmethod clear ((s block-scanner))
  ;;:= todo
  )

