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
  (format stream "{block scanner~%tokens: ~{~a~^, ~}}" (tokens bs)))

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
	  (#\( ) ;;:= todo
	  (#\  (if (/= 0 (length word-token))
			   (setf (tokens s) (append (tokens s)
										(list (concatenate 'string (reverse word-token))))
					 word-token nil)
			   ))
	  (otherwise (push c word-token))
	  )))

(defmethod clear ((s block-scanner))
  ;;:= todo
  )


