(ql:quickload '("closer-mop" "str" "lisp-unit"))

(defpackage #:scanner
  (:use #:CL)
  (:export #:block-scanner
		   #:parenthesis-scanner
		   #:tokens
		   #:scan
		   #:equal-scanner)
  )

(in-package #:scanner)

(defparameter *ignonre-tokens*
  (let ((table (make-hash-table
				:test 'equal)))
	(setf (gethash #\newline table) t
		  (gethash "" table) t)
	table))

(defun ignore-token-p (x)
  (gethash x *ignonre-tokens*))

(defclass scanner () ())

(defgeneric equal-scanner (s1 s2)
  (:documentation "comparing scanners"))

#|
block-scanner class below
|#

(defclass block-scanner (scanner)
  (
   (tokens
	:initarg :tokens
	:initform nil
	:accessor tokens)
   )
  (:documentation "scanner for {} block"))

(defmethod equal-scanner ((s1 block-scanner) (s2 block-scanner))
  (do* ((s1-tokens (tokens s1) (cdr s1-tokens))
		(s2-tokens (tokens s2) (cdr s2-tokens))
		(s1-t (car s1-tokens) (car s1-tokens))
		(s2-t (car s2-tokens) (car s2-tokens)))
	   ((if (and (not s1-t) (not s2-t))
			t)
		t)
	(typecase s1-t
	  (scanner (if (not (equal-scanner s1-t s2-t))
				   (progn
					 (format t "~s not equal ~s" s1-t s2-t)
					 (return-from equal-scanner nil))))
	  (otherwise
	   (if (not (equal s1-t s2-t))
		   (progn
			 (format t "~s not equal ~s" s1-t s2-t)
			 (return-from equal-scanner nil)))))
	))

(defmethod print-object ((bs block-scanner) stream)
  (format stream "{block: tokens: ~{~a~^, ~}}" (tokens bs)))

(defmethod scan ((s block-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((or (not c) (char= c #\}))
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))))
	   (setf (tokens s)
			 (remove-if #'ignore-token-p (tokens s))))
	(case c
	  (#\{ (setf (tokens s)
				 (append (tokens s)
						 (list
						  (concatenate 'string (reverse word-token))
						  (let ((sub-block-scanner (make-instance 'block-scanner)))
							(scan sub-block-scanner stream)
							sub-block-scanner)))
				 word-token
				 nil))
	  
	  (#\( (setf (tokens s)
				 (append (tokens s)
						 (list
						  (concatenate 'string (reverse word-token))
						  (let ((sub-block-scanner (make-instance 'parenthesis-scanner)))
							(scan sub-block-scanner stream)
							sub-block-scanner)))
				 word-token
				 nil))
	  
	  ((#\  #\, #\newline) ;; ignore tokens 
	   (setf (tokens s)
			 (append (tokens s)
					 (list (concatenate 'string (reverse word-token))))
			 word-token
			 nil)
	   )	  

	  ((#\#)
	   (setf (tokens s)
			 (append (tokens s)
					 (list
					  (concatenate 'string (reverse word-token))
					  (let ((sub-block-scanner (make-instance 'comment-scanner)))
						(scan sub-block-scanner stream)
						sub-block-scanner
						)))
			 word-token
			 nil)
	   )
	  
	  ((#\. #\:) ;; : is special
	   (setf (tokens s)
			 (append (tokens s)
					 (list (concatenate 'string (reverse word-token))
						   c))
			 word-token
			 nil))
	  
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
		
		(reverse result))
	
	(ctypecase this-token
	  (string
	   (cond ((not last-colon)
			  (if cache-sentence
				  (progn (push cache-sentence result)
						 (setf cache-sentence (make-instance 'struct-sentence :name this-token)))
				  (setf cache-sentence (make-instance 'struct-sentence :name this-token))))
		   ))
	  (scanner
	   (if (not cache-sentence) ;; when only block body
		   (setf cache-sentence (make-instance 'struct-sentence)))
	   (ctypecase this-token
		 (parenthesis-scanner
		  (setf (arguments cache-sentence) (schema-values this-token)))
		 (block-scanner
		  (setf (sub-sentences cache-sentence) (schema-values this-token)))
		 ;;:= todo: (comment-scanner)
		 )
	   ))	
	))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass parenthesis-scanner (scanner)
  ((tokens
	:initarg :tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of () block")
  )

(defmethod equal-scanner ((s1 parenthesis-scanner) (s2 parenthesis-scanner))
  (do* ((s1-tokens (tokens s1) (cdr s1-tokens))
		(s2-tokens (tokens s2) (cdr s2-tokens))
		(s1-t (car s1-tokens) (car s1-tokens))
		(s2-t (car s2-tokens) (car s2-tokens)))
	   ((if (and (not s1-t) (not s2-t))
			t)
		t)
	(typecase s1-t
	  (scanner (if (not (equal-scanner s1-t s2-t))
				   (progn
					 (format t "~s not equal ~s" s1-t s2-t)
					 (return-from equal-scanner nil))))
	  (otherwise
	   (if (not (equal s1-t s2-t))
		   (progn
			 (format t "~s not equal ~s" s1-t s2-t)
			 (return-from equal-scanner nil)))))
	))

(defmethod print-object ((ps parenthesis-scanner) stream)
  (format stream "{parenthesis block: tokens: ~{~a~^, ~}}" (tokens ps)))

(defmethod scan ((s parenthesis-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((or (not c) (char= c #\)))
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))))
	   (setf (tokens s)
			 (remove-if #'ignore-token-p (tokens s)))
	   )
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
										  ":"))
				 word-token nil)
		   (setf (tokens s) (append (tokens s) '(":")))))
	  (#\,
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token))
										  ","))
				 word-token nil)
		   (setf (tokens s) (append (tokens s) '(",")))))
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

	(case this-token
	  (":"
	   (setf (val cache-sentence) (cadr tokens)
			 tokens (cdr tokens)))
	  (","
	   (push cache-sentence result)
	   (setf cache-sentence nil))
	  (otherwise
	   (if (not cache-sentence)
		   (setf cache-sentence (make-instance 'arguments-sentence :key this-token))
		   (progn (push cache-sentence result)
				  (setf cache-sentence (make-instance 'arguments-sentence :key this-token)))))
	  )
	(setf tokens (cdr tokens)))
  )
  
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass plain-scanner (scanner)
  ((tokens
	:initarg :tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of plain text (like fragment)")
  )

(defmethod equal-scanner ((s1 plain-scanner) (s2 plain-scanner))
  (do* ((s1-tokens (tokens s1) (cdr s1-tokens))
		(s2-tokens (tokens s2) (cdr s2-tokens))
		(s1-t (car s1-tokens) (car s1-tokens))
		(s2-t (car s2-tokens) (car s2-tokens)))
	   ((if (and (not s1-t) (not s2-t))
			t)
		t)
	(typecase s1-t
	  (scanner (if (not (equal-scanner s1-t s2-t))
				   (progn
					 (format t "~s not equal ~s" s1-t s2-t)
					 (return-from equal-scanner nil))))
	  (otherwise
	   (if (not (equal s1-t s2-t))
		   (progn
			 (format t "~s not equal ~s" s1-t s2-t)
			 (return-from equal-scanner nil)))))
	))

(defmethod scan ((s plain-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((not c)
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))))
	   (setf (tokens s)
			 (remove-if #'ignore-token-p (tokens s))))
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

(defclass comment-scanner (scanner)
  ((tokens
	:initarg :tokens
	:initform nil
	:accessor tokens))
  (:documentation "scanner of comments line")
  )

(defmethod scan ((s comment-scanner) stream)
  (do ((c (read-char stream nil nil) (read-char stream nil nil))
	   (word-token nil))
	  ((char= c #\newline)
	   (if (/= 0 (length word-token))
		   (setf (tokens s) (append (tokens s)
									(list (concatenate 'string (reverse word-token)))))))
	(case c
	  ((#\ )
	   (if (/= 0 (length word-token))
		   (setf (tokens s)
				 (append (tokens s)
						 (list (concatenate 'string (reverse word-token))))
				 word-token
				 nil)
		   )
	   )
	  (otherwise (push c word-token))
	  )))

(defmethod schema-values ((s comment-scanner))
  (make-instance 'comment-sentence
				 :content
				 (str:join #\ (tokens s))))


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
   (val ;;:= next: need some val change like false to nil; true to t
	:initarg :val
	:accessor val))
  )

(defmethod print-object ((as arguments-sentence) stream)
  (format stream "{key: ~a, val: ~a}" (key as) (val as)))

(defmethod to-keys ((as arguments-sentence))
  (list (read-from-string (str:concat ":" (key as)))
		(map-graphql-value (val as))))


(defclass comment-sentence (sentence)
  ((content
	:initarg :content
	:accessor content)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *values-mapping* (make-hash-table :test 'equal)
  "mapping the value from graph ql to lisp object"
  )

(setf (gethash "false" *values-mapping*) nil
	  (gethash "true" *values-mapping*) t)

(defun map-graphql-value (gv)
  (gethash gv *values-mapping*))
