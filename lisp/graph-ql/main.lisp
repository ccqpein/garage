;; (defun check-line-terminators (s)
;;   "check line terminator \\n and \\t"
;;   (declare (stream s) )
  
;;   )

(deftype status ()
  '(member :in-the-comment:
	))

(defclass scanner ()
  ()
  (:documentation "adam scanner class"))

(defgeneric clear (obj)
  (:documentation "clear all the content of this scanner")
  (:method-class scanner))

(defclass block-scanner (scanner)
  ()
  (:documentation "scanner for {} block"))

(defun scan (rest-s status-stack)
  
  )
