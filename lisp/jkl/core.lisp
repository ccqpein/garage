(defpackage :jkl-core
  (:use :CL :jkl-options)
  (:export :command)
  )

(in-package :jkl-core)

(defparameter *executor*
  #+sbcl
  #'sb-ext:run-program
  #-(or allegro clasp clisp clozure cmucl ecl gcl lispworks mkcl sbcl scl xcl)
  (error "no implemented")
  "the executor of command")

(defclass command ()
  ((name
    :initarg :name
    :accessor name)
   (options
    :initarg :options
    :accessor options
    :documentation "hashtable of options")))

(defmethod gen-options ((comm command) &rest args &key &allow-other-keys)
  (do ((this (car args) (car args))
       result)
      ((not this) result)
    (if (keywordp this)
        (let ((option (gethash (string this) (options comm)))
              )
          (if option
              (setf result (append result (restore-back-to-string option (second args))))
              (error (format nil "cannot find the option: ~a" this)))
          (setf args (cddr args)))
        (progn (setf result (append result (list this)))
               (setf args (cdr args))))))

#+sbcl
(defun sbcl-run (name options)
  "run in sbcl and return the output stream"
  (let ((out (make-string-output-stream)))
    (sb-ext:run-program name options
                        :search t
                        :output out
                        :error nil)
    out))

;;:= run command, need input a lot key words
(defmethod run ((comm command) &rest args &key &allow-other-keys)
  #+sbcl
  (sbcl-run (name comm) (gen-options comm))
  #-(or allegro clasp clisp clozure cmucl ecl gcl lispworks mkcl sbcl scl xcl)
  (error "no implemented")
  )
