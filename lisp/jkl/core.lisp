(defpackage :jkl-core
  (:use :CL :jkl-options)
  (:export :command
           :make-new-command
           :parse-option-from-help)
  )

(in-package :jkl-core)

(defparameter *jkl-cmd-folder* (merge-pathnames (asdf:system-source-directory :jkl) "cmd")
  "default cmd folder")

(defclass command ()
  ((name
    :initarg :name
    :accessor name)
   ;; (option-kind
   ;;  :initarg :option-kind
   ;;  :accessor option-kind
   ;;  :documentation "the option kind of this command")
   (subcommand
    :initarg :subcommand
    :accessor subcommand
    :documentation "the subcommand, hashtable")
   (options
    :initarg :options
    :accessor options
    :documentation "hashtable of options")))

;;:= todo
(defmethod print-object ((cmd command) stream)
  (format t "name: ~a~%")
  )

;;:= todo: need the subcommand thing
(defmethod gen-options ((comm command) &rest args &key &allow-other-keys)
  "give command and the keyword/value pairs and more argvs to get command line options"
  (do ((this (car args) (car args))
       result)
      ((not this) result)
    (if (keywordp this)
        (let ((option (gethash (string this) (options comm)))
              )
          (if option
              (setf result (append result (restore-back-to-string option (second args)))))
          (setf args (cddr args)))
        (progn (setf result (append result (list this)))
               (setf args (cdr args))))))

#+sbcl
(defun sbcl-run (name options &key (output *standard-output*) (error :output))
  "run in sbcl and return the output stream"
  (sb-ext:run-program name options
                      :search t
                      :output output
                      :error error)
  output)

(defmethod run ((comm command)
                &rest args
                &key (jkl-output *standard-output*) (jkl-error *error-output*)
                &allow-other-keys)
  #+sbcl
  (sbcl-run (name comm) (apply #'gen-options comm args) :output jkl-output ::error jkl-error)
  #-(or sbcl)
  (error "no implemented")
  )

;;;;;;;;;;;; tools for generate command

#|
(make-new-command "curl"
                  (mapcar (lambda (line) (parse-option-from-help 'option1 line))
                          help-lines)
                  ;;:= todo
                  ;; :subcommand `(,("subcommand name" '(options...) :subcommand ...))
                  )
|#

(defun make-new-command (name options &key subcommand)
  (let ((cmd (make-instance 'command :name name)))
    (loop with table = (make-hash-table :test 'equal)
          for opt in options
          do (if (short-option opt)
                 (setf (gethash (str:upcase (short-option opt)) table) opt))
          do (if (long-option opt)
                 (setf (gethash (str:upcase (long-option opt)) table) opt))
          finally (setf (options cmd) table)
          )

    (if subcommand
        (loop with table = (make-hash-table :test 'equal)
              for subc in subcommand
              do (setf (gethash (first subc) table) (apply #'make-new-command subc))
              finally (setf (subcommand cmd) table)
              ))
    cmd
    ))

(defun parse-option-from-help (class line)
  (let ((opt (make-instance class)))
    (option-match-string opt line)
    opt
    ))

(defun write-to-cmd-file (&key (cmd-folder *jkl-cmd-folder*))
  "write")

;;; define command package in core

(defpackage :jkl-cmd
  (:use :CL :jkl-core :jkl-options))
