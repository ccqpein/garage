;;; the top level commands of espresso
(defpackage #:espresso/commands
  (:use #:CL)
  
  (:import-from #:espresso/libs/command
				#:make-command
				#:*command-output*
				#:*command-error*)
    
  (:export #:*commands*
		   #:*plugins-commands*
		   
		   #:pick-command))

(in-package #:espresso/commands)

(defvar *commands* nil
  "the commands that belong espresso")

(defvar *plugins-commands*
  (list
   (cons espresso/plugin/homebrew:*plugin-name*
		 espresso/plugin/homebrew:*commands*))
  "the bucket that contains plugins commands")

(defun pick-command (argv)
  "pick the command from shell arguments"
  (let ((comm (car argv)))
	(or (find-if (lambda (command)
				   (string= comm
							(espresso/libs/command:command-comm command)))
				 *commands*)

		(loop for plugin-commands in *plugins-commands*
			  for p = (loop for plugin-command in (cdr plugin-commands)
							if (string= comm
										(espresso/libs/command:command-comm plugin-command))
							  return plugin-command)
			  if p return p))
	))

(defun install-receipt (&rest receipts)
  (loop for r in receipts
		for filename-version = (str:split ":" r)
		if (> (length filename-version) 2)
		  do (error "receipts too many \":\" inside")
		else
		  do (espresso/receipts:install
			  (apply #'espresso/receipts:look-up-receipt filename-version)
			  :output *command-output*
			  :error-output *command-error*)))

(defun new-receipt ()
  (format *command-output* "The name of receipt (package name):~%")
  (let ((name (read-line))
		(template-file-path (str:concat espresso/config:*espresso-self-folder* "receipt_template")))
	(with-open-file (f (str:concat espresso/config:*espresso-receipts-folder* name ".lisp")
					   :direction :output
					   :if-exists :error
					   :if-does-not-exist :create)
	  (format f
			  (uiop:read-file-string template-file-path)
			  name
			  name)
	  )))

(defun list-installed ()
  ;;:= next
  )

(setf *commands*
	  (list (make-command :comm "install"
						  :doc "install receipt(s)"
						  :func #'install-receipt)
			(make-command :comm "new"
						  :doc "make new receipt"
						  :func #'new-receipt)))
