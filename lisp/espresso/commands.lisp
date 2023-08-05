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
  (dolist (r receipts)
	(espresso/receipts:install (espresso/receipts:look-up-receipt r)
							   :output *command-output*
							   :error-output *command-error*)))

(setf *commands*
	  (list (make-command :comm "install"
						  :doc "install receipt(s)"
						  :func #'install-receipt)))
