(defpackage #:espresso/plugin/homebrew
  (:use #:CL
		#:espresso/libs/plugin
		#:espresso/libs/shell-util)

  (:import-from #:espresso/libs/command
				#:*command-output*)
  
  (:export *commands*
		   *plugin-name*))

(in-package #:espresso/plugin/homebrew)

(defun binstall (&rest args)
  (shell-run-program
   (format nil "brew install~{~^ ~a~}" args)
   :output
   *command-output*))

(defun bupdate (&rest args)
  (shell-run-program
   "brew update"
   :output
   *command-output*))

(defun blist (&rest args)
  (shell-run-program
   "brew list"
   :output
   *command-output*))

(defparameter *plugin-name* "homebrew")

(defparameter *commands*
  (list
   (make-alias-command
	"binstall" "brew install"
	:command-func #'binstall)

   (make-alias-command 
	"bupdate" "brew update"
	:command-func #'bupdate)

   (make-alias-command 
	"blist" "brew list"
	:command-func #'blist)

   (make-alias-command 
	"bubu" "brew update && brew upgrade && brew cleanup"
	:command-func (lambda ()
					(shell-run-program "brew update" :output *command-output*)
					(shell-run-program "brew upgrade" :output *command-output*)
					(shell-run-program "brew cleanup" :output *command-output*)))
   ))

