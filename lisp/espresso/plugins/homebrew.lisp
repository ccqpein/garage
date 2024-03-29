(defpackage #:espresso/plugin/homebrew
  (:use #:CL
		#:espresso/libs/plugin
		#:espresso/libs/shell-util)

  (:import-from #:espresso/libs/command
				#:*command-output*
				#:*command-error*)
  
  (:export *commands*
		   *plugin-name*))

(in-package #:espresso/plugin/homebrew)

(defun binstall (&rest args)
  (shell-run-program
   (format nil "brew install~{~^ ~a~}" args)
   :output
   *command-output*
   :error-output
   *command-error*
   ))

(defun bupdate (&rest args)
  (shell-run-program
   "brew update"
   :output
   *command-output*
   :error-output
   *command-error*
   ))

(defun blist (&rest args)
  (shell-run-program
   "brew list"
   :output
   *command-output*
   :error-output
   *command-error*
   ))

(defun buninstall (&rest args)
  (shell-run-program
   (format nil "brew uninstall~{~^ ~a~}" args)
   :output
   *command-output*
   :error-output
   *command-error*
   ))

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

   (make-alias-command
	"buninstall" "brew uninstall"
	:command-func #'buninstall)

   (make-alias-command
	"binfo" "brew info"
	:command-func (lambda (&rest args)
					(shell-run-program (format nil "brew info~{~^ ~a~}" args)
									   :output *command-output*)))

   (make-alias-command
	"bupgrade" "brew upgrade"
	:command-func (lambda (&rest args)
					(shell-run-program (format nil "brew upgrade~{~^ ~a~}" args)
									   :output *command-output*)))
   ))

