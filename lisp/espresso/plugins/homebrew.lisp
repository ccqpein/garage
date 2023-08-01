(defpackage #:espresso/plugin/homebrew
  (:use #:CL
		#:espresso/libs/plugin
		#:espresso/libs/shell-util)
  (:export *commands*
		   *plugin-name*))

(in-package #:espresso/plugin/homebrew)

(defun binstall (&rest args)
  (apply
   #'sbcl-run-command
   nil
   "brew"
   "install"
   args))

(defun bupdate (&rest args)
  (apply
   #'sbcl-run-command
   nil
   "brew"
   "update"
   args))

(defun blist (&rest args)
  (apply
   #'sbcl-run-command
   nil
   "brew"
   "list"
   args))

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
					(let ((output (make-string-output-stream)))
					  (sbcl-run-command output "brew" "update")
					  (sbcl-run-command output "brew" "upgrade")
					  (sbcl-run-command output "brew" "cleanup")
					  output)))
   ))

