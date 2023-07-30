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
   "brew"
   "install"
   args))

(defun bupdate (&rest args)
  (apply
   #'sbcl-run-command
   "brew"
   "update"
   args))

(defun blist (&rest args)
  (apply
   #'sbcl-run-command
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
   ))

