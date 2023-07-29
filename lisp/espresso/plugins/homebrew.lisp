(defpackage #:plugin/homebrew
  (:use #:CL
		#:libs/plugin))

(in-package #:plugin/homebrew)

(defparameter *commands*
  (list
   (make-alias-command
	"binstall" "brew install"
	:command-func #'binstall)

   (plugin-command-alias 
	"bupdate" "brew update"
	:command-func #'bupdate)

   (plugin-command-alias 
	"blist" "brew list"
	:command-func #'blist)
   ))

(defun binstall (&rest args)
  (apply
   #'libs/shell-util:sbcl-run-command
   "brew"
   "install"
   args))

(defun bupdate (&rest args)
  (apply
   #'libs/shell-util:sbcl-run-command
   "brew"
   "update"
   args))

(defun blist (&rest args)
  (apply
   #'libs/shell-util:sbcl-run-command
   "brew"
   "list"
   args))
