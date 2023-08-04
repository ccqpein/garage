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
  (apply
   #'run-program
   "brew install"
   :output
   *command-output*
   args))

(defun bupdate (&rest args)
  (apply
   #'run-program
   "brew update"
   :output
   *command-output*
   args))

(defun blist (&rest args)
  (apply
   #'run-program
   "brew list"
   :output
   *command-output*
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
					(run-program "brew update" :output *command-output*)
					(run-program "brew upgrade" :output *command-output*)
					(run-program "brew cleanup" :output *command-output*)))
   ))

