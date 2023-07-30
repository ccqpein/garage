;;; the top level commands of espresso

(defpackage #:espresso/commands
  (:use #:CL)
  (:import-from #:espresso/libs/command
				#:make-command)
  (:export #:*commands*
		   #:*plugins-commands*))

(in-package #:espresso/commands)

(defvar *commands* nil
  "the commands that belong espresso")

(defvar *plugins-commands*
  (list
   (cons espresso/plugin/homebrew:*plugin-name*
		 espresso/plugin/homebrew:*commands*))
  "the bucket that contains plugins commands")

(defun install-receipt (&rest receipts)
  ;;;= todo here
  )

(setf *commands*
	  (list (make-command :comm "install"
						  :doc "install receipt(s)"
						  :func #'install-receipt)))
