(defpackage #:libs/plugin
  (:documentation
   "libs codes for plugins")
  (:use #:CL
		#:libs/command)
  (:export #:plugin-command-alias)
  )

(in-package #:libs/plugin)

(defstruct plugin-alias
  alias
  original
  )

(defun plugin-command-alias (to from)
  "give alias to plugins' command"
  (make-plugin-alias
   :alias to
   :original from
   ))

(defun alias-to-command (alias &key custom-doc)
  "generate the command struct from alias struct"
  (declare (plugin-alias alias))
  (make-command
   :comm (plugin-alias-alias alias)
   :doc (if custom-doc
			custom-doc
			(format nil "alias command of ~s" (plugin-alias-original alias)))))
