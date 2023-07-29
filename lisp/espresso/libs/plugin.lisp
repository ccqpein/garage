(defpackage #:libs/plugin
  (:documentation
   "libs codes for plugins")
  (:use #:CL
		#:libs/command)
  (:export #:make-alias-command))

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

(defun alias-to-command (alias &key custom-doc command-func)
  "generate the command struct from alias struct"
  (declare (plugin-alias alias))
  (make-command
   :comm (plugin-alias-alias alias)
   :doc (if custom-doc
			custom-doc
			(format nil "alias command of ~s" (plugin-alias-original alias)))
   :func command-func))


(defun make-alias-command (to from &key custom-doc command-func)
  "make alias command"
  (alias-to-command (plugin-command-alias to from)
					:custom-doc custom-doc
					:command-func command-func)
  )
