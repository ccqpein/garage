(defpackage #:libs/plugin
  (:documentation
   "libs codes for plugins")
  (:use #:CL)
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
   )
  )
