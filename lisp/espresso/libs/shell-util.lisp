;;; shell-run package
(defpackage #:espresso/libs/shell-util
  (:use #:CL)
  (:export #:run-program)
  )

(in-package #:espresso/libs/shell-util)

(defun run-program (&rest argvs)
  "just alias of uiop:run-program"
  (apply #'uiop:run-program argvs))
