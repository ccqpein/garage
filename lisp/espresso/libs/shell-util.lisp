;;; shell-run package
(defpackage #:espresso/libs/shell-util
  (:use #:CL)
  (:export #:shell-run-program)
  )

(in-package #:espresso/libs/shell-util)

(defun shell-run-program (&rest argvs)
  "just alias of uiop:run-program"
  ;;:= maybe destructuring-bind the output and error output?
  (uiop:wait-process (apply #'uiop:launch-program argvs)))
