(defpackage #:espresso/libs/fs
  (:use #:CL)
  (:export #:if-file-exist)
  )

(in-package #:espresso/libs/fs)

(defun if-file-exist (folder-name)
  (declare (pathname folder-name))
  (uiop:directory-exists-p folder-name)
  )
