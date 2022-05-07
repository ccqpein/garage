(in-package #:macchiato)

(defpackage #:macchiato.download-tools
  (:use #:CL #:macchiato)
  (:export #:downloadable))

(in-package #:macchiato.download-tools)

(defun download-type-p (dt)
  (ccase dt
    (:url t)))

(deftype download-type ()
  `(satisfies download-type-p))

(defclass downloadable ()
  ((type
    :initarg :type
    :type download-type)
   (from
    :initarg :from)
   ))

