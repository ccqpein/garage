(defpackage event
  (:use #:CL))

(in-package event)

(defclass meta-event () ())

(defclass event (meta-event) ())


