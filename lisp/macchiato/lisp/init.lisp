(in-package #:cl-user)

(defpackage #:macchiato
  (:use #:CL))

(in-package #:macchiato)

(defvar *macchiato-root-dir* #P"~/.Macchiato/")

(defvar *download-folder* #P"~/Library/Caches/Macchiato/")

(uiop/filesystem:ensure-all-directories-exist (list *macchiato-root-dir* *download-folder*))
