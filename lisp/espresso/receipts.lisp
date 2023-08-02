(defpackage #:espresso/receipts
  (:use #:CL)
  (:export #:*cache-folder*)
  )

(in-package #:espresso/receipts)

;:= need default and need the config
(defparameter *cache-folder*
  #P"~/Desktop"
  "cache folder to download stuffs the receipt need")
