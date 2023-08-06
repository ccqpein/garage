(defpackage #:espresso/config
  (:use #:CL)
  (:export #:*espresso-self-folder*
		   #:*espresso-libs-folder*
		   #:*espresso-bins-folder*
		   #:*espresso-receipts-folder*
		   #:*espresso-custome-receipts-folder*
		   #:*espresso-config-folder*
		   ))

(in-package #:espresso/config)

;;(defvar *espresso-self-folder* "~/espresso/")
(defvar *espresso-self-folder* (namestring (uiop/os:getcwd))) ;;:= for developing 
(defvar *espresso-libs-folder* (str:concat *espresso-self-folder* "lib/"))
(defvar *espresso-bins-folder* (str:concat *espresso-self-folder* "bin/"))
(defvar *espresso-receipts-folder* (str:concat *espresso-self-folder* "receipts/"))
(defvar *espresso-custome-receipts-folder*)
(defvar *espresso-config-folder* "~/.config/espresso/") 

(defvar *espresso-cache-folder* #P"~/Desktop"
		"cache folder to download stuffs the receipt need")
