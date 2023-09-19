(defpackage #:espresso/config
  (:use #:CL)
  (:export #:*espresso-self-folder*
		   #:*espresso-libs-folder*
		   #:*espresso-bins-folder*
		   #:*espresso-recipes-folder*
		   #:*espresso-custome-recipes-folder*
		   #:*espresso-config-folder*
		   #:*espresso-cache-folder*
		   ))

(in-package #:espresso/config)

;;(defvar *espresso-self-folder* "~/espresso/")
(defvar *espresso-self-folder* "~/Code/garage/lisp/espresso/") ;;:= for developing 
(defvar *espresso-libs-folder* (str:concat *espresso-self-folder* "lib/"))
(defvar *espresso-bins-folder* (str:concat *espresso-self-folder* "bin/"))
(defvar *espresso-recipes-folder* (str:concat *espresso-self-folder* "recipes/"))
(defvar *espresso-custome-recipes-folder*)

(defvar *espresso-config-folder*
  (or
   (uiop:getenv "ESPRESSO_CONFIG")
   "~/.config/espresso/")) 

(defvar *espresso-cache-folder* #P"~/.cache/espresso/"
		"cache folder to download stuffs the recipe need")
