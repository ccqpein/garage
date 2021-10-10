(ql:quickload "cffi")

(defpackage #:cffi-tutorial ;; let me try define package with string
  (:use #:cl #:cffi) ;; this part cannot use string
  )

(in-package #:cffi-tutorial)

(define-foreign-library libcurl
    (:darwin (:or "libcurl.3.dylib" "libcurl.dylib"))
  ;; (:unix (:or "libcurl.so.3" "libcurl.so"))
  (t (:default "libcurl")))

(use-foreign-library libcurl)

;; https://common-lisp.net/project/cffi/manual/html_node/Tutorial_002dInitializing.html
(defctype curl-code :int)

(defcfun "curl_global_init" curl-code
    (flags :long))

(defcfun "curl_easy_init" :pointer)
   
(defcfun "curl_easy_cleanup" :void
  (easy-handle :pointer))

;; https://common-lisp.net/project/cffi/manual/html_node/Tutorial_002deasy_005fsetopt.html
(defmacro define-curl-options (name type-offsets &rest enum-args)
  "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:
   
      (NAME TYPE NUMBER)
   
  Where the arguments are as they are with the CINIT macro defined
  in curl.h, except NAME is a keyword.
   
  TYPE-OFFSETS is a plist of TYPEs to their integer offsets, as
  defined by the CURLOPTTYPE_LONG et al constants in curl.h."
  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset)))
    ;; here is some define enum inside
    `(progn
       (defcenum ,name
         ,@(loop for (name type number) in enum-args
                 collect (list name (enumerated-value type number))))
       ',name)))               

 (define-curl-options curl-option
      (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
    (:noprogress long 43)
    (:nosignal long 99)
    (:errorbuffer objectpoint 10)
   (:url objectpoint 2))

#|
 (progn
    (defcenum curl-option
      (:noprogress 43)
      (:nosignal 99)
      (:errorbuffer 10010)
      (:url 10002))
    'curl-option)
|#
