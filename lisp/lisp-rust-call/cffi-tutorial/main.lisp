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


;; https://common-lisp.net/project/cffi/manual/html_node/Tutorial_002dLisp-easy_005fsetopt.html
;;; We will use this type later in a more creative way.  For now, just
;;; consider it a marker that this isn't just any pointer.
(defctype easy-handle :pointer)
   
(defmacro curl-easy-setopt (easy-handle enumerated-name
                            value-type new-value)
  "Call `curl_easy_setopt' on EASY-HANDLE, using ENUMERATED-NAME
  as the OPTION.  VALUE-TYPE is the CFFI foreign type of the third
  argument, and NEW-VALUE is the Lisp data to be translated to the
  third argument.  VALUE-TYPE is not evaluated."
  `(foreign-funcall "curl_easy_setopt" easy-handle ,easy-handle
                    curl-option ,enumerated-name
                    ,value-type ,new-value curl-code))

(defun curry-curl-option-setter (function-name option-keyword)
  "Wrap the function named by FUNCTION-NAME with a version that
  curries the second argument as OPTION-KEYWORD.
   
  This function is intended for use in DEFINE-CURL-OPTION-SETTER."
  (setf (symbol-function function-name)
        (let ((c-function (symbol-function function-name)))
          (lambda (easy-handle new-value)
            (funcall c-function easy-handle option-keyword
                     new-value)))))
   
(defmacro define-curl-option-setter (name option-type
                                     option-value foreign-type)
  "Define (with DEFCFUN) a function NAME that calls
  curl_easy_setopt.  OPTION-TYPE and OPTION-VALUE are the CFFI
  foreign type and value to be passed as the second argument to
  easy_setopt, and FOREIGN-TYPE is the CFFI foreign type to be used
  for the resultant function's third argument.
   
  This macro is intended for use in DEFINE-CURL-OPTIONS."
  `(progn
     (defcfun ("curl_easy_setopt" ,name) curl-code
       (easy-handle easy-handle)
       (option ,option-type)
       (new-value ,foreign-type))
     (curry-curl-option-setter ',name ',option-value)))
   
(defmacro define-curl-options (type-name type-offsets &rest enum-args)
  "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:
   
      (NAME TYPE NUMBER)
   
  Where the arguments are as they are with the CINIT macro defined
  in curl.h, except NAME is a keyword.
   
  TYPE-OFFSETS is a plist of TYPEs to their integer offsets, as
  defined by the CURLOPTTYPE_LONG et al constants in curl.h.
   
  Also, define functions for each option named
  set-`TYPE-NAME'-`OPTION-NAME', where OPTION-NAME is the NAME from
  the above destructuring."
  (flet ((enumerated-value (type offset)
           (+ (getf type-offsets type) offset))
         ;; map PROCEDURE, destructuring each of ENUM-ARGS
         (map-enum-args (procedure)
           (mapcar (lambda (arg) (apply procedure arg)) enum-args))
         ;; build a name like SET-CURL-OPTION-NOSIGNAL
         (make-setter-name (option-name)
           (intern (concatenate
                    'string "SET-" (symbol-name type-name)
                    "-" (symbol-name option-name)))))
    `(progn
       (defcenum ,type-name
           ,@(map-enum-args
              (lambda (name type number)
                (list name (enumerated-value type number)))))
       ,@(map-enum-args
          (lambda (name type number)
            (declare (ignore number))
            `(define-curl-option-setter ,(make-setter-name name)
               ,type-name ,name ,(ecase type
                                   (long :long)
                                   (objectpoint :pointer)
                                   (functionpoint :pointer)
                                   (off-t :long)))))
       ',type-name)))

;; https://cffi.common-lisp.dev/manual/html_node/Tutorial_002dMemory.html

(set-curl-option-url *easy-handle* "http://www.cliki.net/CFFI")

;;:= note this one has problem “undefined behavior”
;; (with-foreign-string (url "http://www.cliki.net/CFFI")
;;        (foreign-funcall "curl_easy_setopt" easy-handle *easy-handle*
;;                         curl-option :url :pointer url curl-code))

 (let (easy-handle)
    (unwind-protect
      (with-foreign-string (url "http://www.cliki.net/CFFI")
        (setf easy-handle (curl-easy-init))
        (set-curl-option-url easy-handle url)
        #|do more with the easy-handle, like actually get the URL|#)
      (when easy-handle
        (curl-easy-cleanup easy-handle))))

(defvar *easy-handle-cstrings* (make-hash-table)
  "Hashtable of easy handles to lists of C strings that may be
  safely freed after the handle is freed.")
   
(defun make-easy-handle ()
  "Answer a new CURL easy interface handle, to which the lifetime
  of C strings may be tied.  See `add-curl-handle-cstring'."
  (let ((easy-handle (curl-easy-init)))
    (setf (gethash easy-handle *easy-handle-cstrings*) '())
    easy-handle))

(defun free-easy-handle (handle)
  "Free CURL easy interface HANDLE and any C strings created to
  be its options."
  (curl-easy-cleanup handle)
  (mapc #'foreign-string-free
        (gethash handle *easy-handle-cstrings*))
  (remhash handle *easy-handle-cstrings*))
   
(defun add-curl-handle-cstring (handle cstring)
  "Add CSTRING to be freed when HANDLE is, answering CSTRING."
  (car (push cstring (gethash handle *easy-handle-cstrings*))))


(defun curry-curl-option-setter (function-name option-keyword)
  "Wrap the function named by FUNCTION-NAME with a version that
  curries the second argument as OPTION-KEYWORD.
   
  This function is intended for use in DEFINE-CURL-OPTION-SETTER."
  (setf (symbol-function function-name)
        (let ((c-function (symbol-function function-name)))
          (lambda (easy-handle new-value)
            (funcall c-function easy-handle option-keyword
                     (if (stringp new-value)
                         (add-curl-handle-cstring
                          easy-handle
                          (foreign-string-alloc new-value))
                         new-value))))))

;; https://cffi.common-lisp.dev/manual/html_node/Tutorial_002dCallbacks.html#Tutorial_002dCallbacks

;;; Alias in case size_t changes.
(defctype size :unsigned-int)
   
;;; To be set as the CURLOPT_WRITEFUNCTION of every easy handle.
(defcallback easy-write size
  ((ptr :pointer) (size size)
   (nmemb size) (stream :pointer))
  (let ((data-size (* size nmemb)))
    (handler-case
        ;; We use the dynamically-bound *easy-write-procedure* to
        ;; call a closure with useful lexical context.
        (progn (funcall (symbol-value '*easy-write-procedure*)
                        (foreign-string-to-lisp ptr :count data-size))
               data-size)               ;indicates success
      ;; The WRITEFUNCTION should return something other than the
      ;; #bytes available to signal an error.
      (error () (if (zerop data-size) 1 0)))))

(define-curl-options curl-option
    (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
  (:noprogress long 43)
  (:nosignal long 99)
  (:errorbuffer objectpoint 10)
  (:url objectpoint 2)
  (:writefunction functionpoint 11)) ;new item here


 (defcfun "curl_easy_perform" curl-code
   (handle easy-handle))

;; https://cffi.common-lisp.dev/manual/html_node/Tutorial_002dTypes.html

