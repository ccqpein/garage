;;;; This package including functions those bot server needed
(defpackage telegram-client
  (:use #:CL
        #:tele-api-doc
        )
  (:export #:call-api-method
           #:*telegram-api-url*
           #:read-token))

(in-package telegram-client)

(defparameter *telegram-api-url* "https://api.telegram.org/bot~a/")

(defun read-token (filepath)
  (with-open-file (s filepath)
    (read-line s)))

;;;:= not finish yet, some types are missing
(defun method-type-checker (s v)
  (cond ((string= "PassportElementError" s))
        ((string= "ShippingOption" s))
        ((string= "LabeledPrice" s))
        ((string= "InlineQueryResult" s))
        ((string= "MaskPosition" s))
        ((string= "InputMedia" s))
        ((string= "BotCommand" s))
        ((string= "ChatPermissions" s))
        ((string= "Float number" s))
        ((string= "InputMediaAudio" s))
        ((string= "InlineKeyboardMarkup" s))
        ((string= "MessageEntity" s))
        ((string= "Integer or String" s)
         (typep v '(or (simple-array character (*)) fixnum)))
        ((string= "Boolean" s)
         (typep v 'boolean))
        ((string= "InputFile" s))
        ((string= "String" s)
         (stringp v))
        ((string= "Array of String" s)
         (and (typep v 'cons)
              (every #'(lambda (v)
                         (typep v '(simple-array character (*))))
                     v)))
        ((string= "Integer" s)
         (typep v 'fixnum))
        (t nil)))

(defun method-uri-parameters (key-pairs)
  (apply #'str:concat
         "?"
         (loop
           with re = '()
           for kp in key-pairs
    
           if (method-type-checker (second (car kp))
                                   (cadr kp))
             do (progn
                  (push "&" re)
                  (push (if (consp (cadr kp))
                            (let ((fs (format nil
                                              "~~{~a=~~a~~^&~~}"
                                              (string-downcase (string (caar kp))))))
                              (format nil fs (cadr kp)))
                            (format nil
                                    "~a=~a"
                                    (string-downcase (string (caar kp)))
                                    (cadr kp)))
                        re))
           else
             do (error "Input \"~a\" not match the type ~a"
                       (cadr kp)
                       (second (car kp))
                       )
           finally (return (cdr (reverse re)))
           )))

(defun make-method-uri (method &rest args &key (url *telegram-api-url*) token &allow-other-keys)
  "make method url for calling telegram api"
  (str:concat
   (format nil url token) ;; give token
   (api-method-name method) ;; name of method
   (method-uri-parameters (api-keywords-parser method args)))) ;; query string

(defun call-api-method (method &rest args &key (http-method 'get) &allow-other-keys)
  (declare (api-method method))
  (let ((endpoint (apply #'make-method-uri method args)))
    (ccase http-method
      ('get (dex:get endpoint))
      ('post (dex:post endpoint)))
    )
  )
