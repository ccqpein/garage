;;; client of github api call
(defpackage #:github-client
  (:use #:CL #:github-api-doc)
  (:shadow #:get) ;; shadow get from CL
  )

(in-package #:github-client)

(defclass api-client ()
  ((username
    :initarg :username
    :type string
    :initform ""
    :accessor username)
   
   (token
    :initarg :token
    :type string
    :initform ""
    :accessor token)))

(defmethod token-p ((clt api-client))
  "check if client has token"
  (declare (api-client clt))
  (not (string= "" (token clt))))

(defmethod token-p-or-input ((clt api-client))
  "check if client has token, if not, ask to input"
  (if (not (token-p clt))
      (progn
        (format t "Please input your token~%")
        (setf (token clt) (symbol-name (read)))
        )))

(defgeneric get (client api &key &allow-other-keys))

(defgeneric run (client api &key &allow-other-keys))

;;:= MAYBE need :before method to check if need token or not

(defmethod run ((clt api-client) (api api-doc) &key owner repo)
  (let ((url (make-call-url api)))
    
    ))

;;; client get 
(defmethod get ((clt api-client) (api api-doc) &key token))

