;;; client of github api call
(defpackage #:github-client
  (:use #:CL)
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

;;; client get 
(defmethod get ((clt api-client)))

