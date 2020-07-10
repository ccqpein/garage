;;; client of github api call
(defpackage #:github-client
  (:use #:CL #:github-api-doc)
  (:shadow #:get) ;; shadow get from CL
  )

(in-package #:github-client)

(ql:quickload '("dexador" "yason"))

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

(defgeneric http-call (client url &rest args &key method &allow-other-keys))

(defmethod http-call ((clt api-client) url &key (method :get))
  (declare (ignore clt))
  (let ((call-func (ecase method
                     (:get #'dex:get)
                     (:post #'dex:post))))
    (funcall call-func url)))
