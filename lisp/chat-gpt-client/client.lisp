;;; http client package
(defpackage #:client
  (:use #:CL)
  (:export)
  )

(in-package #:client)

(defun call-api (message
				 &key
				   (model "gpt-4")
				   (api-token (uiop:getenv "OPENAI_API_KEY")))
  (dex::post
   "https://api.openai.com/v1/chat/completions"
   :headers (list (cons "Authorization" (format nil "Bearer ~a" api-token))
				  (cons "Content-Type" "application/json"))
   :content message)
  )

(defstruct message
  "the message struct send to chat api"
  role
  content)

(defmethod to-json ((a message))
  (let ((s (make-string-output-stream)))
	(yason:encode-alist
	 (list (cons "role" (message-role a))
		   (cons "content" (message-content a)))
	 s)
	(get-output-stream-string s)))

(defun make-request-body (model messages)
  "messages is the list of struct message"
  (format nil "{\"model\": ~S, \"messages\":[~{~a~}]}"
		  model
		  (mapcar (lambda (m) (to-json m)) messages))
  )

;;:= todo: response from json to message
