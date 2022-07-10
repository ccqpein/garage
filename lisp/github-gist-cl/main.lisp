(ql:quickload "github-api-cl")

(defclass gist-api-doc (github-api-doc:api-doc)
  ((content
    :initarg :content
    :type string
    :initform ""
    :accessor content
    :documentation "content send to api")))

(defmethod content-p ((api gist-api-doc))
  (not (string= (content api) "")))

;;:= make method that input the content
(defmethod input-content ((api gist-api-doc)))

;;:= just make the github-api-cl:api-client
(defun make-api-client (&key token)
  (if token
      (make-instance 'github-client:api-client :token token)
      (make-instance 'github-client:api-client)))

;;:= add two gist headers inside, make a github-api-call call with content
(defun api-call (client api)
  )
