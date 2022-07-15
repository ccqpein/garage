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
(defmethod api-call ((client github-client:api-client) (api gist-api-doc)
                     &rest args)
  (if (content-p api)
      (github-client:github-api-call client api :content (content api))
      (github-client:github-api-call client api)
      ))
