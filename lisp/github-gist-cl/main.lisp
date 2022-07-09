(ql:quickload "github-api-cl")

(defclass gist-api-doc (github-api-doc:api-doc)
  ((content
    :initarg :content
    :accessor content
    :documentation "content send to api")))

;;:= just make the github-api-cl:api-client
(defun make-api-client ())

;;:= add two gist headers inside, make a github-api-call call with content
(defun api-call (client api)
  )
