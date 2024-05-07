(ql:quickload "github-api-cl")

;;(import 'github-api-doc:api-doc)

(defparameter *hacker-news-get-item-api*
  (make-instance 'github-api-doc:api-doc
                 :api "GET /v0/item/:id.json"
                 :parameters '(("print" "string"))))

(setf github-api-doc::*api-root-url* "https://hacker-news.firebaseio.com")

;; not right because the api is weird has "<id>.json?blablabla"
(github-client:github-api-call (make-instance 'github-client:api-client)
                               *hacker-news-get-item-api*
                               :print "pretty")
