;;; define the spec of the api
;;; this should be optional

(def-rpc-package "demo")

(def-msg language-perfer :lang 'string)

(def-msg book-info
  :lang 'language-perfer
  :title 'string
  :version 'string
  :id 'number)

(def-rpc get-book
    '(:title 'string :vesion 'string :lang 'language-perfer)
  'book-info)
