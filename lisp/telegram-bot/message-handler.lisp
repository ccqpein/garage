(defpackage message-handler
  (:use #:CL)
  (:export #:parse-message
           #:get-value-with-keys))

(in-package message-handler)

(defun parse-message (msg)
  (yason:parse msg))

(defun get-value-with-keys (obj keys)
  "recursivly get key from obj"
  (if (not keys)
      obj
      (typecase obj
        (hash-table (get-value-with-keys (gethash (car keys) obj) (cdr keys)))
        (cons (loop
                for x in obj
                collect (get-value-with-keys x keys)))
        (otherwise obj))))

