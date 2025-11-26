(ql:quickload '("str" "alexandria"))

(defparameter *example* "  (def-msg language-perfer :lang 'string)

  (def-msg book-info
    :lang 'language-perfer
    :title 'string
    :version 'string
    :id 'string)

  (def-rpc get-book
      '(:title 'string :vesion 'string :lang '(:lang 'string :encoding 'number))
    'book-info)")

(defparameter *checker-map*
  (mapcar #'cons
       '("def-msg" "def-rpc")
       '(def-msg-checker def-rpc-checker)))

(defun spec-check-one (expr)
  "get one expr, check it roughly and try to eval real checker it"
  (if (< (length expr) 2)
      (return-from
       spec-check-one
        (error "spec expr at least have two elements inside")))
  (let ((x (first expr))        
        checker)
    (loop for (sx . c) in *checker-map*
          when (or (string= x sx) (string= x (str:upcase sx)))
               do (setf checker c)
               and return nil
          finally
             (return-from spec-check-one
               (error (format nil
                              "spec expr only support the ~{~a~^, ~}"
                              (mapcar #'car *checker-map*)))))
    (apply checker (cdr expr))))

(defun def-msg-checker (name &rest args)
  (print name))


;; (let ((s (make-string-input-stream *example*)))
;;   (print (read s))
;;   (print (read s))
;;   (print (read s)))
