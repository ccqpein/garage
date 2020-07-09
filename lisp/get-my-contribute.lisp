(ql:quickload '("dexador" "yason"))

(defun get-all-repos ()
  (let ((token (progn (format t "token:~%")
                      (read-line))))
    (yason:parse
     (car (multiple-value-list
           (dex:get "https://api.github.com/users/ccqpein/repos"
                    :headers (list
                              (cons "Authorization" (format nil "token ~a" token)))))))))

;; parse time
(decode-universal-time (+ 1593302400 (encode-universal-time 0 0 0 1 1 1970 5)) 5)

;; find method of generic function
(sb-mop:specializer-direct-generic-functions (find-class 'string))


(defmethod test :before ((a string) &key aa &allow-other-keys)
  (print :allow-other-keys)
  (print aa)
  (print "bbb"))

(defmethod test ((a string) &key)
  ;(print aa)
  (print "aa"))

(defmethod test :after ((a string) &key)
  (print a))
