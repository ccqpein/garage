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
