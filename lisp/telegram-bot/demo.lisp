(defparameter *decode-stream* (dex:get "https://core.telegram.org/bots/api" :want-stream t))

;;; read time after time, then will get the right result
(let ((buf (make-string (* 4 1024)))) (read-sequence buf *decode-stream*) (format t "~a" buf))
