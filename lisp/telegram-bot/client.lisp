(defparameter *telegram-api-url* "https://api.telegram.org/bot~a/")

(defun read-token (filepath)
  (with-open-file (s filepath)
    (read-line s)))

(defun call-method (mtnd)
  (declare (api-method mtnd))
  ;;(make-uri mtnd)
  )

(defun method-uri-parameters (key-pairs)
  (str:join "?" )) ;;:= TODO: need type check

;;;:= TODO: this function
(defun make-method-uri (mthd &rest args &key (url *telegram-api-url*) token &allow-other-keys)
  "make method url for calling telegram api"
  (str:join
   (format nil url token)
   (api-keywords-parser mthd args)))

(defun call-method-by-name (name)
  (call-method ))
