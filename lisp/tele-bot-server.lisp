(ql:quickload '("yason" "flexi-streams" "woo"))

(defvar *listen-address*
  (with-open-file (s "./vault/listen-address")
    (read-line s)))

(defvar *listen-ports*
  (with-open-file (s "./vault/listen-port")
    (parse-integer (read-line s))))

(defun listen-update (env)
  (when (eq (getf env :REQUEST-METHOD) :POST)
    (format t "~a" (parse-http-body-to-json env))
    '(200 (:content-type "text/plain") ())))

(defun parse-http-body-to-json (env)
  (handler-case (let ((body (yason:parse (getf env :RAW-BODY))))
                  body)
    (error (e)
      (format nil "illegal http body"))))

;;; implement these two generic function for json parse
(defmethod SB-GRAY:STREAM-READ-CHAR ((s flexi-streams:in-memory-stream))
  (code-char (read-byte s)))

(defmethod SB-GRAY:STREAM-PEEK-CHAR ((s flexi-streams:in-memory-stream))
  (let ((char (code-char (flexi-streams:peek-byte s))))
    char))

(defun run ()
  (woo:run #'listen-update :address *listen-address* :port *listen-ports*))

(run)
