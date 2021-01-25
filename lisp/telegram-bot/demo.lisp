(ql:quickload "telegram-bot")

(defparameter *global-record* (make-list :size 5))
(defparameter *token*)

(defun check-if-inside-record (&optional (record *global-record*))
  )

;;:= Use this demo for organizing code 
(defun handle ()
  (let* ((mesg (telegram-bot:call-api-method
                (get-method "getUpdates")
                :token *token*
                :offset -10
                ))
         (obj (telegram-bot:parse-message mesg))
         (all-results (telegram-bot:get-value-with-keys
                       obj
                       '("result")))
         )
    ;; stop here
    (if (string= (caar (telegram-bot:get-value-with-keys
                        obj
                        '("result" "message" "entities" "type")))
                 "bot_command")
        (telegram-bot:call-api-method
         (telegram-bot:get-method "sendMessage")
         :token *token*
         :chat_id (car
                   (telegram-bot:get-value-with-keys
                    obj
                    '("result" "message" "chat" "id")))
         :text (concatenate 'string "hello, "
                            (car
                             (telegram-bot:get-value-with-keys
                              obj
                              '("result" "message" "chat" "first_name"))))))))
