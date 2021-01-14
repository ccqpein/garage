(ql:quickload "telegram-bot")

;;:= Use this demo for organizing code 
(defun main ()
  (let* ((token (read))
         (mesg (telegram-bot::call-api-method
                (car telegram-bot::*all-methods*)
                :token token
                ;;:offset -1
                ))
         (obj (telegram-bot::parse-message mesg))
         )
    (if (string= (caar (telegram-bot::get-value-with-keys
                        obj
                        '("result" "message" "entities" "type")))
                 "bot_command")
        (telegram-bot::call-api-method
         (telegram-bot::get-method "sendMessage")
         :token token
         :chat_id (car
                   (telegram-bot::get-value-with-keys
                    obj
                    '("result" "message" "chat" "id")))
         :text (concatenate 'string "hello, "
                            (car
                             (telegram-bot::get-value-with-keys
                              obj
                              '("result" "message" "chat" "first_name"))))))))
