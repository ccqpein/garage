(defpackage telegram-bot
  (:use #:CL
        #:tele-api-doc
        #:message-handler
        #:telegram-client
        )
  (:export #:call-api-method
           #:parse-message
           #:get-value-with-keys
           #:call-api-method
           #:read-token
           #:get-datatype
           #:get-method
           
           ;;#:*telegram-api-url*
           ))
;;:= TODO: need export symbols here...
;;:= and clean other packages' export symbols.

(in-package telegram-bot)
