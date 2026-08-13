(load "./output/demo.asd")

(ql:quickload '("lisp-rpc" "demo"))

(use-package :lisp-rpc-server)
(use-package :demo)

;; 1. Initialize server
(defparameter *server* (make-rpc-server))

;; 2. Register handler (validated against rpc-endpoint-p)
(register-rpc-handler *server*
                      'demo:get-book
                      (lambda (req)
                        ;; req is automatically deserialized into a
                        ;; #S(DEMO:GET-BOOK ...) struct
                        (demo:make-book-info
                         :lang (make-language-preference :lang "en")
                         :title (demo:get-book-title req)
                         :id "12345")))

;; 3. Start server
(start-server *server*)
