(ql:quickload "grpc")
(grpc:init-grpc)
;; connect to go version server
;;(defvar *insecure-channel* (channel "localhost:9091"))

(grpc:with-insecure-channel (channel "localhost:9091")
  (format t "~a~%" channel)
  )

;; (grpc:with-ssl-channel (channel ("localhost:9091"
;; 								 (:pem-root-certs "")
;; 								 )))

;;:= ssl channel maybe in future
