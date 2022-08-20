(ql:quickload "grpc")
(grpc:init-grpc)

;; protobufs support
(ql:quickload "cl-protobufs")
(grpc:with-insecure-channel (channel "localhost:9091")
  ;;(format t "~a~%" channel)
  (let* ((message)
		 (response))
	))

;; (grpc:with-ssl-channel (channel ("localhost:9091"
;; 								 (:pem-root-certs "")
;; 								 )))

;;:= ssl channel maybe in future
