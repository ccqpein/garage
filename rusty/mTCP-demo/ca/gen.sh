openssl genrsa -out ca.key 2048
openssl req -new -x509 -key ca.key -out ca.crt

# server side

openssl genrsa -out localhost.key 2048
#openssl rsa -in localhost.key -pubout -out localhost.pubkey

openssl req -new -key localhost.key -addext "subjectAltName = DNS:localhost" -out localhost.csr
openssl x509 -req -in localhost.csr -CA ca.crt -CAkey ca.key -CAcreateserial -extfile ./configfile -out localhost.crt

cat localhost.crt ca.crt > localhost.bundle.crt

# client_0 side
openssl genrsa -out client_0.key 2048

openssl req -new -key client_0.key -addext "subjectAltName = DNS:localhost" -out client_0.csr

openssl x509 -req -in client_0.csr -CA ca.crt -CAkey ca.key -CAcreateserial -extfile ./configfile -out client_0.crt

cat client_0.crt client_0.key > client_0.pem
openssl pkcs12 -export -in client_0.pem -out client_0.p12 -name "client_0"

# client_1 side
openssl genrsa -out client_1.key 2048

openssl req -new -key client_1.key -addext "subjectAltName = DNS:localhost" -out client_1.csr

openssl x509 -req -in client_1.csr -CA ca.crt -CAkey ca.key -CAcreateserial -extfile ./configfile -out client_1.crt

cat client_1.crt client_1.key > client_1.pem
openssl pkcs12 -export -in client_1.pem -out client_1.p12 -name "client_1"
