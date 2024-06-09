package main

import (
	"net/http"

	svix "github.com/svix/svix-webhooks/go"
)

func main() {
	// will find in app consumer portal
	// but I don't have my endpoint. So I just left demo from doc
	secret := "whsec_MfKQ9r8GKYqrTwjUPD8ILPZIo2LaLaSw"

	// These were all sent from the server
	headers := http.Header{}
	headers.Set("svix-id", "msg_p5jXN8AQM9LWM0D4loKWxJek")
	headers.Set("svix-timestamp", "1614265330")
	headers.Set("svix-signature", "v1,g0hM9SsE+OTPJTGt/tmIKtSyZlE3uFJELVlNIOLJ1OE=")
	//
	payload := []byte(`{"test": 2432232314}`)

	wh, _ := svix.NewWebhook(secret)
	_ = wh.Verify(payload, headers)
	// returns nil on success, error otherwise
}
