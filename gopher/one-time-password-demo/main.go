package main

import (
	"fmt"
	"log"
	. "one-time-password-demo/lib"
)

func main() {
	userService := NewUserService()

	// Register a new user
	userService.Users["john"] = &User{Username: "john"}

	// Enable TOTP for the user
	secret, qrURL, err := userService.EnableTOTP("john")
	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("Secret: %s\n", secret)
	fmt.Printf("QR Code URL: %s\n", qrURL)

	// User scans QR code with their authenticator app
	// and enters the code
	code := "123456" // This would be user input
	err = userService.ValidateAndEnableTOTP("john", code)
	if err != nil {
		log.Fatal(err)
	}

	// Later, during login
	loginCode := "654321" // This would be user input
	err = userService.Login("john", loginCode)
	if err != nil {
		log.Fatal(err)
	}
}
