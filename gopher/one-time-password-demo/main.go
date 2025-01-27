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

	// input the code
	fmt.Print("Enter the TOTP code from your app to enable TOTP: ")
	var code string
	fmt.Scanln(&code) // Wait for user input

	err = userService.ValidateAndEnableTOTP("john", code)
	if err != nil {
		log.Fatalf("Failed to validate and enable TOTP: %v\n", err)
	}
	fmt.Println("TOTP successfully enabled!")

	// Simulate user login
	fmt.Print("Enter the TOTP code from your app to log in: ")
	var loginCode string
	fmt.Scanln(&loginCode) // Wait for the user to enter a login code

	err = userService.Login("john", loginCode)
	if err != nil {
		log.Fatalf("Login failed: %v\n", err)
	}

	fmt.Println("Login successful!")
}
