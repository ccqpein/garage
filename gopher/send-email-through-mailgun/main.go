package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"time"

	"github.com/mailgun/mailgun-go/v4"
	"github.com/pelletier/go-toml/v2"
)

type Config struct {
	Domain    string `toml:"domain"`
	APIKey    string `toml:"apikey"`
	Sender    string `toml:"sender"`
	Recipient string `toml:"recipient"`
}

func main() {

	data, err := os.ReadFile(".env.toml")
	if err != nil {
		log.Fatalf("Failed to read .env.toml: %v", err)
	}

	var config Config
	err = toml.Unmarshal(data, &config)
	if err != nil {
		log.Fatalf("Failed to unmarshal .env.toml: %v", err)
	}

	// Create an instance of the Mailgun Client
	mg := mailgun.NewMailgun(config.Domain, config.APIKey)

	//When you have an EU-domain, you must specify the endpoint:
	//mg.SetAPIBase("https://api.eu.mailgun.net/v3")

	sender := config.Sender
	subject := "Fancy subject!"
	body := "Hello from Mailgun Go!"
	recipient := config.Recipient

	// The message object allows you to add attachments and Bcc recipients
	message := mailgun.NewMessage(sender, subject, body, recipient)

	ctx, cancel := context.WithTimeout(context.Background(), time.Second*10)
	defer cancel()

	// Send the message with a 10 second timeout
	resp, id, err := mg.Send(ctx, message)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Printf("ID: %s Resp: %s\n", id, resp)
}
