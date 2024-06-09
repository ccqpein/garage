package main

import (
	"context"
	"log"
	"os"
	"path/filepath"

	svix "github.com/svix/svix-webhooks/go"
)

func main() {
	ctx := context.Background()
	authTokenPath := filepath.Join("vault", "auth_token")
	authToken, err := os.ReadFile(authTokenPath)
	if err != nil {
		log.Fatalf("Failed to read auth_token: %v", err)
	}
	//fmt.Println("Auth Token Content:")
	//fmt.Println(string(authToken))

	svixClient := svix.New(string(authToken), nil)

	/// create the application
	// app, err := svixClient.Application.Create(ctx, &svix.ApplicationIn{
	// 	Name: "create application",
	// })

	// event and message

	// new message should has diff eventID
	// or give null
	// then the MESSAGE ID in consumer portal will equal with
	// MESSAGE ID in server portal
	//eventId := "evt_Wqb1k73rXprtTm7Qdlr38p"

	appId, err := os.ReadFile(filepath.Join("vault", "app_id"))
	if err != nil {
		log.Fatalf("Failed to read auth_token: %v", err)
	}
	svixClient.Message.Create(ctx, string(appId), &svix.MessageIn{
		EventType: "invoice.paid",
		//EventId:   *svix.NullableString(&eventId),
		EventId: *svix.NullableString(nil),
		Payload: map[string]interface{}{
			"type":    "invoice.paid",
			"id":      "invoice_WF7WtCLFFtd8ubcTgboSFNql",
			"status":  "paid",
			"attempt": 2,
		},
	})
}
