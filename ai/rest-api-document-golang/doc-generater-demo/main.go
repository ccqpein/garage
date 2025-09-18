// mygenerator/main.go
package main

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"log"
	"os"
	"regexp"

	openai "github.com/sashabaranov/go-openai"
)

const pickHandlersPrompt = `this is the 
`

func callLLM(words string) (string, error) {
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		return "", errors.New("OPENAI_API_KEY environment variable not set")
	}

	client := openai.NewClient(apiKey)
	resp, err := client.CreateChatCompletion(
		context.Background(),
		openai.ChatCompletionRequest{
			Model: openai.GPT5,
			Messages: []openai.ChatCompletionMessage{
				{
					Role:    openai.ChatMessageRoleUser,
					Content: words,
				},
			},
			MaxTokens: 1500,
		},
	)

	if err != nil {
		return "", fmt.Errorf("chat completion error: %w", err)
	}

	// Check if there are any choices (responses)
	if len(resp.Choices) == 0 {
		return "", errors.New("no response choices from OpenAI API")
	}

	// Return the content of the first message from the assistant
	return resp.Choices[0].Message.Content, nil
}

func getFunctionDefination(filePath, funcName string, level int) (string, int, error) {
	sourceCode, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatalf("Error reading source file %s: %v", filePath, err)
	}

	regexPattern := fmt.Sprintf(`func\s+%s\s*\(.*?\)\s*\{\n(?s:.*?)\n\s*\}`, regexp.QuoteMeta(funcName))
	//fmt.Println("regexPattern: ", regexPattern)

	re := regexp.MustCompile(regexPattern)

	matches := re.FindStringSubmatch(string(sourceCode))
	matchIndex := re.FindStringIndex(string(sourceCode))

	if len(matches) > 0 {
		lineNumber := bytes.Count(sourceCode[:matchIndex[0]], []byte("\n")) + 1
		return matches[0], lineNumber, nil
	} else {
		return "", 0, fmt.Errorf("Function '%s' not found or did not match expected pattern in %s\n", funcName, filePath)
	}
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run mygenerator/main.go <source_file> <function_name>")
		fmt.Println("Example: go run mygenerator/main.go ../target.go MyFunction")
		os.Exit(1)
	}

	sourceFilePath := os.Args[1]
	functionName := os.Args[2]

	if def, ln, err := getFunctionDefination(sourceFilePath, functionName, 0); err != nil {
		panic(err)
	} else {
		fmt.Printf("%+v, %+v\n", def, ln)
	}
}
