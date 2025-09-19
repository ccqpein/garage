// mygenerator/main.go
package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"log"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"

	"github.com/openai/openai-go/v2"
	"github.com/openai/openai-go/v2/option"
	//openai "github.com/sashabaranov/go-openai"
)

const pickHandlersPrompt = `this is the golang rest handler register function. Try to pick the function name and the api endpoint. return the json data that the list of struct like:

[{"endpoint": "aa/bb/cc", "funcName": "healthCheckHandler", lnum: 2, colNum: 12}...]

lnum is the specific endpoint's line number in code snippet.
colNume is the *funcName* colomn number (can give any value in the range)

don't return anything else, the data is:


`

func callLLM(words string) (string, error) {
	apiKey := os.Getenv("OPENAI_API_KEY")
	if apiKey == "" {
		return "", errors.New("OPENAI_API_KEY environment variable not set")
	}
	client := openai.NewClient(
		option.WithAPIKey(apiKey),
	)
	chatCompletion, err := client.Chat.Completions.New(context.Background(), openai.ChatCompletionNewParams{
		Messages: []openai.ChatCompletionMessageParamUnion{
			openai.UserMessage(words),
		},
		Model: openai.ChatModelGPT5,
	})
	if err != nil {
		return "", err
	}
	return chatCompletion.Choices[0].Message.Content, nil
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

func getGoplsDefinition(sourceFilePath string, queryLine int, queryCol int) (string, int, int, error) {
	// Construct the argument for gopls
	posArg := fmt.Sprintf("%s:%d:%d", sourceFilePath, queryLine, queryCol)

	// Prepare the command
	cmd := exec.Command("gopls", "definition", posArg)

	// Execute the command and capture its output
	output, err := cmd.CombinedOutput()
	if err != nil {
		// Log the error output from gopls for debugging
		return "", 0, 0, fmt.Errorf("failed to run gopls definition: %w\nOutput:\n%s", err, string(output))
	}

	outputStr := strings.TrimSpace(string(output))

	re := regexp.MustCompile(`^(?P<path>.+):(?P<line>\d+):(?P<col>\d+)(?:-\d+)?(?:.*$)`)

	match := re.FindStringSubmatch(outputStr)
	if match == nil || len(match) < 4 { // Expect at least 4 groups: full match, path, line, col
		return "", 0, 0, fmt.Errorf("failed to parse gopls output: %s", outputStr)
	}

	// Extract captured groups by name
	resultPath := match[re.SubexpIndex("path")]
	resultLineStr := match[re.SubexpIndex("line")]
	resultColStr := match[re.SubexpIndex("col")]

	resultLine, err := strconv.Atoi(resultLineStr)
	if err != nil {
		return "", 0, 0, fmt.Errorf("failed to convert line number '%s' to int: %w", resultLineStr, err)
	}

	resultCol, err := strconv.Atoi(resultColStr)
	if err != nil {
		return "", 0, 0, fmt.Errorf("failed to convert column number '%s' to int: %w", resultColStr, err)
	}

	return resultPath, resultLine, resultCol, nil
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run mygenerator/main.go <source_file> <function_name>")
		fmt.Println("Example: go run mygenerator/main.go ../target.go MyFunction")
		os.Exit(1)
	}

	sourceFilePath := os.Args[1]
	functionName := os.Args[2]

	def, ln, err := getFunctionDefination(sourceFilePath, functionName, 0)
	if err != nil {
		panic(err)
	} else {
		fmt.Printf("%+v, %+v\n", def, ln)
	}

	// call llm to pick the endpoint and the
	llmResp, err := callLLM(pickHandlersPrompt + def)
	if err != nil {
		panic(err)
	}

	fmt.Printf("llmResp: %+v\n", llmResp)

	endpoints := []struct {
		endpoint string
		funcName string
		lnum     int
		colNum   int
	}{}

	if err := json.Unmarshal([]byte(llmResp), &endpoints); err != nil {
		panic(err)
	}

	fmt.Printf("%+v\n", endpoints)

	for _, ee := range endpoints {
		ee.lnum += ln - 1
	}

	fmt.Printf("updated?: %+v\n", endpoints)

	//for _,ee :=range endpoints {}
}
