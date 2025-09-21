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
)

const pickHandlersPrompt = `this is the golang rest handler register function. Try to pick the function name and the api endpoint. return the json data that the list of struct like:

[{"endpoint": "aa/bb/cc", "funcName": "healthCheckHandler"}...]

don't return anything else, the data is:


`

const pickFuncCallInside = `this is the golang function code. pick the functions those called inside this function. pick the functions names out those will be used for lsp to find the defination. ignore all the std lib or some popular open source lib's funcions. format them like this:

["healthCheckHandler"...]

don't return anything else, the data is:


`

const thisEndpointDoc = `this is the api handler function of endpoint %s and all functions it calls inside. Use these codes to generate the human readable doc that what this handle function doing:

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

// get the function declaration from the file
// return the code snippets and the line number
func getFunctionDefination(filePath, funcName string) (string, int, error) {
	sourceCode, err := os.ReadFile(filePath)
	if err != nil {
		log.Fatalf("Error reading source file %s: %v", filePath, err)
	}

	regexPattern := fmt.Sprintf(`func(?:\s*\((?P<receiver>[^)]+)\))?\s+(%s\w*)\s*\([^)]*\)(?:\s*(?:[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*|\([^)]*\)))?\s*\{(?P<body>[\s\S]*?)\n\}\s*(?:$|\n)`, regexp.QuoteMeta(funcName))

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

// get the symbols line number and col number with gopls
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
	fmt.Printf("outputStr: %v\n", outputStr)

	re := regexp.MustCompile(`(?s)^(.+?):(\d+):(\d+)(?:-\d+)?.*$`)

	match := re.FindStringSubmatch(outputStr)

	var filePath, lineStr, colStr string
	var line, col int

	if len(match) >= 3 {
		filePath = match[1] // The first captured group (path)
		lineStr = match[2]  // The second captured group (line number)
		colStr = match[3]   // The third captured group (column number)

		line, _ = strconv.Atoi(lineStr)
		col, _ = strconv.Atoi(colStr)

		fmt.Printf("File Path: %s\n", filePath)
		fmt.Printf("Line: %d\n", line)
		fmt.Printf("Column: %d\n", col)

		// Desired output format:
		fmt.Printf("Extracted: %s %d %d\n", filePath, line, col)

	} else {
		fmt.Printf("No match found or regex failed to capture all components. Matches: %+v.\n", match)
	}

	resultLine, err := strconv.Atoi(lineStr)
	if err != nil {
		return "", 0, 0, fmt.Errorf("failed to convert line number '%s' to int: %w", lineStr, err)
	}

	resultCol, err := strconv.Atoi(colStr)
	if err != nil {
		return "", 0, 0, fmt.Errorf("failed to convert column number '%s' to int: %w", colStr, err)
	}

	return filePath, resultLine, resultCol, nil
}

// give the code snippet and symbol name and return the row number and col number of this symbol
func pickLineColNum(codeSnippet string, symbolName string) (int, int, error) {
	lines := strings.Split(codeSnippet, "\n")

	for i, line := range lines {
		lineNumber := i // Line numbers are 1-based

		colIndex := strings.Index(line, symbolName)

		if colIndex != -1 {
			columnNumber := colIndex + 1
			return lineNumber, columnNumber, nil
		}
	}

	// If the loop completes, the symbol was not found
	return 0, 0, fmt.Errorf("symbol '%s' not found in the code snippet", symbolName)
}

func recQueryfuncs(filePath string, funcName string, level int) ([]string, error) {
	if level == 0 {
		return nil, nil
	}

	// 1. get the function def
	code, linenum, err := getFunctionDefination(filePath, funcName)
	if err != nil {
		return nil, fmt.Errorf("getFunctionDefination error: %w", err)
	}

	functionChain := []string{code}

	// 2. pick those functions call inside (symbols)
	llmResp, err := callLLM(pickFuncCallInside + code)
	fmt.Printf("llmResp: %v\n", llmResp)
	funcSyms := []string{}
	if err := json.Unmarshal([]byte(llmResp), &funcSyms); err != nil {
		return nil, err
	}

	// 3. pick the symbols row, col
	for _, s := range funcSyms {
		r, c, err := pickLineColNum(code, s)
		if err != nil {
			return nil, err
		}

		// 4. ask gopls where are them
		fp, _, _, err := getGoplsDefinition(filePath, r+linenum, c)
		if err != nil {
			fmt.Printf("error when getGoplsDefinition: %+v\n", err)
		}

		// 5. call the next level
		nextChains, err := recQueryfuncs(fp, s, level-1)
		if err != nil {
			fmt.Printf("error when recQueryfuncs: %+v\n", err)
		}

		functionChain = append(functionChain, nextChains...)
	}

	return functionChain, nil
}

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run mygenerator/main.go <source_file> <function_name>")
		fmt.Println("Example: go run mygenerator/main.go ../target.go MyFunction")
		os.Exit(1)
	}

	sourceFilePath := os.Args[1]
	functionName := os.Args[2]

	// get the function code
	code, ln, err := getFunctionDefination(sourceFilePath, functionName)
	if err != nil {
		panic(err)
	} else {
		fmt.Printf("%+v, %+v\n", code, ln)
	}

	// call llm to pick the endpoint and the
	llmResp, err := callLLM(pickHandlersPrompt + code)
	if err != nil {
		panic(err)
	}

	fmt.Printf("llmResp: %+v\n", llmResp)

	// get all endpoints and functions
	endpoints := []struct {
		Endpoint string
		Funcname string
	}{}

	if err := json.Unmarshal([]byte(llmResp), &endpoints); err != nil {
		panic(err)
	}

	fmt.Printf("%+v\n", endpoints)

	for _, ep := range endpoints {
		fmt.Printf("start to doc the endpoint: %+v\n", ep)

		rn, cn, err := pickLineColNum(code, ep.Funcname)
		if err != nil {
			fmt.Printf("main err on pick handler funcion row and col: %+v\n", err)
			continue
		}

		fmt.Printf("func %v rn %v cn %v\n", ep.Endpoint, rn, cn)

		fp, _, _, err := getGoplsDefinition(sourceFilePath, rn+ln, cn)
		if err != nil {
			fmt.Printf("main err on getGoplsDefinition: %+v\n", err)
			continue
		}

		allCodes, err := recQueryfuncs(fp, ep.Funcname, 1)
		if err != nil {
			fmt.Printf("main err on recQueryfuncs: %+v\n", err)
			continue
		}

		doc, err := callLLM(fmt.Sprintf(thisEndpointDoc, ep.Endpoint) + strings.Join(allCodes, "\n\n"))
		if err != nil {
			fmt.Printf("main err on recQueryfuncs: %+v\n", err)
			continue
		}

		fmt.Printf("doc for endpoint %s:\n%s\n", ep.Endpoint, doc)
	}
}
