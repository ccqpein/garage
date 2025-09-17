// mygenerator/main.go
package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("Usage: go run mygenerator/main.go <source_file> <function_name>")
		fmt.Println("Example: go run mygenerator/main.go ../target.go MyFunction")
		os.Exit(1)
	}

	sourceFilePath := os.Args[1]
	functionName := os.Args[2]

	sourceCode, err := os.ReadFile(sourceFilePath)
	if err != nil {
		log.Fatalf("Error reading source file %s: %v", sourceFilePath, err)
	}

	regexPattern := fmt.Sprintf(`func\s+%s\s*\(.*?\)\s*\{\n(?s:.*?)\n\s*\}`, regexp.QuoteMeta(functionName))
	//fmt.Println("regexPattern: ", regexPattern)

	re := regexp.MustCompile(regexPattern)

	matches := re.FindStringSubmatch(string(sourceCode))

	if len(matches) > 0 {
		fmt.Printf("--- Extracted Function '%s' from %s ---\n", functionName, sourceFilePath)
		fmt.Println(matches[0]) // matches[0] is the whole string matched by the regex
	} else {
		fmt.Printf("Function '%s' not found or did not match expected pattern in %s\n", functionName, sourceFilePath)
	}
}
