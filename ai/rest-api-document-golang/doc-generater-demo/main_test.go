package main

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPickLineColNum(t *testing.T) {
	data := `func registerRoutes() {
	http.HandleFunc("/health", healthCheckHandler)
	http.HandleFunc("/products", productsHandler) // Use a multiplexer for /products and /products/{id}
	http.HandleFunc("/products/", productsHandler)
	http.HandleFunc("/products", productsHandler) // Use a multiplexer for /products and /products/{id}
}`

	r, c, e := pickLineColNum(data, "productsHandler")
	assert.NoError(t, e, "")
	assert.Equal(t, 2, r, "")
	assert.Equal(t, 31, c, "")
}

func TestGoplsOutputRegex(t *testing.T) {
	re := regexp.MustCompile(`(?s)^(.+?):(\d+):(\d+)(?:-\d+)?.*$`)

	case0 := `/demo-server/main.go:180:6-21: defined here as func productsHandler(w http.ResponseWriter, r *http.Request)
productsHandler acts as a simple router for /products and /products/{id}.`

	match := re.FindStringSubmatch(case0)
	t.Log(match)
}

func TestGetFunctionDefination(t *testing.T) {

	case0 := `func a(x int, y int) {
}`

	//regexPattern := fmt.Sprintf(`(?s)^func(?:\s*\(([^)]+)\))?\s+(%s\w*)\s*\([^)]*\)(?:\s*(?:[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*|\([^)]*\)))?\s*\{(?s:.*?)\}`, regexp.QuoteMeta("a"))
	regexPattern := fmt.Sprintf(`func(?:\s*\((?P<receiver>[^)]+)\))?\s+(%s\w*)\s*\([^)]*\)(?:\s*(?:[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*|\([^)]*\)))?\s*\{(?P<body>[\s\S]*?)\n\}\s*(?:$|\n)`, regexp.QuoteMeta("a"))
	re := regexp.MustCompile(regexPattern)

	matches := re.FindStringSubmatch(string(case0))

	t.Log(matches[0])

	case1 := `func registerRoutes2() {
	http.HandleFunc("/health", healthCheckHandler)
	http.HandleFunc("/products", productsHandler)
	http.HandleFunc("/products/", productsHandler)
}`

	regexPattern = fmt.Sprintf(`func(?:\s*\((?P<receiver>[^)]+)\))?\s+(%s\w*)\s*\([^)]*\)(?:\s*(?:[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*|\([^)]*\)))?\s*\{(?P<body>[\s\S]*?)\n\}\s*(?:$|\n)`, regexp.QuoteMeta("registerRoutes2"))
	//regexPattern = fmt.Sprintf(`(?s)^func(?:\s*\(([^)]+)\))?\s+(%s\w*)\s*\([^)]*\)(?:\s*(?:[a-zA-Z_]\w*(?:\s*,\s*[a-zA-Z_]\w*)*|\([^)]*\)))?\s*\{(?s:.*?)\}`, regexp.QuoteMeta("registerRoutes2"))
	re = regexp.MustCompile(regexPattern)
	matches = re.FindStringSubmatch(string(case1))

	t.Log(matches[0])

}
