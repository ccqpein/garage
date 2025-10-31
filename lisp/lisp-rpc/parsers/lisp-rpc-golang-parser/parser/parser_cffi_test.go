package parser

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestTokenize(t *testing.T) {
	testcase0 := "(a (add a b) (+ a b)) (add 10 20) 'quoted-symbol :keyword"
	p := New().ConfigReadNumber(true)
	defer p.Free()

	tokens, err := p.Tokenize(testcase0)
	assert.NoError(t, err, "token should be fine")
	assert.Equal(t, []string{"(", "a", " ", "(", "add", " ", "a", " ", "b", ")", " ", "(", "+", " ", "a", " ", "b", ")", ")", " ", "(", "add", " ", "10", " ", "20", ")", " ", "'", "quoted-symbol", " ", ":", "keyword"}, tokens, "should be same")
}

func TestParse(t *testing.T) {
	testcase0 := `(def-msg language-perfer :lang 'string)

(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)`
	p := New().ConfigReadNumber(true)
	defer p.Free()

	exprs, err := p.Parse(testcase0)
	assert.NoError(t, err, "shouldnt have error")

	e, err := exprs[0].ToString()
	assert.NoError(t, err, "shouldnt have error")
	fmt.Printf("%+v\n", e)

}
