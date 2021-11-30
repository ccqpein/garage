package main

import (
	"log"
	//"os"
	"fmt"
	"strings"
	"text/template"
	"text/template/parse"
)

func main() {
	// First we create a FuncMap with which to register the function.
	funcMap := template.FuncMap{
		// The name "title" is what the function will be called in the template text.
		"title": strings.Title,
	}

	// A simple template definition to test our function.
	// We print the input text several ways:
	// - the original
	// - title-cased
	// - title-cased and then printed with %q
	// - printed with %q and then title-cased.
	const templateText = `
Input: {{printf "%q" .}}
Output 0: {{title 10 1}}
Output 1: {{title . | printf "%q"}}
Output 2: {{printf "%q" . | title}}
`

	// Create a template, add the function map, and parse the text.
	tmpl, err := template.New("titleTest").Funcs(funcMap).Parse(templateText)
	if err != nil {
		log.Fatalf("parsing: %s", err)
	}

	fmt.Printf("tree: %+v\n", tmpl.Tree)
	fmt.Printf("list node in tree: %+v\n", tmpl.Tree.Root)
	for _, n := range tmpl.Tree.Root.Nodes {
		fmt.Printf("node inside root: %+v\n", n)
		fmt.Printf("node type : %+v\n", n.Type())
		switch n.Type() {
		case 1:
			fmt.Print("it is action node\n")
			nn := n.(*parse.ActionNode)
			fmt.Printf("action node pipe: %+v, decl: %+v, cmds: %+v\n", nn.Pipe, nn.Pipe.Decl, nn.Pipe.Cmds)
			for i, c := range nn.Pipe.Cmds {
				fmt.Printf("%dth command: %+v\n", i, c)
			}
		}
		println()
	}

	// Run the template to verify the output.
	//err = tmpl.Execute(os.Stdout, "the go programming language")
	if err != nil {
		log.Fatalf("execution: %s", err)
	}

}
