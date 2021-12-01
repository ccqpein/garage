package main

import (

	//"os"

	"fmt"
	"log"
	"strings"
	"text/template"
	"text/template/parse"
)

func main() {
	// First we create a FuncMap with which to register the function.
	funcMap := template.FuncMap{
		// The name "title" is what the function will be called in the template text.
		"title": strings.Title,
		"a": func(a int) int {
			return a + 1
		},
		"add": func(a int) int {
			return a + 1
		},
		"filter": func() int {
			return 1
		},
		"any": func() int {
			return 0
		},
	}

	// A simple template definition to test our function.
	// We print the input text several ways:
	// - the original
	// - title-cased
	// - title-cased and then printed with %q
	// - printed with %q and then title-cased.
	/*const templateText = `
	Input: {{printf "%q" .}}
	Output 0: {{title .}}
	Output 1: {{title . | printf "%q"}}
	Output 2: {{printf "%q" . | title}}
	Output 3: {{ $a := (add 10) }}
	Output 4: {{ $result := (filter "{{ and (eq .a 5) (eq .b 5) }}" .somewraper) | any }}
	Output 5: {{ $result := filter "{{ and (eq .a 5) (eq .b 5) }}" .somewraper }}
	`

		// Create a template, add the function map, and parse the text.
		tmpl, err := template.New("titleTest").Funcs(funcMap).Parse(templateText)
		if err != nil {
			log.Fatalf("parsing: %s", err)
		}

		fmt.Printf("tree: {%+v}\n", tmpl.Tree)
		fmt.Printf("list node in tree: {%+v}\n", tmpl.Tree.Root)
		for _, n := range tmpl.Tree.Root.Nodes {
			//fmt.Printf("node inside root: %+v\n", n)
			fmt.Printf("node type : %+v\n", n.Type())
			switch n.(type) {
			case *parse.ActionNode:
				fmt.Print("it is action node\n")
				nn := n.(*parse.ActionNode)
				fmt.Printf("action node pipe: %+v, decl: %+v, cmds: %+v\n", nn.Pipe, nn.Pipe.Decl, nn.Pipe.Cmds)
				for i, c := range nn.Pipe.Cmds {
					fmt.Printf("%dth command: %+v and arg: %+v\n", i, c, c.Args)
				}
			default:
				fmt.Print("it is not action node\n")
				nn := n.(*parse.TextNode)
				fmt.Println(string(nn.Text))
			}

			println()
		}

		// Run the template to verify the output.
		//err = tmpl.Execute(os.Stdout, "the go programming language")
		if err != nil {
			log.Fatalf("execution: %s", err)
		}*/

	const templateText = `
	Output 0: {{ $result := (filter "{{ and (eq .a 5) (eq .b 5) }}" .somewraper) | any }}
	Output 1: {{ $result := filter "{{ and (eq .a 5) (eq .b 5) }}" .somewraper }}
	`

	tmpl, err := template.New("titleTest").Funcs(funcMap).Parse(templateText)
	if err != nil {
		log.Fatalf("parsing: %s", err)
	}

	fmt.Println("root node type: ", tmpl.Root.NodeType) // NodeNumber?

	fmt.Println("root node tree root: ", tmpl.Tree.Root)

	listnode := tmpl.Tree.Root
	fmt.Println("listnode type: ", listnode.Type()) // 11, NodeNumber?

	for i, no := range listnode.Nodes {
		fmt.Printf("%dth node type is: %v, value is %s\n", i, no.Type(), no.String())

		switch nn := no.(type) {
		case *parse.ActionNode:
			fmt.Printf("this action node pipe decl: %v, cmds:\n", nn.Pipe.Decl)
			for i, cm := range nn.Pipe.Cmds {
				fmt.Println(i, ": ", cm)
				fmt.Println("type is: ", cm.Type()) // 4 command node
				for i, n := range cm.Args {
					fmt.Printf("%dth arg type is: %v, value is: %v\n", i, n.Type(), n.String())
					tmpl_inner, err := template.New("titleTest").Funcs(funcMap).Parse(n.String())
					if err != nil {
						log.Fatalf("parsing: %s", err)
					}
					println("re parse type is:", tmpl_inner.Root.Type())
					fmt.Printf("first inside values are: %+v\n", tmpl_inner.Root.Nodes[0])
				}
			}
		}
		println("---------------")
	}
}
