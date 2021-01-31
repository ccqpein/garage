package main

// import "C"
import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"os"
)

type Method struct {
	Name         string   `json:"name"`
	Doc          string   `json:"doc"`
	Types        []string `json:"types"`
	Parameters   []string `json:"parameters"`
	Requireds    []string `json:"requireds"`
	Descriptions []string `json:"descriptions"`
}

// type (
// 	C_Method C.struct_Method
// )

func ParseMethods(b []byte) []Method {
	result := []Method{}

	if err := json.Unmarshal(b, &result); err != nil {
		panic(err)
	}

	return result
}

// func ParseMethodsFromSO(b string) []Method {
// 	return ([]Method)(C.json_parser(C.CString(b)))
// }

func main() {
	body := []byte{}
	if f, err := os.Open("./methods.json"); err != nil {
		panic(err)
	} else {
		body, err = ioutil.ReadAll(f)
	}

	fmt.Println(len(ParseMethods(body)))
}
