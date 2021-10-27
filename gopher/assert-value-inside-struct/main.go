package main

import (
	"encoding/json"
	"fmt"
)

// result.Reason, ok = reason.([]string)
type Result struct {
	Inner []string
}

func main() {
	var a interface{} = []string{"a", "b"}

	result := Result{}
	ok := false

	result.Inner, ok = a.([]string)
	if !ok {
		panic("not ok")
	}

	fmt.Println(result.Inner)

	//////////////////

	var b = []byte(`{"Reason": ["a", "b", "c"]}`)
	cache := map[string]interface{}{}
	if err := json.Unmarshal(b, &cache); err != nil {
		panic(err)
	}

	//fmt.Printf("%+v\n", cache)
	reason, ok := cache["Reason"]
	if !ok {
		panic("not ok")
	}

	//fmt.Printf("%+T\n", reason)
	// result.Inner, ok = reason.([]string)  // cannot go to []string from []interface directly
	// if !ok {
	// 	panic("not ok")
	// }

	// if json.Unmarshal(reason.([]byte), &result.Inner) != nil {
	// 	panic("directly json unmarshal failed too")
	// }

	for _, v := range reason.([]interface{}) {
		if s, ok := v.(string); ok {
			result.Inner = append(result.Inner, s)
		} else {
			panic("again")
		}
	}

	fmt.Println(result.Inner)
}
