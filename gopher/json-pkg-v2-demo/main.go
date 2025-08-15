package main

import (
	json1 "encoding/json"
	"encoding/json/jsontext"
	"encoding/json/v2"
	json2 "encoding/json/v2" // need the env GOEXPERIMENT=jsonv2 2025/08/14
	"fmt"
	"io"
	"strings"
	"time"
)

type Person struct {
	Name string
	Age  int
}

func testMarshalWrite1() {
	alice := Person{Name: "Alice", Age: 25}
	out := new(strings.Builder) // io.Writer

	enc := json1.NewEncoder(out) // can replaced by json2 MarshalWrite
	enc.Encode(alice)            //

	fmt.Println("testMarshalWrite1: ", out.String())

	in := strings.NewReader(`{"Name":"Bob","Age":30}`) // io.Reader
	dec := json1.NewDecoder(in)
	var bob Person
	dec.Decode(&bob)
	fmt.Println("testMarshalWrite1: ", bob)
}

func testMarshalWrite2() {
	alice := Person{Name: "Alice", Age: 25}
	out := new(strings.Builder)

	// one line below replace the
	// enc := json1.NewEncoder(out)
	// enc.Encode(alice)
	json2.MarshalWrite(out, alice)

	fmt.Println("testMarshalWrite2: ", out.String())

	in := strings.NewReader(`{"Name":"Bob","Age":30}`)
	var bob Person
	json2.UnmarshalRead(in, &bob)
	fmt.Println("testMarshalWrite2: ", bob)
}

func testMarshalEncode() {
	people := []Person{
		{Name: "Alice", Age: 25},
		{Name: "Bob", Age: 30},
		{Name: "Cindy", Age: 15},
	}
	out := new(strings.Builder)
	enc := jsontext.NewEncoder(out) // make encoder like this

	for _, p := range people {
		json2.MarshalEncode(enc, p)
	}

	fmt.Print("testMarshalEncode: ", out.String())

	in := strings.NewReader(`
    {"Name":"Alice","Age":25}
    {"Name":"Bob","Age":30}
    {"Name":"Cindy","Age":15}
`)
	dec := jsontext.NewDecoder(in)

	for {
		var p Person
		// Decodes one Person object per call.
		err := json2.UnmarshalDecode(dec, &p)
		if err == io.EOF {
			break
		}
		fmt.Println("testMarshalEncode: ", p)
	}
}

func testMarshalOptions() {
	alice := Person{Name: "Alice", Age: 25}
	b, _ := json2.Marshal(
		alice,
		json2.OmitZeroStructFields(true),
		json2.StringifyNumbers(true),

		// below three options make json2 behavior as same as json 1
		json2.FormatNilMapAsNull(true),   // new option
		json2.FormatNilSliceAsNull(true), // new option
		jsontext.Multiline(true),         // new option

		jsontext.WithIndent("  "),
	)
	fmt.Println(string(b))
}

type Person2 struct {
	Name string `json:"name"`

	BirthDate time.Time `json:"birth_date,format:DateOnly"`

	// this address will go root level
	Address `json:",inline"`

	// more data here
	Data map[string]any `json:",unknown"`
}

type Address struct {
	Street string `json:"street"`
	City   string `json:"city"`
}

func testInlineAndFormat() {
	alice := Person2{
		Name:      "Alice",
		BirthDate: time.Date(2001, 7, 15, 12, 35, 43, 0, time.UTC),
		Address: Address{
			Street: "123 Main St",
			City:   "Wonderland",
		},
	}
	b, _ := json2.Marshal(alice, jsontext.WithIndent("  "))
	fmt.Println("testInlineAndFormat: ", string(b))
}

func testInlineAndFormat2() {
	src := `{
        "name": "Alice",
        "hobby": "adventure",
        "friends": [
            {"name": "Bob"},
            {"name": "Cindy"}
        ]
    }`
	var alice Person2
	json2.Unmarshal([]byte(src), &alice,
		json.MatchCaseInsensitiveNames(true), // new option make json2 to json1
	)
	fmt.Printf("testInlineAndFormat2:\n%+v\n", alice)
}

/// the custom marshalling testing below

// func MarshalFunc[T any](fn func(T) ([]byte, error)) *Marshalers
// func UnmarshalFunc[T any](fn func([]byte, T) error) *Unmarshalers

func testCustomMarshal() {
	boolMarshaler := json2.MarshalFunc(
		func(val bool) ([]byte, error) {
			if val {
				return []byte(`"✓"`), nil
			}
			return []byte(`"✗"`), nil
		},
	)

	val := true
	data, err := json.Marshal(val, json.WithMarshalers(boolMarshaler))
	fmt.Println("testCustomMarshal: ", string(data), err)

	//

	boolUnmarshaler := json.UnmarshalFunc(
		func(data []byte, val *bool) error {
			*val = string(data) == `"✓"`
			return nil
		},
	)

	val = false
	src := []byte(`"✓"`)
	err = json2.Unmarshal(src, &val, json.WithUnmarshalers(boolUnmarshaler))
	fmt.Println("testCustomMarshal: ", val, err)
}

// func MarshalToFunc[T any](fn func(*jsontext.Encoder, T) error) *Marshalers
// func UnmarshalFromFunc[T any](fn func(*jsontext.Decoder, T) error) *Unmarshalers

func testCustomMarshalTo() {
	boolMarshaler := json2.MarshalToFunc(
		func(enc *jsontext.Encoder, val bool) error {
			if val {
				return enc.WriteToken(jsontext.String("✓"))
			}
			return enc.WriteToken(jsontext.String("✗"))
		},
	)

	strMarshaler := json2.MarshalToFunc(
		func(enc *jsontext.Encoder, val string) error {
			if val == "on" || val == "true" {
				return enc.WriteToken(jsontext.String("✓"))
			}
			if val == "off" || val == "false" {
				return enc.WriteToken(jsontext.String("✗"))
			}
			return json.SkipFunc
		},
	)

	marshalers := json.JoinMarshalers(boolMarshaler, strMarshaler)

	vals := []any{true, "off", "hello", 1} // no int marshaler, just use default

	data, err := json.Marshal(vals, json.WithMarshalers(marshalers))
	fmt.Println("testCustomMarshalTo: ", string(data), err)
}

func main() {
	testInlineAndFormat()
	testInlineAndFormat2()

	testCustomMarshal()

	testCustomMarshalTo()
}
