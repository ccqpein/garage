package main

import (
	"fmt"
	"reflect"
	"reflectTypeFromOtherPackage/A"
)

func main() {
	a := A.As{}
	fmt.Println(reflect.TypeOf(a).Name())
	fmt.Println(reflect.TypeOf(a).PkgPath())
}
