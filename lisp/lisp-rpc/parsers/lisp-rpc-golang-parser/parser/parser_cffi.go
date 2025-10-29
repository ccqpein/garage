package parser

/*
#cgo LDFLAGS: -L. -llisp_rpc_rust_parser
#include "parser_lib.h"
#include <stdlib.h>
*/
import "C"
import "unsafe"

// Wrapper for CStringArray to manage memory in Go
type GoStringArray struct {
	Ptr *C.CStringArray
}

func (gsa *GoStringArray) Free() {
	if gsa.Ptr != nil {
		C.cstring_array_free(*gsa.Ptr)
		gsa.Ptr = nil
	}
}

// Helper to convert CStringArray to Go []string
func (gsa *GoStringArray) ToStrings() []string {
	if gsa.Ptr == nil || gsa.Ptr.strings == nil {
		return nil
	}
	length := int(gsa.Ptr.len)
	goStrings := make([]string, length)
	// Access the C array of char* pointers
	cStringsSlice := (*[1 << 30]*C.char)(unsafe.Pointer(gsa.Ptr.strings))[:length:length]

	for i, cstr := range cStringsSlice {
		goStrings[i] = C.GoString(cstr)
		// NOTE: Individual strings within CStringArray are freed by cstring_array_free
		// Do not call C.free on cstr here.
	}
	return goStrings
}

// Wrapper for CExprArray to manage memory in Go
type GoExprArray struct {
	Ptr *C.CExprArray
}
