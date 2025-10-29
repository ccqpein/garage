package parser

/*
#cgo LDFLAGS: -L. -llisp_rpc_rust_parser
#include "parser_lib.h"
#include <stdlib.h>
*/
import "C"
import (
	"fmt"
	"unsafe"
)

// Wrapper for CStringArray to manage memory in Go
type goStringArray struct {
	Ptr *C.CStringArray
}

func (gsa *goStringArray) free() {
	if gsa.Ptr != nil {
		C.cstring_array_free(*gsa.Ptr)
		gsa.Ptr = nil
	}
}

// Helper to convert CStringArray to Go []string
func (gsa *goStringArray) toStrings() []string {
	if gsa.Ptr == nil || gsa.Ptr.strings == nil {
		return nil
	}
	length := int(gsa.Ptr.len)
	goStrings := make([]string, length)
	cStringsSlice := (*[1 << 30]*C.char)(unsafe.Pointer(gsa.Ptr.strings))[:length:length]

	for i, cstr := range cStringsSlice {
		goStrings[i] = C.GoString(cstr)
	}
	return goStrings
}

// Wrapper for CExprArray to manage memory in Go
type goExprArray struct {
	Ptr *C.CExprArray
}

func (gea *goExprArray) free() {
	if gea.Ptr != nil {
		C.cexpr_array_free(*gea.Ptr)
		gea.Ptr = nil
	}
}

func (gea *goExprArray) toGoExprs() []*C.Expr {
	if gea.Ptr == nil || gea.Ptr.exprs == nil {
		return nil
	}
	length := int(gea.Ptr.len)
	goExprs := make([]*C.Expr, length)
	cExprsSlice := (*[1 << 30]*C.Expr)(unsafe.Pointer(gea.Ptr.exprs))[:length:length]
	copy(goExprs, cExprsSlice)
	return goExprs
}

type Parser struct {
	innerParser *C.Parser
	readNumber  bool
}

func New() *Parser {
	return &Parser{
		innerParser: C.parser_new(),
	}
}

func (p *Parser) ConfigReadNumber(b bool) *Parser {
	C.parser_config_read_number(p.innerParser, C.bool(b))
	return p
}

func (p *Parser) Free() {
	C.parser_free(p.innerParser)
}

func (p *Parser) Tokenize(source string) ([]string, error) {
	cSourceCode := C.CString(source)
	defer C.free(unsafe.Pointer(cSourceCode))

	cTokenArray := C.parser_tokenize(p.innerParser, cSourceCode)
	tokenArray := &goStringArray{Ptr: &cTokenArray}
	defer tokenArray.free()

	if tokenArray.Ptr.error_code != C.Success {
		return nil, fmt.Errorf("Error tokenizing: %v\n", tokenArray.Ptr.error_code)

	}
	return tokenArray.toStrings(), nil
}

func (p *Parser) Parse(source string) ([]*Expr, error) {
	cSourceCode := C.CString(source)
	defer C.free(unsafe.Pointer(cSourceCode))

	cExprArray := C.parser_parse_root(p.innerParser, cSourceCode)
	exprArray := &goExprArray{Ptr: &cExprArray}
	defer exprArray.free()

	if exprArray.Ptr.error_code != C.Success {
		return nil, fmt.Errorf("Error parsing: %v\n", exprArray.Ptr.error_code)
	}

	exprs := []*Expr{}

	for _, e := range exprArray.toGoExprs() {
		exprs = append(exprs, FromCExpr(e))
	}

	return exprs, nil
}

// C-enum mappings for Go
const (
	AtomTypeSymbol  C.CTypeValueType = C.Symbol
	AtomTypeString  C.CTypeValueType = C.String
	AtomTypeKeyword C.CTypeValueType = C.Keyword
	AtomTypeNumber  C.CTypeValueType = C.Number
)

const (
	ParserErrorSuccess       C.CParserErrorCode = C.Success
	ParserErrorInvalidStart  C.CParserErrorCode = C.InvalidStart
	ParserErrorInvalidToken  C.CParserErrorCode = C.InvalidToken
	ParserErrorUnknownToken  C.CParserErrorCode = C.UnknownToken
	ParserErrorNullPointer   C.CParserErrorCode = C.NullPointer
	ParserErrorSerialization C.CParserErrorCode = C.SerializationError
	ParserErrorInternalError C.CParserErrorCode = C.InternalError
)

const (
	ExprTypeAtom  C.CExprType = C.ExprTypeAtom
	ExprTypeList  C.CExprType = C.List
	ExprTypeQuote C.CExprType = C.Quote
)

type Atom struct {
	innerAtom *C.Atom
}

func FromCAtom(cAtom *C.Atom) *Atom {
	if cAtom == nil {
		return nil
	}
	return &Atom{innerAtom: cAtom}
}

func (a *Atom) Free() {
	if a.innerAtom != nil {
		C.atom_free(a.innerAtom)
		a.innerAtom = nil
	}
}

func NewAtomSymbol(s string) *Atom {
	cStr := C.CString(s)
	defer C.free(unsafe.Pointer(cStr))
	return &Atom{innerAtom: C.atom_new_symbol(cStr)}
}

func NewAtomString(s string) *Atom {
	cStr := C.CString(s)
	defer C.free(unsafe.Pointer(cStr))
	return &Atom{innerAtom: C.atom_new_string(cStr)}
}

func NewAtomKeyword(s string) *Atom {
	cStr := C.CString(s)
	defer C.free(unsafe.Pointer(cStr))
	return &Atom{innerAtom: C.atom_new_keyword(cStr)}
}

func NewAtomNumber(n int64) *Atom {
	return &Atom{innerAtom: C.atom_new_number(C.int64_t(n))}
}

func (a *Atom) GetType() C.CTypeValueType {
	if a.innerAtom == nil {
		return ^C.CTypeValueType(0) // Return an invalid type
	}
	return C.atom_get_type(a.innerAtom)
}

func (a *Atom) GetStringValue() (string, error) {
	if a.innerAtom == nil {
		return "", fmt.Errorf("Atom is nil")
	}
	cStr := C.atom_get_string_value(a.innerAtom)
	if cStr == nil {
		return "", fmt.Errorf("Atom is not a string-like type")
	}

	return C.GoString(cStr), nil
}

func (a *Atom) GetNumberValue() (int64, error) {
	if a.innerAtom == nil {
		return 0, fmt.Errorf("Atom is nil")
	}
	if C.atom_get_type(a.innerAtom) != C.Number {
		return 0, fmt.Errorf("Atom is not a number type")
	}
	return int64(C.atom_get_number_value(a.innerAtom)), nil
}

func (a *Atom) ToString() (string, error) {
	if a.innerAtom == nil {
		return "", fmt.Errorf("Atom is nil")
	}
	cStr := C.atom_to_string(a.innerAtom)
	if cStr == nil {
		return "", fmt.Errorf("Failed to convert Atom to string")
	}
	defer C.string_free(cStr) // Free the C string returned by atom_to_string
	return C.GoString(cStr), nil
}

type Expr struct {
	innerExpr *C.Expr
}

func FromCExpr(cExpr *C.Expr) *Expr {
	if cExpr == nil {
		return nil
	}
	return &Expr{innerExpr: cExpr}
}

// Free deallocates the underlying C Expr.
func (e *Expr) Free() {
	if e.innerExpr != nil {
		C.expr_free(e.innerExpr)
		e.innerExpr = nil
	}
}

// GetType returns the type of the Expr.
func (e *Expr) GetType() C.CExprType {
	if e.innerExpr == nil {
		return ^C.CExprType(0) // Return an invalid type
	}
	return C.expr_get_type(e.innerExpr)
}

// GetAtom returns the Atom if the Expr is of type ExprTypeAtom.
// The returned Atom's underlying C pointer is owned by the Expr and should not be
// freed separately.
func (e *Expr) GetAtom() *Atom {
	if e.innerExpr == nil || C.expr_get_type(e.innerExpr) != C.ExprTypeAtom {
		return nil
	}
	return FromCAtom(C.expr_get_atom(e.innerExpr))
}

// ListLen returns the length of the list if the Expr is of type List.
// Returns -1 if not a List or if the inner expr is nil.
func (e *Expr) ListLen() (int32, error) {
	if e.innerExpr == nil {
		return -1, fmt.Errorf("Expr is nil")
	}
	length := C.expr_list_len(e.innerExpr)
	if length == -1 {
		return -1, fmt.Errorf("Expr is not a List type")
	}
	return int32(length), nil
}

// ListGetElement returns the element at the given index if the Expr is a List.
// The returned Expr's underlying C pointer is owned by the parent List Expr and
// should not be freed separately. Returns nil if not a List or index is out of bounds.
func (e *Expr) ListGetElement(index int32) *Expr {
	if e.innerExpr == nil || C.expr_get_type(e.innerExpr) != C.List {
		return nil
	}
	return FromCExpr(C.expr_list_get_element(e.innerExpr, C.int32_t(index)))
}

// GetQuotedExpr returns the quoted Expr if the Expr is of type Quote.
// The returned Expr's underlying C pointer is owned by the parent Quote Expr and
// should not be freed separately. Returns nil if not a Quote.
func (e *Expr) GetQuotedExpr() *Expr {
	if e.innerExpr == nil || C.expr_get_type(e.innerExpr) != C.Quote {
		return nil
	}
	return FromCExpr(C.expr_get_quoted_expr(e.innerExpr))
}

// ToString returns the full string representation of the Expr.
// Call string_free on the returned char* in C.
func (e *Expr) ToString() (string, error) {
	if e.innerExpr == nil {
		return "", fmt.Errorf("Expr is nil")
	}
	cStr := C.expr_to_string(e.innerExpr)
	if cStr == nil {
		return "", fmt.Errorf("Failed to convert Expr to string")
	}
	defer C.string_free(cStr) // Free the C string returned by expr_to_string
	return C.GoString(cStr), nil
}
