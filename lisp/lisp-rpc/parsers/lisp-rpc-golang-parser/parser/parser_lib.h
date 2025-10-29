#ifndef PARSER_LIB_H
#define PARSER_LIB_H

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>


// Opaque structs
typedef struct Parser Parser;
typedef struct Atom Atom;
typedef struct Expr Expr;

// Rust enum mapping for TypeValue
typedef enum CTypeValueType {
  Symbol = 0,
  String = 1,
  Keyword = 2,
  Number = 3,
} CTypeValueType;

// Rust enum mapping for ParserError
typedef enum CParserErrorCode {
  Success = 0,
  InvalidStart = 1,
  InvalidToken = 2,
  UnknownToken = 3,
  NullPointer = 4,
  SerializationError = 5,
  InternalError = 6,
} CParserErrorCode;

// Rust enum mapping for ExprType
typedef enum CExprType {
  ExprTypeAtom = 0,
  List = 1,
  Quote = 2,
} CExprType;

// C-compatible struct for returning an array of C-strings
typedef struct CStringArray {
  char **strings;
  int32_t len;
  CParserErrorCode error_code;
} CStringArray;

// C-compatible struct for returning an array of opaque Expr pointers
typedef struct CExprArray {
  Expr **exprs;
  int32_t len;
  CParserErrorCode error_code;
} CExprArray;


#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

// Parser functions
Parser *parser_new(void);
void parser_free(Parser *ptr);
void parser_config_read_number(Parser *ptr, bool v);
CStringArray parser_tokenize(Parser *parser_ptr, const char *source_code);
CExprArray parser_parse_root(Parser *parser_ptr, const char *source_code);

// Atom functions (creation, accessors, destructor)
Atom *atom_new_symbol(const char *s);
Atom *atom_new_string(const char *s);
Atom *atom_new_keyword(const char *s);
Atom *atom_new_number(int64_t n);
void atom_free(Atom *ptr);
CTypeValueType atom_get_type(const Atom *ptr);
char *atom_get_string_value(const Atom *ptr); // Returns NULL if not string-like
int64_t atom_get_number_value(const Atom *ptr); // Returns 0 if not number
char *atom_to_string(const Atom *ptr); // Full Atom string representation

// Expr functions (accessors, destructor)
void expr_free(Expr *ptr);
CExprType expr_get_type(const Expr *ptr);
Atom *expr_get_atom(const Expr *ptr); // Returns NULL if not an Atom. Do not free.
int32_t expr_list_len(const Expr *ptr); // Returns -1 if not a List
Expr *expr_list_get_element(const Expr *ptr, int32_t index); // Returns NULL if not List or out of bounds. Do not free.
Expr *expr_get_quoted_expr(const Expr *ptr); // Returns NULL if not a Quote. Do not free.
char *expr_to_string(const Expr *ptr); // Full Expr string representation

// General memory management
void string_free(char *s);
void cstring_array_free(CStringArray array);
void cexpr_array_free(CExprArray array);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif /* YOUR_PARSER_LIB_H */
