use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_longlong}; // c_longlong for i64

//
// =========== FFI bleow (gemini) ================
//

// C-compatible enum for TypeValue discrimination
#[repr(C)]
pub enum CTypeValueType {
    Symbol = 0,
    String = 1,
    Keyword = 2,
    Number = 3,
}

// --- FFI functions for Atom ---

/// Creates an Atom from a symbol string.
#[unsafe(no_mangle)]
pub extern "C" fn atom_new_symbol(s: *const c_char) -> *mut Atom {
    let go_string = unsafe { CStr::from_ptr(s).to_string_lossy().into_owned() };
    Box::into_raw(Box::new(Atom::read(&go_string)))
}

/// Creates an Atom from a string literal.
#[unsafe(no_mangle)]
pub extern "C" fn atom_new_string(s: *const c_char) -> *mut Atom {
    let go_string = unsafe { CStr::from_ptr(s).to_string_lossy().into_owned() };
    Box::into_raw(Box::new(Atom::read_string(&go_string)))
}

/// Creates an Atom from a keyword string.
#[unsafe(no_mangle)]
pub extern "C" fn atom_new_keyword(s: *const c_char) -> *mut Atom {
    let go_string = unsafe { CStr::from_ptr(s).to_string_lossy().into_owned() };
    Box::into_raw(Box::new(Atom::read_keyword(&go_string)))
}

/// Creates an Atom from a number.
#[unsafe(no_mangle)]
pub extern "C" fn atom_new_number(n: c_longlong) -> *mut Atom {
    Box::into_raw(Box::new(Atom::read_number("", n)))
}

/// Frees an Atom instance.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Atom` previously created by `atom_new_*`
/// or returned by an FFI function.
#[unsafe(no_mangle)]
pub extern "C" fn atom_free(ptr: *mut Atom) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}

/// Returns the TypeValue type of the Atom.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Atom`.
#[unsafe(no_mangle)]
pub extern "C" fn atom_get_type(ptr: *const Atom) -> CTypeValueType {
    if ptr.is_null() {
        return CTypeValueType::Symbol; // or handle error appropriately, e.g., panicking or returning an error code
    }
    let atom = unsafe { &*ptr };
    match atom.value {
        TypeValue::Symbol(_) => CTypeValueType::Symbol,
        TypeValue::String(_) => CTypeValueType::String,
        TypeValue::Keyword(_) => CTypeValueType::Keyword,
        TypeValue::Number(_) => CTypeValueType::Number,
    }
}

/// Returns the string representation of the Atom's value (Symbol, String, Keyword).
/// Returns NULL if the Atom is a Number.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Atom`.
/// The returned `*mut c_char` must be freed using `string_free`.
#[unsafe(no_mangle)]
pub extern "C" fn atom_get_string_value(ptr: *const Atom) -> *mut c_char {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let atom = unsafe { &*ptr };
    let rust_string = match &atom.value {
        TypeValue::Symbol(s) => s.clone(),
        TypeValue::String(s) => s.clone(),
        TypeValue::Keyword(s) => s.clone(),
        TypeValue::Number(_) => {
            return std::ptr::null_mut(); // Numbers don't have a string value in this context
        }
    };
    CString::new(rust_string).unwrap_or_default().into_raw()
}

/// Returns the numeric value of the Atom.
/// Returns 0 if the Atom is not a Number. You might want to add an error indicator.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Atom`.
#[unsafe(no_mangle)]
pub extern "C" fn atom_get_number_value(ptr: *const Atom) -> c_longlong {
    if ptr.is_null() {
        return 0; // Error or invalid state
    }
    let atom = unsafe { &*ptr };
    match atom.value {
        TypeValue::Number(n) => n,
        _ => 0, // Not a number
    }
}

/// Returns the full string representation of the Atom as per its `to_string` implementation.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Atom`.
/// The returned `*mut c_char` must be freed using `string_free`.
#[unsafe(no_mangle)]
pub extern "C" fn atom_to_string(ptr: *const Atom) -> *mut c_char {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let atom = unsafe { &*ptr };
    CString::new(atom.to_string())
        .unwrap_or_default()
        .into_raw()
}

// Universal string free function
#[unsafe(no_mangle)]
pub extern "C" fn string_free(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        let _ = CString::from_raw(s);
    }
}

// --- FFI functions for Atom End ---

// C-compatible error codes
#[repr(C)]
#[derive(Debug, PartialEq, Eq)]
pub enum CParserErrorCode {
    Success = 0,
    InvalidStart = 1,
    InvalidToken = 2,
    UnknownToken = 3,
    NullPointer = 4,        // Added for FFI safety
    SerializationError = 5, // For string conversions
    InternalError = 6,      // General catch-all
}

// Global buffer for the last error message

// C-compatible enum for Expr discrimination
#[repr(C)]
pub enum CExprType {
    ExprTypeAtom = 0,
    List = 1,
    Quote = 2,
}

// --- FFI functions for Expr ---

/// Frees an Expr instance and all its nested children.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
/// This function must be called exactly once for each `Expr` created or returned by FFI.
#[unsafe(no_mangle)]
pub extern "C" fn expr_free(ptr: *mut Expr) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(ptr); // Drops the Box, which recursively frees contents
    }
}

/// Returns the type of the Expr (Atom, List, Quote).
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
#[unsafe(no_mangle)]
pub extern "C" fn expr_get_type(ptr: *const Expr) -> CExprType {
    if ptr.is_null() {
        return CExprType::ExprTypeAtom; // Default or error value
    }
    let expr = unsafe { &*ptr };
    match expr {
        Expr::Atom(_) => CExprType::ExprTypeAtom,
        Expr::List(_) => CExprType::List,
        Expr::Quote(_) => CExprType::Quote,
    }
}

/// If the Expr is an Atom, returns a pointer to its Atom.
/// Otherwise, returns NULL.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
/// The returned `*mut Atom` points to data owned by the `Expr`. Do not free it directly.
/// It becomes invalid when the parent `Expr` is freed.
#[unsafe(no_mangle)]
pub extern "C" fn expr_get_atom(ptr: *const Expr) -> *mut Atom {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let expr = unsafe { &*ptr };
    match expr {
        Expr::Atom(atom) => atom as *const Atom as *mut Atom,
        _ => std::ptr::null_mut(),
    }
}

/// If the Expr is a List, returns its length. Otherwise, returns -1.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
#[unsafe(no_mangle)]
pub extern "C" fn expr_list_len(ptr: *const Expr) -> c_int {
    if ptr.is_null() {
        return -1;
    }
    let expr = unsafe { &*ptr };
    match expr {
        Expr::List(list) => list.len() as c_int,
        _ => -1,
    }
}

/// If the Expr is a List, returns a pointer to the Expr at the given index.
/// Returns NULL if not a List, or index is out of bounds.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr` (a List).
/// `index` must be within the bounds of the list.
/// The returned `*mut Expr` points to data owned by the parent List `Expr`. Do not free it directly.
/// It becomes invalid when the parent `Expr` is freed.
#[unsafe(no_mangle)]
pub extern "C" fn expr_list_get_element(ptr: *const Expr, index: c_int) -> *mut Expr {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let expr = unsafe { &*ptr };
    match expr {
        Expr::List(list) => {
            if index >= 0 && (index as usize) < list.len() {
                &list[index as usize] as *const Expr as *mut Expr
            } else {
                std::ptr::null_mut()
            }
        }
        _ => std::ptr::null_mut(),
    }
}

/// If the Expr is a Quote, returns a pointer to the quoted Expr. Otherwise, returns NULL.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
/// The returned `*mut Expr` points to data owned by the parent Quote `Expr`. Do not free it directly.
/// It becomes invalid when the parent `Expr` is freed.
#[unsafe(no_mangle)]
pub extern "C" fn expr_get_quoted_expr(ptr: *const Expr) -> *mut Expr {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let expr = unsafe { &*ptr };
    match expr {
        Expr::Quote(boxed_expr) => boxed_expr.as_ref() as *const Expr as *mut Expr,
        _ => std::ptr::null_mut(),
    }
}

/// Returns the string representation of the Expr.
/// # Safety
/// `ptr` must be a valid, non-null pointer to an `Expr`.
/// The returned `*mut c_char` must be freed using `string_free`.
#[unsafe(no_mangle)]
pub extern "C" fn expr_to_string(ptr: *const Expr) -> *mut c_char {
    if ptr.is_null() {
        return std::ptr::null_mut();
    }
    let expr = unsafe { &*ptr };
    CString::new(expr.into_tokens())
        .unwrap_or_default()
        .into_raw()
}

// A C-compatible struct to hold the tokenized strings.
// This allows returning an array of strings and its length.
#[repr(C)]
pub struct CStringArray {
    pub strings: *mut *mut c_char, // Array of C strings
    pub len: c_int,
    pub error_code: CParserErrorCode, // Add error code directly
}

// A C-compatible struct to hold the parsed expressions.
#[repr(C)]
pub struct CExprArray {
    pub exprs: *mut *mut Expr, // Array of opaque Expr pointers
    pub len: c_int,
    pub error_code: CParserErrorCode, // Add error code directly
}

// --- FFI functions for Parser ---

impl Parser {
    /// config_read_number_internal for C ffi
    fn config_read_number_internal(&mut self, v: bool) {
        // Changed to take `&mut self`
        self.read_number_config = v;
    }

    /// parse_root_internal for C ffi
    pub fn parse_root_internal(&mut self, source_code: &str) -> Result<Vec<Expr>, ParserError> {
        let mut tokens = self.tokenize_internal(source_code);

        let mut res = vec![];

        let first_token = tokens.front().cloned();
        match first_token.as_deref() {
            Some("(") => { // It must start with an open parenthesis
                // Read the whole expression list, effectively populating `res`
            }
            Some(t) if t == " " || t == "\n" => {
                tokens.pop_front();
            }
            _ => return Err(ParserError::InvalidStart),
        }

        let root_expr = self.read_exp(&mut tokens)?;

        loop {
            match tokens.front() {
                Some(b) => match b.as_str() {
                    " " | "\n" => {
                        tokens.pop_front();
                    }
                    _ => {
                        // Attempt to read an expression
                        match self.read_router(b)?(self, &mut tokens) {
                            Ok(expr) => res.push(expr),
                            Err(e) => return Err(e), // Propagate parse error
                        }
                    }
                },
                None => break,
            }
        }

        Ok(res)
    }

    /// tokenize_internal for C ffi
    pub fn tokenize_internal(&self, source_code: &str) -> VecDeque<String> {
        let mut cache = vec![];
        let mut res = Vec::new(); // Changed from `vec![]` for consistency
        let mut chars = source_code.chars().peekable();

        while let Some(c) = chars.next() {
            match c {
                '(' | ' ' | ')' | '\'' | '"' | ':' | '\n' => {
                    if !cache.is_empty() {
                        res.push(String::from_utf8(cache.clone()).unwrap());
                        cache.clear();
                    }

                    // Handle consecutive spaces or newlines gracefully
                    if c == ' ' || c == '\n' {
                        if !res.is_empty()
                            && (res.last() == Some(&" ".to_string())
                                || res.last() == Some(&"\n".to_string()))
                        {
                            // If last token was space/newline and current is also space/newline, skip adding another one
                            if let Some(prev_char) = res.last().and_then(|s| s.chars().next()) {
                                if (c == ' ' && prev_char == ' ')
                                    || (c == '\n' && prev_char == '\n')
                                {
                                    continue;
                                }
                            }
                        }
                    }

                    res.push(c.to_string());
                }
                _ => {
                    cache.extend_from_slice(c.to_string().as_bytes()); // Push bytes to cache
                }
            }
        }

        if !cache.is_empty() {
            res.push(String::from_utf8(cache.clone()).unwrap());
        }

        res.into()
    }
}

/// Creates a new Parser instance.
#[unsafe(no_mangle)]
pub extern "C" fn parser_new() -> *mut Parser {
    Box::into_raw(Box::new(Parser::new()))
}

/// Frees a Parser instance.
/// # Safety
/// `ptr` must be a valid, non-null pointer to a `Parser` previously created by `parser_new`.
#[unsafe(no_mangle)]
pub extern "C" fn parser_free(ptr: *mut Parser) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}

/// Configures the Parser to read numbers.
/// # Safety
/// `ptr` must be a valid, non-null pointer to a `Parser`.
#[unsafe(no_mangle)]
pub extern "C" fn parser_config_read_number(ptr: *mut Parser, v: bool) {
    if ptr.is_null() {
        return;
    }
    let parser = unsafe { &mut *ptr };
    parser.config_read_number_internal(v);
}

/// Tokenizes the given source code.
/// Returns a CStringArray containing the tokens.
/// # Safety
/// `parser_ptr` must be a valid, non-null pointer to a `Parser`.
/// `source_code` must be a valid, null-terminated C string.
/// The returned `CStringArray` must be freed using `cstring_array_free`.
/// Returns a CStringArray with `strings` as NULL and `len` as 0 on error.
#[unsafe(no_mangle)]
pub extern "C" fn parser_tokenize(
    parser_ptr: *mut Parser,
    source_code: *const c_char,
) -> CStringArray {
    if parser_ptr.is_null() || source_code.is_null() {
        return CStringArray {
            strings: std::ptr::null_mut(),
            len: 0,
            error_code: CParserErrorCode::NullPointer,
        };
    }

    let parser = unsafe { &*parser_ptr };
    let c_str = unsafe { CStr::from_ptr(source_code) };
    let rust_str = match c_str.to_str() {
        Ok(s) => s,
        Err(e) => {
            return CStringArray {
                strings: std::ptr::null_mut(),
                len: 0,
                error_code: CParserErrorCode::SerializationError,
            };
        }
    };

    let tokens = parser.tokenize_internal(rust_str);

    let mut c_strings: Vec<*mut c_char> = Vec::with_capacity(tokens.len());
    for token in tokens {
        c_strings.push(CString::new(token).unwrap_or_default().into_raw());
    }

    let len = c_strings.len();
    let strings_ptr = c_strings.leak() as *mut _ as *mut *mut c_char; // Leak the Vec, return raw pointer to its contents

    CStringArray {
        strings: strings_ptr,
        len: len as c_int,
        error_code: CParserErrorCode::Success,
    }
}

/// Frees a CStringArray.
/// This includes freeing each individual string and the array of pointers itself.
/// # Safety
/// `array` must be a valid `CStringArray` previously returned by `parser_tokenize`.
#[unsafe(no_mangle)]
pub extern "C" fn cstring_array_free(array: CStringArray) {
    if array.strings.is_null() {
        return;
    }
    unsafe {
        // Re-construct the Vec from the raw parts
        let rust_strings =
            Vec::from_raw_parts(array.strings, array.len as usize, array.len as usize);
        // Free each individual CString
        for ptr in rust_strings {
            let _ = CString::from_raw(ptr);
        }
        // The Vec itself will be dropped here, freeing the array of pointers.
    }
}

/// Parses the given source code and returns a CExprArray.
/// Returns a CExprArray with `exprs` as NULL and `len` as 0 on error.
/// # Safety
/// `parser_ptr` must be a valid, non-null pointer to a `Parser`.
/// `source_code` must be a valid, null-terminated C string.
/// The returned `CExprArray` must be freed using `cexpr_array_free`.
#[unsafe(no_mangle)]
pub extern "C" fn parser_parse_root(
    parser_ptr: *mut Parser,
    source_code: *const c_char,
) -> CExprArray {
    if parser_ptr.is_null() || source_code.is_null() {
        return CExprArray {
            exprs: std::ptr::null_mut(),
            len: 0,
            error_code: CParserErrorCode::NullPointer,
        };
    }

    let parser = unsafe { &mut *parser_ptr }; // Need mutable parser for parse_root
    let c_str = unsafe { CStr::from_ptr(source_code) };
    let rust_str = match c_str.to_str() {
        Ok(s) => s,
        Err(e) => {
            return CExprArray {
                exprs: std::ptr::null_mut(),
                len: 0,
                error_code: CParserErrorCode::SerializationError,
            };
        }
    };

    match parser.parse_root_internal(rust_str) {
        Ok(expr_vec) => {
            let mut c_exprs: Vec<*mut Expr> = Vec::with_capacity(expr_vec.len());
            for expr in expr_vec {
                c_exprs.push(Box::into_raw(Box::new(expr)));
            }

            let len = c_exprs.len();
            let exprs_ptr = c_exprs.leak() as *mut _ as *mut *mut Expr;

            CExprArray {
                exprs: exprs_ptr,
                len: len as c_int,
                error_code: CParserErrorCode::Success,
            }
        }
        Err(e) => {
            CExprArray {
                exprs: std::ptr::null_mut(),
                len: 0,
                error_code: e.into(), // Convert ParserError into CParserErrorCode
            }
        }
    }
}

/// Frees a CExprArray.
/// This includes freeing each individual Expr contained within and the array of pointers itself.
/// # Safety
/// `array` must be a valid `CExprArray` previously returned by `parser_parse_root`.
#[unsafe(no_mangle)]
pub extern "C" fn cexpr_array_free(mut array: CExprArray) {
    if array.exprs.is_null() {
        return;
    }
    unsafe {
        // Re-construct the Vec from the raw parts
        let rust_expr_ptrs =
            Vec::from_raw_parts(array.exprs, array.len as usize, array.len as usize);
        // Free each individual Expr
        for ptr in rust_expr_ptrs {
            expr_free(ptr); // Use the existing expr_free function
        }
        // The Vec itself will be dropped here, freeing the array of pointers.
    }
}

// Helper for ParserError to CParserErrorCode conversion
impl From<ParserError> for CParserErrorCode {
    fn from(err: ParserError) -> Self {
        match err {
            ParserError::InvalidStart => CParserErrorCode::InvalidStart,
            ParserError::InvalidToken(_) => CParserErrorCode::InvalidToken,
            ParserError::UnknownToken => CParserErrorCode::UnknownToken,
            ParserError::CorruptData(_) => todo!(),
        }
    }
}
