#![feature(iter_array_chunks)]
#![feature(assert_matches)]
pub mod data;

use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_longlong}; // c_longlong for i64

use std::{collections::VecDeque, error::Error, io::Read};
use tracing::error;

#[derive(Debug, PartialEq, Eq)]
pub enum ParserError {
    InvalidStart,
    InvalidToken(&'static str),
    CorruptData(&'static str),
    UnknownToken,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::InvalidStart => write!(f, "parser error: Invalid start token"),
            ParserError::InvalidToken(msg) => write!(f, "parser error: Invalid token: {}", msg),
            ParserError::UnknownToken => write!(f, "parser error: Unknown token"),
            ParserError::CorruptData(msg) => write!(f, "parser error: illegal data: {}", msg),
        }
    }
}

impl Error for ParserError {}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeValue {
    Symbol(String),
    String(String),
    Keyword(String),
    Number(i64),
}

impl TypeValue {
    pub fn to_string(&self) -> String {
        match self {
            TypeValue::Symbol(s) => s.clone(),
            TypeValue::String(s) => format!("\"{}\"", s),
            TypeValue::Keyword(s) => format!(":{}", s),
            TypeValue::Number(d) => d.to_string(),
        }
    }

    pub fn make_symbol(s: &str) -> Result<Self, Box<dyn Error>> {
        if s.contains([' ']) {
            Err(Box::new(ParserError::CorruptData(
                "cannot make symbol with this str",
            )))
        } else {
            Ok(Self::Symbol(s.to_string()))
        }
    }
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Atom {
    pub value: TypeValue,
}

impl Atom {
    fn read(s: &str) -> Self {
        Self {
            value: TypeValue::Symbol(s.to_string()),
        }
    }

    fn read_string(s: &str) -> Self {
        Self {
            value: TypeValue::String(s.to_string()),
        }
    }

    fn read_keyword(s: &str) -> Self {
        Self {
            value: TypeValue::Keyword(s.to_string()),
        }
    }

    fn read_number(_s: &str, n: i64) -> Self {
        Self {
            value: TypeValue::Number(n),
        }
    }

    pub fn is_string(&self) -> bool {
        match self.value {
            TypeValue::String(_) => true,
            _ => false,
        }
    }

    pub fn to_string(&self) -> String {
        self.value.to_string()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    Atom(Atom),
    List(Vec<Expr>),
    Quote(Box<Expr>),
}

impl Expr {
    pub fn into_tokens(&self) -> String {
        match self {
            Expr::Atom(atom) => atom.to_string(),
            Expr::List(exprs) => {
                String::from("(")
                    + &exprs
                        .iter()
                        .map(|a| a.into_tokens())
                        .collect::<Vec<String>>()
                        .join(" ")
                    + ")"
            }
            Expr::Quote(expr) => String::from("'") + &expr.into_tokens(),
        }
    }

    pub fn nth(&self, ind: usize) -> Option<&Self> {
        match self {
            Expr::List(exprs) => exprs.get(ind),
            _ => None,
        }
    }

    pub fn iter(&self) -> Option<impl Iterator<Item = &Expr>> {
        match self {
            Expr::List(exprs) => Some(exprs.iter()),
            _ => None,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.into_tokens())
    }
}

pub struct Parser {
    /// will read number if this field is true. default is true
    /// turn it off will treat the number as the symbol in Expr
    read_number_config: bool,
}

impl Default for Parser {
    fn default() -> Self {
        Self {
            read_number_config: true,
        }
    }
}

impl Parser {
    pub fn new() -> Self {
        Self {
            read_number_config: true,
        }
    }

    /// set the parser read_number config
    pub fn config_read_number(mut self, v: bool) -> Self {
        self.read_number_config = v;
        self
    }

    /// tokenize the source code
    pub fn tokenize(&self, mut source_code: impl Read) -> VecDeque<String> {
        let mut buf = [0; 1];
        let mut cache = vec![];
        let mut res = vec![];
        loop {
            match source_code.read(&mut buf) {
                Ok(n) if n != 0 => {
                    let c = buf.get(0).unwrap();
                    match c {
                        b'(' | b' ' | b')' | b'\'' | b'"' | b':' | b'\n' => {
                            if !cache.is_empty() {
                                res.push(String::from_utf8(cache.clone()).unwrap());
                                cache.clear();
                            }

                            match res.last() {
                                Some(le) if le == " " && *c == b' ' => continue,
                                _ => (),
                            }

                            res.push(String::from_utf8(vec![*c]).unwrap())
                        }
                        _ => {
                            cache.push(*c);
                        }
                    }
                }
                Ok(_) => break,
                Err(e) => error!("error in tokenize step {}", e),
            }
        }

        if !cache.is_empty() {
            res.push(String::from_utf8(cache.clone()).unwrap());
        }

        res.into()
    }

    fn parse_root(&mut self, source_code: impl Read) -> Result<Vec<Expr>, ParserError> {
        let mut tokens = self.tokenize(source_code);

        let mut res = vec![];
        match tokens.get(0) {
            Some(t) if t == "(" => {}
            _ => return Err(ParserError::InvalidStart),
        }

        loop {
            match tokens.front() {
                Some(b) => match b.as_str() {
                    "(" => {
                        res.push(self.read_exp(&mut tokens)?);
                    }
                    " " | "\n" => {
                        tokens.pop_front();
                    }
                    _ => {
                        return {
                            println!("{:?}", b);
                            Err(ParserError::InvalidToken("in read_root"))
                        };
                    }
                },
                None => break,
            }
        }

        Ok(res)
    }

    /// choose which read function
    fn read_router(
        &self,
        token: &str,
    ) -> Result<fn(&Self, &mut VecDeque<String>) -> Result<Expr, ParserError>, ParserError> {
        match token {
            "(" => Ok(Self::read_exp),
            "'" => Ok(Self::read_quote),
            "\"" => Ok(Self::read_string),
            ":" => Ok(Self::read_keyword),
            _ => Ok(Self::read_atom),
        }
    }

    fn read_atom(&self, tokens: &mut VecDeque<String>) -> Result<Expr, ParserError> {
        let token = tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_sym"))?;

        if self.read_number_config {
            match token.parse::<i64>() {
                Ok(n) => return Ok(Expr::Atom(Atom::read_number(&token, n))),
                Err(_) => (),
            }
        }

        Ok(Expr::Atom(Atom::read(&token)))
    }

    fn read_quote(&self, tokens: &mut VecDeque<String>) -> Result<Expr, ParserError> {
        tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_quote"))?;

        let res = match tokens.front() {
            Some(t) => self.read_router(t)?(self, tokens)?,
            None => return Err(ParserError::InvalidToken("in read_quote")),
        };

        Ok(Expr::Quote(Box::new(res)))
    }

    /// start from '\('
    pub fn read_exp(&self, tokens: &mut VecDeque<String>) -> Result<Expr, ParserError> {
        let mut res = vec![];
        tokens.pop_front();

        loop {
            match tokens.front() {
                Some(t) if t == ")" => {
                    tokens.pop_front();
                    break;
                }
                // ignore spaces
                Some(t) if t == " " || t == "\n" => {
                    tokens.pop_front();
                }
                Some(t) => res.push(self.read_router(t)?(self, tokens)?),
                None => return Err(ParserError::InvalidToken("in read_exp")),
            }
        }

        Ok(Expr::List(res))
    }

    /// start with "
    fn read_string(&self, tokens: &mut VecDeque<String>) -> Result<Expr, ParserError> {
        tokens.pop_front();

        let mut escape = false;
        let mut res = String::new();
        let mut this_token;
        loop {
            this_token = tokens
                .pop_front()
                .ok_or(ParserError::InvalidToken("in read_string"))?;

            if escape {
                res = res + &this_token;
                escape = false;
                continue;
            }

            match this_token.as_str() {
                "\\" => escape = true,
                "\"" => break,
                _ => res = res + &this_token,
            }
        }

        Ok(Expr::Atom(Atom::read_string(&res)))
    }

    /// start with :
    fn read_keyword(&self, tokens: &mut VecDeque<String>) -> Result<Expr, ParserError> {
        tokens.pop_front();

        let token = tokens
            .pop_front()
            .ok_or(ParserError::InvalidToken("in read_keyword"))?;

        Ok(Expr::Atom(Atom::read_keyword(&token)))
    }
}

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

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn test_tokenize() {
        let parser = Parser::new();
        //
        let s = "(a b c 123 c)";
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "b", " ", "c", " ", "123", " ", "c", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(a '(""))"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "'", "(", "\"", "\"", ")", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(a '() '1)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", "a", " ", "'", "(", ")", " ", "'", "1", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );

        //
        let s = r#"(def-msg language-perfer :lang 'string)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(",
                "def-msg",
                " ",
                "language-perfer",
                " ",
                ":",
                "lang",
                " ",
                "'",
                "string",
                ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        //
        let s = r#"(def-rpc get-book
                     '(:title 'string :vesion 'string :lang 'language-perfer)
                    'book-info)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(",
                "def-rpc",
                " ",
                "get-book",
                "\n",
                " ",
                "'",
                "(",
                ":",
                "title",
                " ",
                "'",
                "string",
                " ",
                ":",
                "vesion",
                " ",
                "'",
                "string",
                " ",
                ":",
                "lang",
                " ",
                "'",
                "language-perfer",
                ")",
                "\n",
                " ",
                "'",
                "book-info",
                ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        //
        let s = r#"(get-book :title "hello world" :version "1984")"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(", "get-book", " ", ":", "title", " ", "\"", "hello", " ", "world", "\"", " ",
                ":", "version", " ", "\"", "1984", "\"", ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        // escapr "
        let s = r#"( get-book :title "hello \"world" :version "1984")"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec![
                "(", " ", "get-book", " ", ":", "title", " ", "\"", "hello", " ", "\\", "\"",
                "world", "\"", " ", ":", "version", " ", "\"", "1984", "\"", ")"
            ]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<Vec<_>>()
        );

        // number

        let s = r#"( get-book :id 1984)"#;
        assert_eq!(
            parser.tokenize(Cursor::new(s.as_bytes())),
            vec!["(", " ", "get-book", " ", ":", "id", " ", "1984", ")"]
                .into_iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_read_string() {
        let parser = Parser::new();
        let mut t = parser.tokenize(Cursor::new(r#""hello""#.as_bytes()));
        assert_eq!(
            parser.read_string(&mut t),
            Ok(Expr::Atom(Atom::read_string("hello")))
        );
        assert!(t.is_empty());
    }

    #[test]
    fn test_read_number() {
        let parser = Parser::new().config_read_number(true);

        let mut t = parser.tokenize(Cursor::new(r#"123"#.as_bytes()));

        assert_eq!(
            parser.read_atom(&mut t),
            Ok(Expr::Atom(Atom::read_number("123", 123)))
        );
    }

    #[test]
    fn test_read_exp() {
        let parser = Parser::new();
        let mut t = parser.tokenize(Cursor::new("(a b c 123 c)".as_bytes()));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("a")),
                    Expr::Atom(Atom::read("b")),
                    Expr::Atom(Atom::read("c")),
                    Expr::Atom(Atom::read("123")),
                    Expr::Atom(Atom::read("c")),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new("((a) b c 123 c)".as_bytes()));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::List([Expr::Atom(Atom::read("a"))].to_vec()),
                    Expr::Atom(Atom::read("b")),
                    Expr::Atom(Atom::read("c")),
                    Expr::Atom(Atom::read("123")),
                    Expr::Atom(Atom::read("c")),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(def-msg language-perfer :lang 'string)"#.as_bytes(),
        ));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("def-msg")),
                    Expr::Atom(Atom::read("language-perfer")),
                    Expr::Atom(Atom::read_keyword("lang")),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        ));
        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("def-rpc")),
                    Expr::Atom(Atom::read("get-book")),
                    Expr::Quote(Box::new(Expr::List(
                        [
                            Expr::Atom(Atom::read_keyword("title")),
                            Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                            Expr::Atom(Atom::read_keyword("version")),
                            Expr::Quote(Box::new(Expr::Atom(Atom::read("string")))),
                            Expr::Atom(Atom::read_keyword("lang")),
                            Expr::Quote(Box::new(Expr::Atom(Atom::read("language-perfer")))),
                        ]
                        .to_vec()
                    ))),
                    Expr::Quote(Box::new(Expr::Atom(Atom::read("book-info")))),
                ]
                .to_vec()
            ),)
        );
        //dbg!(&t);
        assert!(t.is_empty());

        //
        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello world" :version "1984")"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("get-book")),
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Atom(Atom::read_string("hello world")),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Atom(Atom::read_string("1984")),
                ]
                .to_vec()
            ),)
        );

        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello \"world" :version "1984")"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("get-book")),
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Atom(Atom::read_string("hello \"world")),
                    Expr::Atom(Atom::read_keyword("version")),
                    Expr::Atom(Atom::read_string("1984")),
                ]
                .to_vec()
            ),)
        );

        //

        let parser = Parser::new().config_read_number(true);

        let mut t = parser.tokenize(Cursor::new(
            r#"(get-book :title "hello world" :id 1984)"#.as_bytes(),
        ));

        assert_eq!(
            parser.read_exp(&mut t),
            Ok(Expr::List(
                [
                    Expr::Atom(Atom::read("get-book")),
                    Expr::Atom(Atom::read_keyword("title")),
                    Expr::Atom(Atom::read_string("hello world")),
                    Expr::Atom(Atom::read_keyword("id")),
                    Expr::Atom(Atom::read_number("1984", 1984)),
                ]
                .to_vec()
            ),)
        );
    }

    #[test]
    fn test_read_root() {
        let mut parser = Parser::new();

        let expr = parser
            .parse_root(&mut Cursor::new("(a b c 123 c) (a '(1 2 3))".as_bytes()))
            .unwrap();
        assert_eq!(
            expr,
            vec![
                Expr::List(vec![
                    Expr::Atom(Atom::read("a")),
                    Expr::Atom(Atom::read("b")),
                    Expr::Atom(Atom::read("c")),
                    Expr::Atom(Atom::read("123")),
                    Expr::Atom(Atom::read("c")),
                ],),
                Expr::List(vec![
                    Expr::Atom(Atom::read("a")),
                    Expr::Quote(Box::new(Expr::List(vec![
                        Expr::Atom(Atom::read("1")),
                        Expr::Atom(Atom::read("2")),
                        Expr::Atom(Atom::read("3")),
                    ]))),
                ],),
            ],
        );

        let expr = parser
            .parse_root(Cursor::new(r#"('a "hello")"#.as_bytes()))
            .unwrap();
        assert_eq!(
            expr,
            vec![Expr::List(vec![
                Expr::Quote(Box::new(Expr::Atom(Atom::read("a")))),
                Expr::Atom(Atom::read_string("hello")),
            ])],
        );

        //
        let mut t = Cursor::new(
            r#"(def-msg language-perfer :lang 'string)

(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        );

        let s0 = Cursor::new(r#"(def-msg language-perfer :lang 'string)"#.as_bytes());
        let mut t0 = parser.tokenize(s0.clone());

        let s1 = Cursor::new(
            r#"(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        );
        let mut t1 = parser.tokenize(s1.clone());

        let expr = parser.parse_root(&mut t).unwrap();
        assert_eq!(
            expr,
            vec![
                parser.read_exp(&mut t0).unwrap(),
                parser.read_exp(&mut t1).unwrap()
            ]
        );
    }

    #[test]
    fn test_into_tokens() {
        let mut parser = Parser::new();
        let mut t = Cursor::new(
            r#"(def-msg language-perfer :lang 'string)

(def-rpc get-book
                     '(:title 'string :version 'string :lang 'language-perfer)
                    'book-info)"#
                .as_bytes(),
        );

        let expr = parser.parse_root(&mut t).unwrap();

        assert_eq!(
            expr.into_iter().map(|e|e.into_tokens()).collect::<Vec<String>>(),
            vec![
                "(def-msg language-perfer :lang 'string)".to_string(),
                "(def-rpc get-book '(:title 'string :version 'string :lang 'language-perfer) 'book-info)".to_string(),
            ],
        );
    }
}
