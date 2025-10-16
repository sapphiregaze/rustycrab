use logos::Logos;
use logos::Skip;

/// A wrapper around a [`logos::Lexer`] that provides an iterator with
/// consistent error handling.
///
/// The [`Lexer`] struct encapsulates the underlying Logos lexer for a given
/// source string and exposes an iterator of `Result<Token, LexingError>`.
///
/// # Examples
///
/// ```
/// use lexer::Lexer;
/// let mut lexer = Lexer::new("foo bar");
/// for result in lexer.iter() {
///     match result {
///         Ok(token) => println!("Token: {:?}", token),
///         Err(e) => eprintln!("Lexing error: {:?}", e),
///     }
/// }
/// ```
#[derive(Debug)]
pub struct Lexer<'src> {
    inner: logos::Lexer<'src, Token>,
}

impl<'src> Lexer<'src> {
    /// Creates a new [`Lexer`] from a source string slice.
    ///
    /// This initializes the internal Logos lexer and prepares it to
    /// produce tokens from the provided input.
    pub fn new(src: &'src str) -> Self {
        Self {
            inner: Token::lexer(src),
        }
    }

    /// Returns an iterator over the tokens produced by the lexer.
    ///
    /// Each item in the iterator is a `Result<Token, LexingError>`, where:
    /// - `Ok(Token)` represents a successfully parsed token.
    /// - `Err(LexingError)` represents an error encountered during lexing.
    ///
    /// The iterator borrows `self` mutably, as the underlying [`logos::Lexer`]
    /// maintains internal state during iteration.
    pub fn iter(&mut self) -> impl Iterator<Item = Result<Token, LexingError>> + '_ {
        std::iter::from_fn(move || {
            let tok_res = self.inner.next()?;
            let res = match tok_res {
                Ok(Token::Err(e)) => Err(e),
                Ok(t) => Ok(t),
                Err(e) => Err(e),
            };
            Some(res)
        })
    }

    /// Collects all tokens from the lexer into a vector.
    ///
    /// This method consumes the underlying lexer state to produce a `Vec`
    /// containing the results of all lexing operations in sequence.
    pub fn collect_all(&mut self) -> Vec<Result<Token, LexingError>> {
        self.iter().collect()
    }
}

/// Holds additional metadata for each token.
///
/// This includes:
/// - `lexeme`: the matched string slice from the source.
/// - `line`: the current line number in the source code.
/// - `column`: the column position where the token starts.
#[derive(Debug, Clone, PartialEq)]
pub struct Extras {
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Default for Extras {
    /// Provides default values for [`Extras`]:
    /// - `lexeme` is empty
    /// - `line` starts at 1
    /// - `column` starts at 0
    fn default() -> Self {
        Self {
            lexeme: String::new(),
            line: 1,
            column: 0,
        }
    }
}

/// Represents possible errors that can occur during lexical analysis.
///
/// This enum covers unexpected characters, unprocessed preprocessor directives,
/// and other miscellaneous lexing errors that do not fall into a specific
/// category.
#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexingError {
    /// An unexpected or invalid character was encountered while lexing.
    ///
    /// This variant typically indicates a syntax error or unsupported symbol.
    UnexpectedCharacter(char),

    /// A preprocessor directive (e.g., `#include`, `#define`) was encountered.
    ///
    /// This is useful for detecting source code that still contains
    /// unprocessed directives that should have been resolved before lexing.
    UnprocessedDirective(String),

    /// A generic fallback variant for unspecified or miscellaneous lexing
    /// errors.
    #[default]
    Other,
}

impl LexingError {
    /// Constructs a [`LexingError`] from the current lexer state.
    ///
    /// This function extracts the first character of the current token slice
    /// and wraps it in the [`LexingError::UnexpectedCharacter`] variant.
    fn from_lexer(lex: &mut logos::Lexer<Token>) -> Self {
        LexingError::UnexpectedCharacter(lex.slice().chars().next().unwrap())
    }
}

/// Handles newline tokens (`\n`) in the source code.
///
/// Increments the line counter and updates the column to the end of the line.
/// Returns [`Skip`] to ignore the newline token itself.
fn newline_callback(lex: &mut logos::Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

/// Generates an [`Extras`] instance for a matched token.
///
/// Captures:
/// - the lexeme string
/// - current line
/// - column (calculated relative to last newline)
fn token_callback(lex: &mut logos::Lexer<Token>) -> Extras {
    let lexeme = lex.slice().trim().to_string();
    let line = lex.extras.line;
    let column = lex.span().start.saturating_sub(lex.extras.column) + 1;
    Extras {
        lexeme,
        line,
        column,
    }
}

/// Handles multi-line comments.
///
/// Updates line and column tracking appropriately, and skips the comment token.
fn comment_callback(lex: &mut logos::Lexer<Token>) -> Skip {
    let slice = lex.slice();
    let newlines = slice.matches('\n').count();
    lex.extras.line += newlines;
    if let Some(pos) = slice.rfind('\n') {
        lex.extras.column = slice.len() - pos;
    }
    Skip
}

/// Callback for handling unprocessed preprocessor directives.
///
/// This function is used by the lexer when it matches a token that begins with
/// `#`. It captures the entire directive text and returns it as a
/// [`LexingError::UnprocessedDirective`].
///
/// This allows the lexer to flag unexpanded or unsupported preprocessor
/// directives instead of silently skipping them.
fn preprocessor_callback(lex: &mut logos::Lexer<Token>) -> LexingError {
    let directive = lex.slice().trim().to_string();
    LexingError::UnprocessedDirective(directive)
}

/// Tokens for the C syntax.
///
/// Uses the [`Extras`] struct to track line, column, and lexeme metadata.
/// Includes keywords, identifiers, constants, operators, punctuation, and
/// comments.
///
/// Any unidentified character would result in a
/// [`LexingError::UnexpectedCharacter`].
#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(extras = Extras)]
#[logos(skip(r"\n", newline_callback))]
#[logos(skip r"[ \t\v\f]")]
#[logos(error(LexingError, LexingError::from_lexer))]
pub enum Token {
    // === Keywords ===
    #[token("auto", token_callback)]
    Auto(Extras),
    #[token("break", token_callback)]
    Break(Extras),
    #[token("case", token_callback)]
    Case(Extras),
    #[token("char", token_callback)]
    Char(Extras),
    #[token("const", token_callback)]
    Const(Extras),
    #[token("continue", token_callback)]
    Continue(Extras),
    #[token("default", token_callback)]
    Default(Extras),
    #[token("do", token_callback)]
    Do(Extras),
    #[token("double", token_callback)]
    Double(Extras),
    #[token("else", token_callback)]
    Else(Extras),
    #[token("enum", token_callback)]
    Enum(Extras),
    #[token("extern", token_callback)]
    Extern(Extras),
    #[token("float", token_callback)]
    Float(Extras),
    #[token("for", token_callback)]
    For(Extras),
    #[token("goto", token_callback)]
    Goto(Extras),
    #[token("if", token_callback)]
    If(Extras),
    #[token("inline", token_callback)]
    Inline(Extras),
    #[token("int", token_callback)]
    Int(Extras),
    #[token("long", token_callback)]
    Long(Extras),
    #[token("register", token_callback)]
    Register(Extras),
    #[token("restrict", token_callback)]
    Restrict(Extras),
    #[token("return", token_callback)]
    Return(Extras),
    #[token("short", token_callback)]
    Short(Extras),
    #[token("signed", token_callback)]
    Signed(Extras),
    #[token("sizeof", token_callback)]
    Sizeof(Extras),
    #[token("static", token_callback)]
    Static(Extras),
    #[token("struct", token_callback)]
    Struct(Extras),
    #[token("switch", token_callback)]
    Switch(Extras),
    #[token("typedef", token_callback)]
    Typedef(Extras),
    #[token("union", token_callback)]
    Union(Extras),
    #[token("unsigned", token_callback)]
    Unsigned(Extras),
    #[token("void", token_callback)]
    Void(Extras),
    #[token("volatile", token_callback)]
    Volatile(Extras),
    #[token("while", token_callback)]
    While(Extras),

    #[token("_Alignas", token_callback)]
    Alignas(Extras),
    #[token("_Alignof", token_callback)]
    Alignof(Extras),
    #[token("_Atomic", token_callback)]
    Atomic(Extras),
    #[token("_Bool", token_callback)]
    Bool(Extras),
    #[token("_Complex", token_callback)]
    Complex(Extras),
    #[token("_Generic", token_callback)]
    Generic(Extras),
    #[token("_Imaginary", token_callback)]
    Imaginary(Extras),
    #[token("_Noreturn", token_callback)]
    Noreturn(Extras),
    #[token("_Static_assert", token_callback)]
    StaticAssert(Extras),
    #[token("_Thread_local", token_callback)]
    ThreadLocal(Extras),
    #[token("__func__", token_callback)]
    FuncName(Extras),

    // === Identifiers ===
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", token_callback)]
    Identifier(Extras),

    // === Constants ===
    #[regex(
        r"0[xX][a-fA-F0-9]+(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
        token_callback,
        priority = 7
    )]
    #[regex(
        r"0[0-7]*(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
        token_callback,
        priority = 5
    )]
    #[regex(
        r"[1-9][0-9]*(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
        token_callback,
        priority = 3
    )]
    #[regex(
        r#"(u|U|L)?'([^'\\\n]|\\(['\"?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))+'"#,
        token_callback
    )]
    IntegerConstant(Extras),

    #[regex(r"[0-9]+([Ee][+-]?[0-9]+)?(f|F|l|L)?", token_callback)]
    #[regex(r"[0-9]*\.[0-9]+([Ee][+-]?[0-9]+)?(f|F|l|L)?", token_callback)]
    #[regex(r"[0-9]+\.[Ee]?[+-]?[0-9]*(f|F|l|L)?", token_callback, priority = 0)]
    #[regex(r"0[xX][a-fA-F0-9]+([Pp][+-]?[0-9]+)?(f|F|l|L)?", token_callback)]
    #[regex(
        r"0[xX][a-fA-F0-9]*\.[a-fA-F0-9]+([Pp][+-]?[0-9]+)?(f|F|l|L)?",
        token_callback
    )]
    #[regex(r"0[xX][a-fA-F0-9]+\.[Pp][+-]?[0-9]+(f|F|l|L)?", token_callback)]
    FloatConstant(Extras),

    #[regex(r#"(u8|u|U|L)?\"([^"\\\n]|\\.)*\""#, token_callback)]
    StringLiteral(Extras),

    // === Operators ===
    #[token("...", token_callback)]
    Ellipsis(Extras),
    #[token(">>=", token_callback)]
    RightAssign(Extras),
    #[token("<<=", token_callback)]
    LeftAssign(Extras),
    #[token("+=", token_callback)]
    AddAssign(Extras),
    #[token("-=", token_callback)]
    SubAssign(Extras),
    #[token("*=", token_callback)]
    MulAssign(Extras),
    #[token("/=", token_callback)]
    DivAssign(Extras),
    #[token("%=", token_callback)]
    ModAssign(Extras),
    #[token("&=", token_callback)]
    AndAssign(Extras),
    #[token("^=", token_callback)]
    XorAssign(Extras),
    #[token("|=", token_callback)]
    OrAssign(Extras),
    #[token(">>", token_callback)]
    RightOp(Extras),
    #[token("<<", token_callback)]
    LeftOp(Extras),
    #[token("++", token_callback)]
    IncOp(Extras),
    #[token("--", token_callback)]
    DecOp(Extras),
    #[token("->", token_callback)]
    PtrOp(Extras),
    #[token("&&", token_callback)]
    AndOp(Extras),
    #[token("||", token_callback)]
    OrOp(Extras),
    #[token("<=", token_callback)]
    LeOp(Extras),
    #[token(">=", token_callback)]
    GeOp(Extras),
    #[token("==", token_callback)]
    EqOp(Extras),
    #[token("!=", token_callback)]
    NeOp(Extras),

    // === Characters ===
    #[token(";", token_callback)]
    Semicolon(Extras),
    #[token("{", token_callback)]
    LBrace(Extras),
    #[token("}", token_callback)]
    RBrace(Extras),
    #[token(",", token_callback)]
    Comma(Extras),
    #[token(":", token_callback)]
    Colon(Extras),
    #[token("=", token_callback)]
    Assign(Extras),
    #[token("(", token_callback)]
    LParen(Extras),
    #[token(")", token_callback)]
    RParen(Extras),
    #[token("[", token_callback)]
    LBracket(Extras),
    #[token("]", token_callback)]
    RBracket(Extras),
    #[token(".", token_callback)]
    Dot(Extras),
    #[token("&", token_callback)]
    Amp(Extras),
    #[token("!", token_callback)]
    Bang(Extras),
    #[token("~", token_callback)]
    Tilde(Extras),
    #[token("-", token_callback)]
    Minus(Extras),
    #[token("+", token_callback)]
    Plus(Extras),
    #[token("*", token_callback)]
    Star(Extras),
    #[token("/", token_callback)]
    Slash(Extras),
    #[token("%", token_callback)]
    Percent(Extras),
    #[token("<", token_callback)]
    Lt(Extras),
    #[token(">", token_callback)]
    Gt(Extras),
    #[token("^", token_callback)]
    Caret(Extras),
    #[token("|", token_callback)]
    Pipe(Extras),
    #[token("?", token_callback)]
    Question(Extras),

    // === Comments ===
    /// Multi-line (`/* ... */`) and single-line (`// ...`) comments.
    /// Updates line/column counters but does not produce tokens.
    #[regex(
        r#"(?m)/\*([^"*]|".*")*\*+(([^"/*]|".*")([^"*]|".*")*\*+)*/"#,
        comment_callback
    )]
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,

    // === Errors ===
    // Handles lexing errors related to the lack of preprocessing.
    #[regex(
        r"#\s*(assert|define|elif|else|endif|error|ident|if|ifdef|ifndef|import|include|include_next|line|pragma|sccs|unassert|undef|warning)[^\n]*",
        preprocessor_callback
    )]
    Err(LexingError),
}

#[cfg(test)]
mod tests {
    use logos::Logos;

    use super::*;

    #[test]
    fn test_keywords() {
        let src = "int return float _Generic";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let expected: Vec<(&str, usize)> =
            vec![("int", 1), ("return", 5), ("float", 12), ("_Generic", 18)];

        for (token, (lexeme, column)) in tokens.iter().zip(expected.iter()) {
            match token {
                Token::Int(ex) | Token::Return(ex) | Token::Float(ex) | Token::Generic(ex) => {
                    assert_eq!(&ex.lexeme, lexeme);
                    assert_eq!(ex.line, 1);
                    assert_eq!(ex.column, *column);
                }
                _ => panic!("Unexpected token {:?}", token),
            }
        }

        assert_eq!(tokens.len(), expected.len());
    }

    #[test]
    fn test_identifiers() {
        let src = "foo _bar baz123";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let lexemes: Vec<String> = tokens
            .iter()
            .map(|t| match t {
                Token::Identifier(e) => e.lexeme.clone(),
                _ => "".to_string(),
            })
            .collect();

        assert_eq!(lexemes, vec!["foo", "_bar", "baz123"]);
    }

    #[test]
    fn test_integer_constants() {
        let src = "0 123 0xff 0755";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let lexemes: Vec<String> = tokens
            .iter()
            .map(|t| match t {
                Token::IntegerConstant(e) => e.lexeme.clone(),
                _ => "".to_string(),
            })
            .collect();

        assert_eq!(lexemes, vec!["0", "123", "0xff", "0755"]);
    }

    #[test]
    fn test_float_constants() {
        let src = "1.23 4.5e6 0x1.8p1";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let lexemes: Vec<String> = tokens
            .iter()
            .map(|t| match t {
                Token::FloatConstant(e) => e.lexeme.clone(),
                _ => "".to_string(),
            })
            .collect();

        assert_eq!(lexemes, vec!["1.23", "4.5e6", "0x1.8p1"]);
    }

    #[test]
    fn test_operators_and_characters() {
        let src = "+ - * / % ++ -- += -= == != <= >= && || ; , { }";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let lexemes: Vec<String> = tokens
            .iter()
            .map(|t| match t {
                Token::Plus(e)
                | Token::Minus(e)
                | Token::Star(e)
                | Token::Slash(e)
                | Token::Percent(e)
                | Token::IncOp(e)
                | Token::DecOp(e)
                | Token::AddAssign(e)
                | Token::SubAssign(e)
                | Token::EqOp(e)
                | Token::NeOp(e)
                | Token::LeOp(e)
                | Token::GeOp(e)
                | Token::AndOp(e)
                | Token::OrOp(e)
                | Token::Semicolon(e)
                | Token::Comma(e)
                | Token::LBrace(e)
                | Token::RBrace(e) => e.lexeme.clone(),
                _ => "".to_string(),
            })
            .collect();

        assert_eq!(
            lexemes,
            vec![
                "+", "-", "*", "/", "%", "++", "--", "+=", "-=", "==", "!=", "<=", ">=", "&&",
                "||", ";", ",", "{", "}"
            ]
        );
    }

    #[test]
    fn test_comments_and_line_tracking() {
        let src = "int a; // variable\n/* multi\nline comment */\nfloat b;";
        let mut lexer = Lexer::new(src);
        let tokens: Vec<Token> = lexer.iter().filter_map(|res| res.ok()).collect();

        let expected: Vec<(&str, usize, usize)> = vec![
            ("int", 1, 1),
            ("a", 1, 5),
            (";", 1, 6),
            ("float", 4, 1),
            ("b", 4, 7),
            (";", 4, 8),
        ];

        for (token, (lexeme, line, column)) in tokens.iter().zip(expected.iter()) {
            match token {
                Token::Int(ex)
                | Token::Float(ex)
                | Token::Identifier(ex)
                | Token::Semicolon(ex) => {
                    assert_eq!(&ex.lexeme, lexeme);
                    assert_eq!(ex.line, *line);
                    assert_eq!(ex.column, *column);
                }
                _ => panic!("Unexpected token {:?}", token),
            }
        }

        assert_eq!(tokens.len(), expected.len());
    }

    #[test]
    fn test_unprocessed_directive_error() {
        let src = "#include <stdio.h>";
        let mut lexer = Lexer::new(src);

        let token_opt = lexer.iter().next();
        assert!(token_opt.is_some(), "Expected a token from the lexer");

        let token_res = token_opt.unwrap();
        assert!(
            token_res.is_err(),
            "Expected an error for unprocessed directive"
        );

        let err = token_res.err().unwrap();
        assert_eq!(
            err,
            LexingError::UnprocessedDirective("#include <stdio.h>".to_string())
        );
    }

    #[test]
    fn test_unexpected_character_error() {
        let src = "@";
        let mut lexer = Token::lexer(src);

        let token_opt = lexer.next();
        assert!(token_opt.is_some(), "Expected a token from the lexer");

        let token_res = token_opt.unwrap();
        assert!(
            token_res.is_err(),
            "Expected an error for unexpected character"
        );

        let err = token_res.err().unwrap();
        assert_eq!(err, LexingError::UnexpectedCharacter('@'));
    }
}
