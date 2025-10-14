use logos::{Lexer, Logos, Skip};

#[derive(Debug, PartialEq)]
pub struct Extras {
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Default for Extras {
    fn default() -> Self {
        Self {
            lexeme: String::new(),
            line: 1,
            column: 0,
        }
    }
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

fn token_callback(lex: &mut Lexer<Token>) -> Extras {
    let lexeme = lex.slice().to_string();
    let line = lex.extras.line;
    let column = lex.span().start.saturating_sub(lex.extras.column) + 1;
    Extras {
        lexeme,
        line,
        column,
    }
}

fn comment_callback(lex: &mut Lexer<Token>) -> Skip {
    let slice = lex.slice();
    let newlines = slice.matches('\n').count();
    lex.extras.line += newlines;
    if let Some(pos) = slice.rfind('\n') {
        lex.extras.column = slice.len() - pos;
    }
    Skip
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = Extras)]
#[logos(skip(r"\n", newline_callback))]
#[logos(skip r"[ \t\v\f]")]
pub enum Token {
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

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", token_callback)]
    Identifier(Extras),

    #[regex(
        r"0[xX][a-fA-F0-9]+(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
        token_callback,
        priority = 3
    )]
    #[regex(
        r"[1-9][0-9]*(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
        token_callback,
        priority = 3
    )]
    #[regex(
        r"0[0-7]*(((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))?",
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

    #[regex(
        r#"(?m)/\*([^"*]|".*")*\*+(([^"/*]|".*")([^"*]|".*")*\*+)*/"#,
        comment_callback
    )]
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
}
