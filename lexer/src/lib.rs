use logos::{Lexer, Logos, Skip};

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self { line: 1, column: 0 }
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(extras = Position)]
#[logos(skip(r"\n", newline_callback))]
#[logos(skip r"[ \t\v\f]")]
pub enum Token {
    #[token("auto", token_callback)]
    Auto((usize, usize)),
    #[token("break", token_callback)]
    Break((usize, usize)),
    #[token("case", token_callback)]
    Case((usize, usize)),
    #[token("char", token_callback)]
    Char((usize, usize)),
    #[token("const", token_callback)]
    Const((usize, usize)),
    #[token("continue", token_callback)]
    Continue((usize, usize)),
    #[token("default", token_callback)]
    Default((usize, usize)),
    #[token("do", token_callback)]
    Do((usize, usize)),
    #[token("double", token_callback)]
    Double((usize, usize)),
    #[token("else", token_callback)]
    Else((usize, usize)),
    #[token("enum", token_callback)]
    Enum((usize, usize)),
    #[token("extern", token_callback)]
    Extern((usize, usize)),
    #[token("float", token_callback)]
    Float((usize, usize)),
    #[token("for", token_callback)]
    For((usize, usize)),
    #[token("goto", token_callback)]
    Goto((usize, usize)),
    #[token("if", token_callback)]
    If((usize, usize)),
    #[token("inline", token_callback)]
    Inline((usize, usize)),
    #[token("int", token_callback)]
    Int((usize, usize)),
    #[token("long", token_callback)]
    Long((usize, usize)),
    #[token("register", token_callback)]
    Register((usize, usize)),
    #[token("restrict", token_callback)]
    Restrict((usize, usize)),
    #[token("return", token_callback)]
    Return((usize, usize)),
    #[token("short", token_callback)]
    Short((usize, usize)),
    #[token("signed", token_callback)]
    Signed((usize, usize)),
    #[token("sizeof", token_callback)]
    Sizeof((usize, usize)),
    #[token("static", token_callback)]
    Static((usize, usize)),
    #[token("struct", token_callback)]
    Struct((usize, usize)),
    #[token("switch", token_callback)]
    Switch((usize, usize)),
    #[token("typedef", token_callback)]
    Typedef((usize, usize)),
    #[token("union", token_callback)]
    Union((usize, usize)),
    #[token("unsigned", token_callback)]
    Unsigned((usize, usize)),
    #[token("void", token_callback)]
    Void((usize, usize)),
    #[token("volatile", token_callback)]
    Volatile((usize, usize)),
    #[token("while", token_callback)]
    While((usize, usize)),

    #[token("_Alignas", token_callback)]
    Alignas((usize, usize)),
    #[token("_Alignof", token_callback)]
    Alignof((usize, usize)),
    #[token("_Atomic", token_callback)]
    Atomic((usize, usize)),
    #[token("_Bool", token_callback)]
    Bool((usize, usize)),
    #[token("_Complex", token_callback)]
    Complex((usize, usize)),
    #[token("_Generic", token_callback)]
    Generic((usize, usize)),
    #[token("_Imaginary", token_callback)]
    Imaginary((usize, usize)),
    #[token("_Noreturn", token_callback)]
    Noreturn((usize, usize)),
    #[token("_Static_assert", token_callback)]
    StaticAssert((usize, usize)),
    #[token("_Thread_local", token_callback)]
    ThreadLocal((usize, usize)),
    #[token("__func__", token_callback)]
    FuncName((usize, usize)),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", token_callback)]
    Identifier((usize, usize)),

    #[regex(r"0[xX][0-9a-fA-F]+", token_callback)]
    HexConstant((usize, usize)),
    #[regex(r"[1-9][0-9]*", token_callback)]
    DecConstant((usize, usize)),
    #[regex(r"0[0-7]*", token_callback)]
    OctConstant((usize, usize)),
    #[regex(r"(u|U|l|L|ll|LL)*", token_callback, priority = 0)]
    IntegerSuffix((usize, usize)),

    #[regex(r#"(u|U|L)?'([^'\\\n]|\\.)+'"#, token_callback)]
    CharConstant((usize, usize)),

    #[regex(r"[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fFlL]?", token_callback)]
    FloatConstant((usize, usize)),
    #[regex(r"[0-9]+([eE][+-]?[0-9]+)[fFlL]?", token_callback)]
    FloatExpConstant((usize, usize)),
    #[regex(
        r"0[xX][0-9a-fA-F]+\.[0-9a-fA-F]*([pP][+-]?[0-9]+)?[fFlL]?",
        token_callback
    )]
    HexFloatConstant((usize, usize)),

    #[regex(r#"(u8|u|U|L)?\"([^"\\\n]|\\.)*\""#, token_callback)]
    StringLiteral((usize, usize)),

    #[token("...", token_callback)]
    Ellipsis((usize, usize)),
    #[token(">>=", token_callback)]
    RightAssign((usize, usize)),
    #[token("<<=", token_callback)]
    LeftAssign((usize, usize)),
    #[token("+=", token_callback)]
    AddAssign((usize, usize)),
    #[token("-=", token_callback)]
    SubAssign((usize, usize)),
    #[token("*=", token_callback)]
    MulAssign((usize, usize)),
    #[token("/=", token_callback)]
    DivAssign((usize, usize)),
    #[token("%=", token_callback)]
    ModAssign((usize, usize)),
    #[token("&=", token_callback)]
    AndAssign((usize, usize)),
    #[token("^=", token_callback)]
    XorAssign((usize, usize)),
    #[token("|=", token_callback)]
    OrAssign((usize, usize)),
    #[token(">>", token_callback)]
    RightOp((usize, usize)),
    #[token("<<", token_callback)]
    LeftOp((usize, usize)),
    #[token("++", token_callback)]
    IncOp((usize, usize)),
    #[token("--", token_callback)]
    DecOp((usize, usize)),
    #[token("->", token_callback)]
    PtrOp((usize, usize)),
    #[token("&&", token_callback)]
    AndOp((usize, usize)),
    #[token("||", token_callback)]
    OrOp((usize, usize)),
    #[token("<=", token_callback)]
    LeOp((usize, usize)),
    #[token(">=", token_callback)]
    GeOp((usize, usize)),
    #[token("==", token_callback)]
    EqOp((usize, usize)),
    #[token("!=", token_callback)]
    NeOp((usize, usize)),

    #[token(";", token_callback)]
    Semicolon((usize, usize)),
    #[token("{", token_callback)]
    LBrace((usize, usize)),
    #[token("}", token_callback)]
    RBrace((usize, usize)),
    #[token(",", token_callback)]
    Comma((usize, usize)),
    #[token(":", token_callback)]
    Colon((usize, usize)),
    #[token("=", token_callback)]
    Assign((usize, usize)),
    #[token("(", token_callback)]
    LParen((usize, usize)),
    #[token(")", token_callback)]
    RParen((usize, usize)),
    #[token("[", token_callback)]
    LBracket((usize, usize)),
    #[token("]", token_callback)]
    RBracket((usize, usize)),
    #[token(".", token_callback)]
    Dot((usize, usize)),
    #[token("&", token_callback)]
    Amp((usize, usize)),
    #[token("!", token_callback)]
    Bang((usize, usize)),
    #[token("~", token_callback)]
    Tilde((usize, usize)),
    #[token("-", token_callback)]
    Minus((usize, usize)),
    #[token("+", token_callback)]
    Plus((usize, usize)),
    #[token("*", token_callback)]
    Star((usize, usize)),
    #[token("/", token_callback)]
    Slash((usize, usize)),
    #[token("%", token_callback)]
    Percent((usize, usize)),
    #[token("<", token_callback)]
    Lt((usize, usize)),
    #[token(">", token_callback)]
    Gt((usize, usize)),
    #[token("^", token_callback)]
    Caret((usize, usize)),
    #[token("|", token_callback)]
    Pipe((usize, usize)),
    #[token("?", token_callback)]
    Question((usize, usize)),

    #[regex(r"/\*([^*]|\*+[^*/])*\*+/", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    Comment,
}

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras.line += 1;
    lex.extras.column = lex.span().end;
    Skip
}

fn token_callback(lex: &mut Lexer<Token>) -> (usize, usize) {
    let line = lex.extras.line;
    let column = lex.span().start.saturating_sub(lex.extras.column) + 1;
    (line, column)
}
