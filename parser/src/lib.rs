use chumsky::{input::ValueInput, prelude::*};
use lexer::*;

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

// ============================================================================
// AST Node Types
// ============================================================================

/// A translation unit (C source file) is a list of external declarations
pub type TranslationUnit = Vec<ExternalDecl>;

/// Top-level declarations (functions, global variables, etc.)
#[derive(Debug, Clone)]
pub enum ExternalDecl {
    FuncDef(FunctionDefinition),
    Decl(Declaration),
}

/// Function definition
#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
    pub body: CompoundStmt,
}

// ============================================================================
// Statements
// ============================================================================

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Labeled statement: label: stmt
    Labeled {
        label: String,
        stmt: Box<Stmt>,
    },
    /// Case label: case expr: stmt
    Case {
        expr: Expr,
        stmt: Box<Stmt>,
    },
    /// Default label: default: stmt
    Default(Box<Stmt>),
    
    /// Compound statement: { ... }
    Compound(CompoundStmt),
    
    /// Expression statement: expr;
    Expr(Option<Expr>),  // None for empty statement (just semicolon)
    
    /// If statement
    If {
        condition: Expr,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
    
    /// Switch statement
    Switch {
        condition: Expr,
        body: Box<Stmt>,
    },
    
    /// While loop
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    
    /// Do-while loop
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
    },
    
    /// For loop
    For {
        init: Option<ForInit>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Box<Stmt>,
    },
    
    /// Goto statement
    Goto(String),
    
    /// Continue statement
    Continue,
    
    /// Break statement
    Break,
    
    /// Return statement
    Return(Option<Expr>),
}

/// Compound statement (block)
#[derive(Debug, Clone)]
pub struct CompoundStmt {
    pub items: Vec<BlockItem>,
}

/// Block item (can be declaration or statement)
#[derive(Debug, Clone)]
pub enum BlockItem {
    Decl(Declaration),
    Stmt(Stmt),
}

/// For loop initializer (can be expression or declaration in C99+)
#[derive(Debug, Clone)]
pub enum ForInit {
    Expr(Expr),
    Decl(Declaration),
}

// ============================================================================
// Declarations
// ============================================================================

/// Declaration: specifiers + list of declarators
#[derive(Debug, Clone)]
pub struct Declaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarators: Vec<InitDeclarator>,
}

/// Declarator with optional initializer
#[derive(Debug, Clone)]
pub struct InitDeclarator {
    pub declarator: Declarator,
    pub initializer: Option<Initializer>,
}

/// Declaration specifiers (storage class, type qualifiers, type specifiers)
#[derive(Debug, Clone)]
pub enum DeclarationSpecifier {
    StorageClass(Option<StorageClass>),
    TypeQualifier(Option<TypeQualifier>),
    TypeSpecifier(Option<TypeSpecifier>),
    FunctionSpecifier(Option<FunctionSpecifier>),  // inline
}

/// Storage class specifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageClass {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

/// Type qualifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeQualifier {
    Const,
    Volatile,
    Restrict,  // C99
}

/// Function specifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSpecifier {
    Inline,
}

/// Type specifiers
#[derive(Debug, Clone)]
pub enum TypeSpecifier {
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,      // C99 _Bool
    Complex,   // C99 _Complex
    
    // Struct or union
    Struct(StructOrUnion),
    
    // Enum
    Enum(EnumSpecifier),
    
    // Typedef name
    TypedefName(String),
}

// Type qualifier or type specifier
pub enum TypeQualOrSpec {
  Qualifier(Option<TypeQualifier>),
  Specifier(Option<TypeSpecifier>),
}

/// Struct or union specifier
#[derive(Debug, Clone)]
pub struct StructOrUnion {
    pub kind: StructOrUnionKind,
    pub name: Option<String>,
    pub declarations: Option<Vec<StructDeclaration>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StructOrUnionKind {
    Struct,
    Union,
}

/// Struct declaration (member)
#[derive(Debug, Clone)]
pub struct StructDeclaration {
    pub specifiers: Vec<TypeSpecifier>,  // Simplified
    pub declarators: Vec<StructDeclarator>,
}

/// Struct declarator (member with optional bit-field)
#[derive(Debug, Clone)]
pub struct StructDeclarator {
    pub declarator: Declarator,
    pub bit_field: Option<Expr>,  // For bit-fields: int x : 5;
}

/// Enum specifier
#[derive(Debug, Clone)]
pub struct EnumSpecifier {
    pub name: Option<String>,
    pub enumerators: Option<Vec<Enumerator>>,
}

/// Enumerator (enum constant)
#[derive(Debug, Clone)]
pub struct Enumerator {
    pub name: String,
    pub value: Option<Expr>,  // Optional: ENUM_VAL = 42
}

// ============================================================================
// Declarators
// ============================================================================

/// Declarator (describes how to declare a variable/function)
#[derive(Debug, Clone)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct_declarator: DirectDeclarator,
}

/// Pointer (can be chained: **p)
#[derive(Debug, Clone)]
pub struct Pointer {
    pub qualifiers: Vec<TypeQualifier>,
    pub next: Option<Box<Pointer>>,
}

/// Direct declarator
#[derive(Debug, Clone)]
pub enum DirectDeclarator {
    /// Identifier: x
    Identifier(String),
    
    /// Parenthesized declarator: (declarator)
    Declarator(Box<Declarator>),
    
    /// Array: declarator[expr]
    Array {
        declarator: Box<DirectDeclarator>,
        size: Option<Expr>,
    },
    
    /// Function: declarator(params)
    Function {
        declarator: Box<DirectDeclarator>,
        params: ParameterList,
    },
}

/// Parameter list for function declarations
#[derive(Debug, Clone)]
pub struct ParameterList {
    pub params: Vec<ParameterDeclaration>,
    pub variadic: bool,  // true for ... (varargs)
}

/// Parameter declaration
#[derive(Debug, Clone)]
pub struct ParameterDeclaration {
    pub specifiers: Vec<DeclarationSpecifier>,
    pub declarator: Option<Declarator>,  // Can be abstract (unnamed)
}

// ============================================================================
// Initializers
// ============================================================================

/// Initializer
#[derive(Debug, Clone)]
pub enum Initializer {
    /// Single expression: = 5
    Expr(Expr),
    
    /// List of initializers: = {1, 2, 3}
    List(Vec<InitializerListItem>),
}

/// Initializer list item (with optional designation for C99)
#[derive(Debug, Clone)]
pub struct InitializerListItem {
    pub designation: Option<Designation>,
    pub initializer: Initializer,
}

/// Designation (for designated initializers in C99)
#[derive(Debug, Clone)]
pub struct Designation {
    pub designators: Vec<Designator>,
}

/// Designator
#[derive(Debug, Clone)]
pub enum Designator {
    /// Array index: [5]
    Index(Expr),
    
    /// Struct member: .field
    Member(String),
}

// ============================================================================
// Expressions
// ============================================================================

#[derive(Debug, Clone)]
pub enum Expr {
    // Literals - values known at compile time
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    CharLiteral(char),
    
    // Variable access - needs symbol lookup
    Identifier(String),
    
    // Binary operations
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    
    // Unary operations (prefix)
    UnaryOp {
        op: UnaryOperator,
        operand: Box<Expr>,
    },
    
    // Postfix operations
    PostfixOp {
        op: PostfixOperator,
        operand: Box<Expr>,
    },
    
    // Array access: arr[index]
    ArrayAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    
    // Function call: func(args)
    FunctionCall {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
    
    // Member access: obj.member
    MemberAccess {
        object: Box<Expr>,
        member: String,
    },
    
    // Pointer member access: ptr->member
    PointerMemberAccess {
        object: Box<Expr>,
        member: String,
    },
    
    // Cast: (type)expr
    Cast {
        type_name: Box<TypeName>,
        expr: Box<Expr>,
    },
    
    // Ternary/conditional: cond ? then : else
    TernaryOp {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    
    // Assignment: lvalue = rvalue
    Assignment {
        op: AssignmentOperator,
        lvalue: Box<Expr>,
        rvalue: Box<Expr>,
    },
    
    // Comma operator: expr1, expr2
    Sequence(Vec<Box<Expr>>),
    
    // Sizeof operator
    SizeofType(Box<TypeName>),
    SizeofExpr(Box<Expr>),
    
    // Compound literal (C99): (type){initializers}
    CompoundLiteral {
        type_name: Box<TypeName>,
        initializers: Vec<Box<Initializer>>,
    },
}

/// Type name (used in casts, sizeof, etc.)
#[derive(Debug, Clone)]
pub struct TypeName {
    pub specifiers: Vec<TypeSpecifier>,
    pub qualifiers: Vec<TypeQualifier>,
    pub declarator: Option<AbstractDeclarator>,
}

/// Abstract declarator (declarator without identifier)
#[derive(Debug, Clone)]
pub struct AbstractDeclarator {
    pub pointer: Option<Pointer>,
    pub direct: Option<DirectAbstractDeclarator>,
}

/// Direct abstract declarator
#[derive(Debug, Clone)]
pub enum DirectAbstractDeclarator {
    /// Parenthesized: (abstract_declarator)
    Declarator(Box<AbstractDeclarator>),
    
    /// Array: [size]
    Array {
        declarator: Option<Box<DirectAbstractDeclarator>>,
        size: Option<Expr>,
    },
    
    /// Function: (params)
    Function {
        declarator: Option<Box<DirectAbstractDeclarator>>,
        params: ParameterList,
    },
}

// ============================================================================
// Operators
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,           // +
    Sub,           // -
    Mul,           // *
    Div,           // /
    Mod,           // %
    
    // Bitwise
    BitwiseAnd,    // &
    BitwiseOr,     // |
    BitwiseXor,    // ^
    ShiftLeft,     // 
    ShiftRight,    // >>
    
    // Logical
    LogicalAnd,    // &&
    LogicalOr,     // ||
    
    // Comparison
    Equal,         // ==
    NotEqual,      // !=
    LessThan,      // 
    LessEqual,     // <=
    GreaterThan,   // >
    GreaterEqual,  // >=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    // Prefix increment/decrement
    PreIncrement,   // ++x
    PreDecrement,   // --x
    
    // Pointer operations
    Dereference,    // *x
    AddressOf,      // &x
    
    // Arithmetic
    Plus,           // +x
    Minus,          // -x
    
    // Bitwise/Logical
    BitwiseNot,     // ~x
    LogicalNot,     // !x
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOperator {
    Increment,  // x++
    Decrement,  // x--
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,              // =
    AddAssign,           // +=
    SubAssign,           // -=
    MulAssign,           // *=
    DivAssign,           // /=
    ModAssign,           // %=
    BitwiseAndAssign,    // &=
    BitwiseOrAssign,     // |=
    BitwiseXorAssign,    // ^=
    ShiftLeftAssign,     // <<=
    ShiftRightAssign,    // >>=
}

pub fn parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  primary_expression()
    .repeated()
    .collect()
}

// *****************************************************************************************************************************
// Helper parsers for matching Tokens such as RParent, LBrace (needed due to Extras being data in Tokens tagged union)
// TODO find another way to do this?
// *****************************************************************************************************************************
macro_rules! token_parser {
    ($name:ident, $token:path) => {
        fn $name<'tokens, I>() -> impl Parser<'tokens, I, (), extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
        where
            I: ValueInput<'tokens, Token = Token, Span = Span>,
        {
            select! {
                $token(_) => (),
            }
        }
    };
}
token_parser!(left_paren, Token::LParen);
token_parser!(right_paren, Token::RParen);
token_parser!(left_bracket, Token::LBracket);
token_parser!(right_bracket, Token::RBracket);
token_parser!(left_brace, Token::LBrace);
token_parser!(right_brace, Token::RBrace);
token_parser!(left_op, Token::LeftOp);
token_parser!(right_op, Token::RightOp);
token_parser!(dot, Token::Dot);
token_parser!(amp, Token::Amp);
token_parser!(star, Token::Star);
token_parser!(plus, Token::Plus);
token_parser!(minus, Token::Minus);
token_parser!(inc, Token::IncOp);
token_parser!(dec, Token::DecOp);
token_parser!(tilde, Token::Tilde);
token_parser!(bang, Token::Bang);
token_parser!(slash, Token::Slash);
token_parser!(percent, Token::Percent);
token_parser!(eq, Token::EqOp);
token_parser!(ne, Token::NeOp);
token_parser!(lt, Token::Lt);
token_parser!(gt, Token::Gt);
token_parser!(lte, Token::LeOp);
token_parser!(gte, Token::GeOp);
token_parser!(and_op, Token::AndOp);
token_parser!(or_op, Token::OrOp);
token_parser!(caret, Token::Caret);
token_parser!(pipe, Token::Pipe);
token_parser!(question, Token::Question);
token_parser!(colon, Token::Colon);
token_parser!(semicolon, Token::Semicolon);
token_parser!(comma, Token::Comma);
token_parser!(assign, Token::Assign);
token_parser!(ptr_op, Token::PtrOp);

fn identifier<'tokens, I>() -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<chumsky::error::Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Identifier(extra) => (Expr::Identifier(extra.lexeme.clone()), span_from_extra(extra))
    }
}

macro_rules! extract_identifier {
    ($expr:expr) => {
        match $expr {
            Expr::Identifier(name) => name,
            _ => panic!("Expected Expr::Identifier, got {:?}", $expr),
        }
    };
}

// *****************************************************************************************************************************
// Following parsers follow structure and order from https://www.quut.com/c/ANSI-C-grammar-y-2011.html#declarator
// *****************************************************************************************************************************

fn primary_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  choice((
    identifier(),
    constant(),
    string(),
    expression().delimited_by(left_paren(), right_paren())
  ))
}

fn constant<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::IntegerConstant(extra) => {
            let value = extra.lexeme.parse::<i32>().unwrap();
            (Expr::IntLiteral(value), span_from_extra(extra))
        },
        Token::FloatConstant(extra) => {
            let value = extra.lexeme.parse::<f32>().unwrap();
            (Expr::FloatLiteral(value), span_from_extra(extra))
        },
    }
}

fn string<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::StringLiteral(extra) => (Expr::StringLiteral(extra.clone().lexeme), span_from_extra(extra)),
    // the following is used for __func__ debugging thingy
    // Token::FuncName(extra) => (Expr::(extra.clone().lexeme), span_from_extra(extra)),
  }
}

// TODO fix all the spans that do not include the extra stuff after
pub fn postfix_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|postfix| {
    choice((
      primary_expression(),

      // array access
      postfix.clone()
        .then(expression().delimited_by(left_bracket(), right_bracket()))
        .map(|((array, array_span), (index, _index_span))| {
            (Expr::ArrayAccess {
                array: Box::new(array),
                index: Box::new(index),
            }, array_span)
        }),

      // function call w/o arguments
      postfix.clone()
        .then_ignore(left_paren())
        .then_ignore(right_paren())
        .map(|(function, function_span)| (Expr::FunctionCall { 
            function: Box::new(function), 
            args: Vec::new() 
          }, function_span)),

      // function call w/ arguments
      postfix.clone()
        .then(argument_expression_list().delimited_by(left_paren(), right_paren()))
        .map(|((function, function_span), args)| (Expr::FunctionCall { 
            function: Box::new(function), 
            args: args.into_iter().map(|(expr, _span)| expr).collect(), // TODO this might need to change to boxed?
          }, function_span)),

      // member access
      postfix.clone()
        .then_ignore(dot())
        .then(identifier())
        .map(|((identifier, identifier_span), (member, _member_span))| {
            (Expr::MemberAccess { 
              object: Box::new(identifier), 
              member: extract_identifier!(member) 
            }, identifier_span)
        }),
      
      // pointer member operation
      postfix.clone()
        .then_ignore(ptr_op())
        .then(identifier())
        .map(|((identifier, identifier_span), (member, _member_span))| {
            (Expr::MemberAccess { 
              object: Box::new(identifier), 
              member: extract_identifier!(member) 
            }, identifier_span)
        }),

      // increment operation
      postfix.clone()
        .then_ignore(inc())
        .map(|(identifier, identifier_span)| {
            (Expr::PostfixOp { 
              op: PostfixOperator::Increment,
              operand: Box::new(identifier)
            }, identifier_span)
        }),

      // decrement operation
      postfix.clone()
        .then_ignore(dec())
        .map(|(identifier, identifier_span)| {
            (Expr::PostfixOp { 
              op: PostfixOperator::Decrement,
              operand: Box::new(identifier)
            }, identifier_span)
        }),
      
      // compound literals
      left_paren()
        .ignore_then(type_name())
        .then_ignore(right_paren().then(left_brace()))
        .then(initializer_list())
        .then_ignore(comma().or_not())
        .then_ignore(right_brace())
        .map(|((type_name, type_name_span), initializers)| {
          (Expr::CompoundLiteral { 
            type_name: Box::new(type_name), 
            initializers: initializers.into_iter().map(|(initializer, _span)| Box::new(initializer)).collect()
          }, type_name_span)
        })
    ))
  })
}

fn argument_expression_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  assignment_expression()
    .separated_by(comma())
    .at_least(1)
    .collect()
}

fn unary_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|unary| {
    choice((
      postfix_expression(),
      
      // preincrement
      inc()
        .ignore_then(unary.clone())
        .map(|(unary, unary_span)| {
          (Expr::UnaryOp { 
            op: UnaryOperator::PreIncrement, 
            operand: Box::new(unary)
          }, unary_span)
        }),

      // predecrement
      dec()
        .ignore_then(unary.clone())
        .map(|(unary, unary_span)| {
          (Expr::UnaryOp { 
            op: UnaryOperator::PreDecrement, 
            operand: Box::new(unary)
          }, unary_span)
        }),
      
      // cast expression
      unary_operator()
        .then(cast_expression())
        .map(|((unary_op, unary_op_span), (cast, _cast_span))| {
          if let Expr::Cast{type_name, expr} = cast {
            (Expr::UnaryOp { 
              op: unary_op, 
              operand: Box::new(Expr::Cast { 
                type_name: type_name, 
                expr: expr
              })
            }, unary_op_span)
          } else {
            panic!("expected Cast expr!")
          }
        }),

      // TODO SIZEOF and ALIGNOF
    ))
  })
}

fn unary_operator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<UnaryOperator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::Amp(extra) => (UnaryOperator::AddressOf, span_from_extra(extra)),
    Token::Star(extra) => (UnaryOperator::Dereference, span_from_extra(extra)),
    Token::Plus(extra) => (UnaryOperator::Plus, span_from_extra(extra)),
    Token::Minus(extra) => (UnaryOperator::Minus, span_from_extra(extra)),
    Token::Tilde(extra) => (UnaryOperator::BitwiseNot, span_from_extra(extra)),
    Token::Bang(extra) => (UnaryOperator::LogicalNot, span_from_extra(extra)),
  }
}

fn cast_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cast_expr| {
    choice((
      unary_expression(),
      left_paren()
        .ignore_then(type_name())
        .then_ignore(right_paren())
        .then(cast_expr.clone())
        .map(|((type_name, type_name_span), (cast_expr, _cast_expr_span))| {
          (Expr::Cast { 
            type_name: Box::new(type_name), 
            expr: Box::new(cast_expr)
          }, type_name_span)
        })
    ))
  })
}


fn multiplicative_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|mult| {
    choice((
      cast_expression(),
      mult.clone()
        .then_ignore(star())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Mul, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
      mult.clone()
        .then_ignore(slash())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Div, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
      mult.clone()
        .then_ignore(percent())
        .then(cast_expression())
        .map(|((mult_expr, mult_span), (cast_expr, _cast_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Mod, 
            left: Box::new(mult_expr), 
            right: Box::new(cast_expr) 
          }, mult_span)
        }),
    ))
  })
}

fn additive_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|additive| {
    choice((
      multiplicative_expression(),
      additive.clone()
        .then_ignore(plus())
        .then(multiplicative_expression())
        .map(|((add_expr, add_span), (mult_expr, _mult_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Add, 
            left: Box::new(add_expr), 
            right: Box::new(mult_expr) 
          }, add_span)
        }),
      additive.clone()
        .then_ignore(minus())
        .then(multiplicative_expression())
        .map(|((add_expr, add_span), (mult_expr, _mult_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Sub, 
            left: Box::new(add_expr), 
            right: Box::new(mult_expr) 
          }, add_span)
        }),
    ))
  })
}

fn shift_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|shift| {
    choice((
      additive_expression(),
      shift.clone()
        .then_ignore(left_op())
        .then(additive_expression())
        .map(|((shift_expr, shift_span), (add_expr, _add_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::ShiftLeft, 
            left: Box::new(shift_expr), 
            right: Box::new(add_expr) 
          }, shift_span)
        }),
      shift.clone()
        .then_ignore(right_op())
        .then(additive_expression())
        .map(|((shift_expr, shift_span), (add_expr, _add_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::ShiftRight, 
            left: Box::new(shift_expr), 
            right: Box::new(add_expr) 
          }, shift_span)
        }),
    ))
  })
}

fn relational_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|relational| {
    choice((
      shift_expression(),
      relational.clone()
        .then_ignore(lt())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LessThan, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(gt())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::GreaterThan, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(lte())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LessEqual, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
      relational.clone()
        .then_ignore(gte())
        .then(shift_expression())
        .map(|((rel_expr, rel_span), (shift_expr, _shift_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::GreaterEqual, 
            left: Box::new(rel_expr), 
            right: Box::new(shift_expr) 
          }, rel_span)
        }),
    ))
  })
}

fn equality_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|equality| {
    choice((
      relational_expression(),
      equality.clone()
        .then_ignore(eq())
        .then(relational_expression())
        .map(|((eq_expr, eq_span), (rel_expr, _rel_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::Equal, 
            left: Box::new(eq_expr), 
            right: Box::new(rel_expr) 
          }, eq_span)
        }),
      equality.clone()
        .then_ignore(ne())
        .then(relational_expression())
        .map(|((eq_expr, eq_span), (rel_expr, _rel_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::NotEqual, 
            left: Box::new(eq_expr), 
            right: Box::new(rel_expr) 
          }, eq_span)
        }),
    ))
  })
}

fn and_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|and| {
    choice((
      equality_expression(),
      and.clone()
        .then_ignore(amp())
        .then(equality_expression())
        .map(|((and_expr, and_span), (eq_expr, _eq_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::BitwiseAnd, 
            left: Box::new(and_expr), 
            right: Box::new(eq_expr) 
          }, and_span)
        }),
    ))
  })
}

fn exclusive_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
    choice((
      and_expression(),
      or.clone()
        .then_ignore(caret())
        .then(and_expression())
        .map(|((or_expr, or_span), (and_expr, _and_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::BitwiseXor, 
            left: Box::new(or_expr), 
            right: Box::new(and_expr) 
          }, or_span)
        }),
    ))
  })
}

fn inclusive_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
    choice((
      and_expression(),
      or.clone()
        .then_ignore(pipe())
        .then(and_expression())
        .map(|((or_expr, or_span), (and_expr, _and_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::BitwiseOr, 
            left: Box::new(or_expr), 
            right: Box::new(and_expr) 
          }, or_span)
        }),
    ))
  })
}

fn logical_and_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|and| {
    choice((
      inclusive_or_expression(),
      and.clone()
        .then_ignore(and_op())
        .then(inclusive_or_expression())
        .map(|((and_expr, and_span), (or_expr, _or_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LogicalAnd, 
            left: Box::new(and_expr), 
            right: Box::new(or_expr) 
          }, and_span)
        }),
    ))
  })
}

fn logical_or_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|or| {
    choice((
      logical_and_expression(),
      or.clone()
        .then_ignore(or_op())
        .then(logical_and_expression())
        .map(|((or_expr, or_span), (and_expr, _and_span))| {
          (Expr::BinaryOp { 
            op: BinaryOperator::LogicalOr, 
            left: Box::new(or_expr), 
            right: Box::new(and_expr) 
          }, or_span)
        }),
    ))
  })
}

fn conditional_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|cond| {
    choice((
      logical_or_expression(),
      logical_or_expression()
        .then_ignore(question())
        .then(expression())
        .then_ignore(colon())
        .then(cond.clone())
        // NOTE: the names in this map are a bit confusion due to the name of the grammar rules vs member names in the TernaryOp
        .map(|(((or_expr, or_span), (expr, _expr_span)), (cond_expr, _cond_span))| {
          (Expr::TernaryOp { 
            condition: Box::new(or_expr), 
            then_expr: Box::new(expr), 
            else_expr: Box::new(cond_expr)
          }, or_span)
        }),
    ))
  })
}

fn assignment_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  recursive::<_, _, extra::Err<Rich<'tokens, Token, Span>>, _, _>(|assign_expr| {
    choice((
      conditional_expression(),
      unary_expression()
        .then(assignment_operator())
        .then(assign_expr.clone())
        .map(|(((cond_expr, cond_span), (assign_op, _assign_op_span)), (assign_expr, _assign_expr_span))| {
          (Expr::Assignment { 
            op: assign_op, 
            lvalue: Box::new(cond_expr), 
            rvalue: Box::new(assign_expr) 
          }, cond_span)
        }),
    ))
  })
}

fn assignment_operator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<AssignmentOperator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::EqOp(extra) => (AssignmentOperator::Assign, span_from_extra(extra)),
    Token::AddAssign(extra) => (AssignmentOperator::AddAssign, span_from_extra(extra)),
    Token::SubAssign(extra) => (AssignmentOperator::SubAssign, span_from_extra(extra)),
    Token::MulAssign(extra) => (AssignmentOperator::MulAssign, span_from_extra(extra)),
    Token::DivAssign(extra) => (AssignmentOperator::DivAssign, span_from_extra(extra)),
    Token::ModAssign(extra) => (AssignmentOperator::ModAssign, span_from_extra(extra)),
    Token::AndAssign(extra) => (AssignmentOperator::BitwiseAndAssign, span_from_extra(extra)),
    Token::OrAssign(extra) => (AssignmentOperator::BitwiseOrAssign, span_from_extra(extra)),
    Token::XorAssign(extra) => (AssignmentOperator::BitwiseXorAssign, span_from_extra(extra)),
    Token::LeftAssign(extra) => (AssignmentOperator::ShiftLeftAssign, span_from_extra(extra)),
    Token::RightAssign(extra) => (AssignmentOperator::ShiftRightAssign, span_from_extra(extra)),
  }
}

fn expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  assignment_expression()
    .separated_by(comma())
    .at_least(1)
    .collect()
    .map(|list: Vec<Spanned<Expr>>| {
      (Expr::Sequence(list.clone().into_iter().map(|(expr, _span)| Box::new(expr)).collect()), list.clone()[0].1)
    })
}

fn constant_expression<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  conditional_expression()
}

fn declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Declaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  choice((
    declaration_specifiers()
      .then_ignore(semicolon()),
    declaration_specifiers()
      .then(init_declarator_list())
      .then_ignore(semicolon())
      .map(|((dec_expr, dec_span), (list_expr, list_span))| {
        
      }),
    static_assert_declaration()
  ))
}

fn declaration_specifiers<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<DeclarationSpecifiers>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn init_declarator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn init_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn storage_class_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<StorageClass>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::Auto(extra) => (StorageClass::Auto, span_from_extra(extra)),
    Token::Register(extra) => (StorageClass::Register, span_from_extra(extra)),
    Token::Static(extra) => (StorageClass::Static, span_from_extra(extra)),
    Token::Extern(extra) => (StorageClass::Extern, span_from_extra(extra)),
    Token::Typedef(extra) => (StorageClass::Typedef, span_from_extra(extra)),
  } 
}

fn type_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  choice((
    select! {
      Token::Void(extra) => (TypeSpecifier::Void, span_from_extra(extra)),
      Token::Char(extra) => (TypeSpecifier::Char, span_from_extra(extra)),
      Token::Short(extra) => (TypeSpecifier::Short, span_from_extra(extra)),
      Token::Int(extra) => (TypeSpecifier::Int, span_from_extra(extra)),
      Token::Long(extra) => (TypeSpecifier::Long, span_from_extra(extra)),
      Token::Float(extra) => (TypeSpecifier::Float, span_from_extra(extra)),
      Token::Double(extra) => (TypeSpecifier::Double, span_from_extra(extra)),
      Token::Signed(extra) => (TypeSpecifier::Signed, span_from_extra(extra)),
      Token::Unsigned(extra) => (TypeSpecifier::Unsigned, span_from_extra(extra)),
      Token::Bool(extra) => (TypeSpecifier::Bool, span_from_extra(extra)),
      Token::Complex(extra) => (TypeSpecifier::Complex, span_from_extra(extra)),
    },
    atomic_type_specifier(),
    struct_or_union_specifier(),
    enum_specifier(),
  ))
}

fn struct_or_union_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  // : struct_or_union '{' struct_declaration_list '}'
	// | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	// | struct_or_union IDENTIFIER
	// ;
  struct_or_union()
}

fn struct_or_union<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<StructOrUnionKind>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  select! {
    Token::Struct(extra) => (StructOrUnionKind::Struct, span_from_extra(extra)),
    Token::Union(extra) => (StructOrUnionKind::Union, span_from_extra(extra)),
  }
}

fn struct_declaration_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<StructDeclaration>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  struct_declaration()
    .separated_by(comma())
    .at_least(1)
    .collect()
}

fn struct_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<StructDeclaration>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  choice((
    specifier_qualifier_list()
      .then_ignore(semicolon())
      .map(|specifiers| {
        (StructDeclaration{
            specifiers: specifiers.clone().into_iter().map(|(specifier, _span)| specifier).collect(),
            declarators: Vec::new(),
          }, specifiers.clone()[0].1)
      }),
    specifier_qualifier_list()
      .then(struct_declarator_list())
      .then_ignore(semicolon())
      .map(|(specifiers, declarators)| {
        (StructDeclaration{
            specifiers: specifiers.clone().into_iter().map(|(specifier, _span)| specifier).collect(),
            declarators: declarators.into_iter().map(|(declarator, _span)| declarator).collect(),
          }, specifiers.clone()[0].1)
      }),
    // static_assert_declaration(), TODO maybe add this someday
  ))   
}

fn specifier_qualifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<TypeQualOrSpec>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  type_specifier()
    .map(|(specifier, span)| (TypeQualOrSpec {specifier: specifier}, span))
    .or(
      type_qualifier()
        .map(|(qualifier, span)| (TypeQualOrSpec {qualifier: qualifier}, span))
      )
    .separated_by(comma())
    .at_least(1)
    .collect()
}

fn struct_declarator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<StructDeclarator>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  struct_declarator()
    .separated_by(comma())
    .at_least(1)
    .collect()
}

fn struct_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<StructDeclarator>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enum_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enumerator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn enumerator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn atomic_type_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeSpecifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_qualifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeQualifier>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn function_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn alignment_specifier<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn direct_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn pointer<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_qualifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<TypeQualifier>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_type_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn parameter_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn identifier_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Expr>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn type_name<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<TypeName>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn abstract_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn direct_abstract_declarator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn initializer<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn initializer_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Spanned<Initializer>>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designation<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designator_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn designator<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn static_assert_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn labeled_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn compound_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn block_item_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn block_item<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn expression_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn selection_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn iteration_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn jump_statement<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn translation_unit<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn external_declaration<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn function_definition<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn declaration_list<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Spanned<Expr>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
  
}

fn span_from_extra(extra: Extras) -> Span {
  Span::new((), extra.column..extra.column+extra.lexeme.len())
}