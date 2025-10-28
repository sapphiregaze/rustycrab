// ============================================================================
// AST Node Types
// ============================================================================

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
    pub declarations: Vec<Declaration>,
    pub body: CompoundStmt,
}

// ============================================================================
// Statements
// ============================================================================

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Labeled statement: label: stmt
    Labeled { label: String, stmt: Box<Stmt> },
    /// Case label: case expr: stmt
    Case { expr: Expr, stmt: Box<Stmt> },
    /// Default label: default: stmt
    Default(Box<Stmt>),

    /// Compound statement: { ... }
    Compound(CompoundStmt),

    /// Expression statement: expr;
    Expr(Option<Expr>), // None for empty statement (just semicolon)

    /// If statement
    If { condition: Expr, then_stmt: Box<Stmt>, else_stmt: Option<Box<Stmt>> },

    /// Switch statement
    Switch { condition: Expr, body: Box<Stmt> },

    /// While loop
    While { condition: Expr, body: Box<Stmt> },

    /// Do-while loop
    DoWhile { body: Box<Stmt>, condition: Expr },

    /// For loop
    For { init: Option<ForInit>, condition: Option<Expr>, increment: Option<Expr>, body: Box<Stmt> },

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
    StorageClass(StorageClass),
    TypeQualifier(TypeQualifier),
    TypeSpecifier(TypeSpecifier),
    FunctionSpecifier(FunctionSpecifier), // inline
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
    Restrict, // C99
    Atomic,
}

/// Function specifiers
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionSpecifier {
    Inline,
    NoReturn,
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
    Bool,    // C99 _Bool
    Complex, // C99 _Complex

    // Struct or union
    Struct(StructOrUnion),

    // Enum
    Enum(EnumSpecifier),

    // Typedef name
    TypedefName(String),
}

// Type qualifier or type specifier
// TODO see if there is some way to make this a union
#[derive(Debug, Clone)]
pub enum TypeQualOrSpec {
    Qualifier(TypeQualifier),
    Specifier(TypeSpecifier),
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
    pub specifiers: Vec<TypeSpecifier>, // Simplified
    pub declarators: Vec<StructDeclarator>,
}

/// Struct declarator (member with optional bit-field)
#[derive(Debug, Clone)]
pub struct StructDeclarator {
    pub declarator: Declarator,
    pub bit_field: Option<Expr>, // For bit-fields: int x : 5;
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
    pub value: Option<Expr>, // Optional: ENUM_VAL = 42
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
    Array { declarator: Box<DirectDeclarator>, array_type: ArrayDeclaratorType },

    /// Function: declarator(params)
    Function { declarator: Box<DirectDeclarator>, params: Option<ParameterList> },

    /// Function: declarator(arguments)
    FunctionCall { declarator: Box<DirectDeclarator>, params: Option<Vec<Box<Expr>>> },
}

#[derive(Debug, Clone)]
pub enum ArrayDeclaratorType {
    /// Empty: []
    Empty,

    /// Variable length: [*]
    VariableLength { qualifiers: Vec<TypeQualifier> },

    /// Regular array with size: [expr]
    Size { qualifiers: Vec<TypeQualifier>, size: Box<Expr> },

    /// Static array: [static expr]
    Static { qualifiers: Vec<TypeQualifier>, size: Box<Expr> },

    /// Just qualifiers: [const]
    Qualifiers(Vec<TypeQualifier>),
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
    /// Parenthesized declarator: (declarator)
    Declarator(Box<AbstractDeclarator>),

    /// Array: declarator[expr]
    Array { declarator: Option<Box<DirectAbstractDeclarator>>, array_type: ArrayDeclaratorType },

    /// Function: declarator(params)
    Function { declarator: Option<Box<DirectAbstractDeclarator>>, params: Option<ParameterList> },

    /// Function: declarator(arguments)
    FunctionCall { declarator: Option<Box<DirectAbstractDeclarator>>, params: Option<Vec<Box<Expr>>> },
}

/// Parameter list for function declarations
#[derive(Debug, Clone)]
pub struct ParameterList {
    pub params: Vec<ParameterDeclaration>,
    pub variadic: bool, // true for ... (varargs)
}

/// Parameter declaration
#[derive(Debug, Clone)]
pub struct ParameterDeclaration {
    pub specifiers: Vec<DeclarationSpecifier>,    // Changed from specifiers
    pub declarator: Option<DeclaratorOrAbstract>, // Changed type
}

#[derive(Debug, Clone)]
pub enum DeclaratorOrAbstract {
    Declarator(Declarator),       // int x
    Abstract(AbstractDeclarator), // int *
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
    BinaryOp { op: BinaryOperator, left: Box<Expr>, right: Box<Expr> },

    // Unary operations (prefix)
    UnaryOp { op: UnaryOperator, operand: Box<Expr> },

    // Postfix operations
    PostfixOp { op: PostfixOperator, operand: Box<Expr> },

    // Array access: arr[index]
    ArrayAccess { array: Box<Expr>, index: Box<Expr> },

    // Function call: func(args)
    FunctionCall { function: Box<Expr>, args: Vec<Expr> },

    // Member access: obj.member
    MemberAccess { object: Box<Expr>, member: String },

    // Pointer member access: ptr->member
    PointerMemberAccess { object: Box<Expr>, member: String },

    // Cast: (type)expr
    Cast { type_name: Box<TypeName>, expr: Box<Expr> },

    // Ternary/conditional: cond ? then : else
    TernaryOp { condition: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr> },

    // Assignment: lvalue = rvalue
    Assignment { op: AssignmentOperator, lvalue: Box<Expr>, rvalue: Box<Expr> },

    // Comma operator: expr1, expr2
    Sequence(Vec<Box<Expr>>),

    // Sizeof operator
    SizeofType(Box<TypeName>),
    SizeofExpr(Box<Expr>),

    // Compound literal (C99): (type){initializers}
    CompoundLiteral { type_name: Box<TypeName>, initializers: Vec<InitializerListItem> },
}

/// Type name (used in casts, sizeof, etc.)
#[derive(Debug, Clone)]
pub struct TypeName {
    pub specifiers: Vec<TypeSpecifier>,
    pub qualifiers: Vec<TypeQualifier>,
    pub declarator: Option<AbstractDeclarator>,
}

// ============================================================================
// Operators
// ============================================================================

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    // Bitwise
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    ShiftLeft,  //
    ShiftRight, // >>

    // Logical
    LogicalAnd, // &&
    LogicalOr,  // ||

    // Comparison
    Equal,        // ==
    NotEqual,     // !=
    LessThan,     //
    LessEqual,    // <=
    GreaterThan,  // >
    GreaterEqual, // >=
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    // Prefix increment/decrement
    PreIncrement, // ++x
    PreDecrement, // --x

    // Pointer operations
    Dereference, // *x
    AddressOf,   // &x

    // Arithmetic
    Plus,  // +x
    Minus, // -x

    // Bitwise/Logical
    BitwiseNot, // ~x
    LogicalNot, // !x
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOperator {
    Increment, // x++
    Decrement, // x--
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignmentOperator {
    Assign,           // =
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    ModAssign,        // %=
    BitwiseAndAssign, // &=
    BitwiseOrAssign,  // |=
    BitwiseXorAssign, // ^=
    ShiftLeftAssign,  // <<=
    ShiftRightAssign, // >>=
}
