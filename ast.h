#pragma once
#include <string>
#include <ostream>
#include <vector>

struct SourcePos {
  std::string file;
  uint32_t line{0};
  uint32_t column{0};
};

struct SourceRange {
  SourcePos begin{};
  SourcePos end{};
};

struct ASTWalker {
  virtual ~ASTWalker() = default;

  // Types
  virtual void visit(BuiltinType&) {}
  virtual void visit(PointerType&) {}
  virtual void visit(ArrayType&) {}
  virtual void visit(FunctionType&) {}
  // virtual void visit(RecordType&) {}
  virtual void visit(EnumType&) {}
  virtual void visit(TypedefType&) {}

  // Expr
  virtual void visit(IntegerLiteral&) {}
  virtual void visit(FloatingLiteral&) {}
  virtual void visit(CharLiteral&) {}
  virtual void visit(StringLiteral&) {}
  virtual void visit(IdentifierExpr&) {}
  // virtual void visit(MemberExpr&) {}
  // virtual void visit(ArraySubscriptExpr&) {}
  // virtual void visit(CallExpr&) {}
  // virtual void visit(CastExpr&) {}
  virtual void visit(UnaryExpression&) {}
  virtual void visit(BinaryExpression&) {}
  // virtual void visit(ConditionalOp&) {}
  // virtual void visit(InitListExpr&) {}
  // virtual void visit(CompoundLiteralExpr&) {}

  // Stmt
  virtual void visit(NullStmt&) {}
  virtual void visit(ExprStmt&) {}
  // virtual void visit(CompoundStmt&) {}
  virtual void visit(IfStmt&) {}
  // virtual void visit(WhileStmt&) {}
  // virtual void visit(DoWhileStmt&) {}
  // virtual void visit(ForStmt&) {}
  virtual void visit(SwitchStmt&) {}
  // virtual void visit(CaseStmt&) {}
  // virtual void visit(DefaultStmt&) {}
  // virtual void visit(LabelStmt&) {}
  // virtual void visit(BreakStmt&) {}
  // virtual void visit(ContinueStmt&) {}
  // virtual void visit(GotoStmt&) {}
  // virtual void visit(ReturnStmt&) {}

  // Decl
  // virtual void visit(VarDecl&) {}
  // virtual void visit(ParamDecl&) {}
  // virtual void visit(TypedefDecl&) {}
  // virtual void visit(FieldDecl&) {}
  // virtual void visit(RecordDecl&) {}
  // virtual void visit(EnumDecl&) {}
  // virtual void visit(FunctionDecl&) {}
  // virtual void visit(DeclStmt&) {}
  virtual void visit(TranslationUnit&) {}
};

struct ASTNode {
  SourceRange loc{};
  virtual ~ASTNode() = default;
  virtual void accept(ASTWalker &v) = 0;
};

// pretty printer
void print(ASTNode &n, std::ostream &os);

struct Expr;
struct Stmt;
struct Decl;
struct Type;

// expressions
struct Expr : public ASTNode {
  virtual ~Expr() = default;
};

struct IntegerLiteral : Expr {
  std::string literal;
  int intValue;
  void accept(ASTWalker &v) override;
};

struct FloatingLiteral : Expr {
  std::string literal;
  double floatValue;
  void accept(ASTWalker &v) override;
};

struct IdentifierExpr : Expr {
  std::string literal;
  void accept(ASTWalker &v) override;
};

struct CharLiteral : Expr {
  std::string literal;
  char charValue;
  void accept(ASTWalker &v) override;
};

struct StringLiteral : Expr {
  std::string literal;
  void accept(ASTWalker &v) override;
};


// binary expressions
struct BinaryExpression : public Expr {
  BINARY_OPERATOR op;
  Expr* left;
  Expr* right;
  void accept(ASTWalker &v) override;
};

// unary expressions
struct UnaryExpression : public Expr {
  UNARY_OPERATOR op;
  Expr* operand;
  Type* typeOperand; // for sizeof, alignof
  void accept(ASTWalker &v) override;
};

// statements
struct Stmt : public ASTNode {
  virtual ~Stmt() = default;
};

struct NullStmt : public Stmt {
  void accept(ASTWalker &v) override;
};

struct ExprStmt : public Stmt {
  Expr* expr;
  void accept(ASTWalker &v) override;
};

struct IfStmt : public Stmt {
  Expr* condition;
  Stmt* thenBranch;
  Stmt* elseBranch;
  void accept(ASTWalker &v) override;
};

struct SwitchStmt : public Stmt {
  Expr* condition;
  Stmt* body;
  void accept(ASTWalker &v) override;
};

// declarations
struct Decl : public ASTNode {
  virtual ~Decl() = default;
};

struct TranslationUnit : public Decl {
  std::vector<std::unique_ptr<Decl>> declarations;
  void addDecl(std::unique_ptr<Decl> decl) {
    declarations.push_back( std::move(decl) );
  }
  void accept(ASTWalker &v) override;
};

enum class BUILTIN_TYPE {
  INT,
  UINT,
  FLOAT,
  DOUBLE,
  LONG_DOUBLE,
  CHAR,
  SCHAR,
  UCHAR,
  VOID,
  BOOL,
  COMPLEX,
  GENERIC,
  IMAGINARY,
  LONG,
  UNSIGNED_LONG,
  LONG_LONG,
  UNSIGNED_LONG_LONG,
  SHORT,
  USHORT
};

enum class UNARY_OPERATOR {
  PLUS,
  MINUS,
  INCREMENT,
  PRE_INC,
  DECREMENT,
  PRE_DEC,
  ADDRESS_OF,
  DEREFERENCE,
  BITWISE_NOT,
  LOGICAL_NOT,
  SIZEOF,
  ALIGNOF
};

enum class BINARY_OPERATOR {
  ADD,
  SUBTRACT,
  MULTIPLY,
  DIVIDE,
  ASSIGN,
  EQUAL,
  NOT_EQUAL,
  LESS_THAN,
  GREATER_THAN,
  LESS_EQUAL,
  GREATER_EQUAL,
  LOGICAL_AND,
  LOGICAL_OR,
  BITWISE_AND,
  BITWISE_OR,
  BITWISE_XOR,
  MODULO,
  LEFT_SHIFT,
  RIGHT_SHIFT,
  MULTASSIGN,
  DIVASSIGN,
  MODASSIGN,
  ADDASSIGN,
  SUBASSIGN,
  LEFTSHIFTASSIGN,
  RIGHTSHIFTASSIGN,
  ANDASSIGN,
  ORASSIGN,
  XORASSIGN,
  COMMA
};

enum class TypeStorageQualifier {
  TYPEDEF,
  STATIC,
  EXTERN,
  REGISTER,
  AUTO,
  THREAD_LOCAL,
  NONE
};

enum class TYPE_QUALIFIER {
  NONE,
  CONST,
  RESTRICT,
  VOLATILE,
  ATOMIC
};

// types

struct Type : public ASTNode {
  std::vector<TYPE_QUALIFIER> qualifiers;
  std::string name;
  int sizeInBytes;
  int alignmentInBytes;
};

struct BuiltinType : public Type {
  BUILTIN_TYPE type;
  void accept(ASTWalker &v) override;
};

struct PointerType : public Type {
  Type* baseType;
  void accept(ASTWalker &v) override;
};

struct ArrayType : public Type {
  Type* elementType;
  void accept(ASTWalker &v) override;
};

struct FunctionType : public Type {
  Type* returnType;
  void accept(ASTWalker &v) override;
};

// struct RecordType : public Type {
//   std::string recordName;
//   std::vector<FieldDecl*> fields;
//   void accept(ASTWalker &v) override;
// };

struct EnumType : public Type {
  std::string enumName;
  void accept(ASTWalker &v) override;
};

struct TypedefType : public Type {
  std::string typedefName;
  void accept(ASTWalker &v) override;
};