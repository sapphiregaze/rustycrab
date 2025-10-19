#pragma once

#include <string>
#include <iostream>
#include <ostream>
#include <vector>
#include <memory>

namespace AST {

enum class AssignOp {
  Assign,
  Mul,
  Div,
  Mod,
  Add,
  Sub,
  Shl,
  Shr,
  And,
  Xor,
  Or
};

enum class BUILTIN_TYPE {
  Void,
  Int,
  UInt,
  Float,
  Double,
  Long_Double,
  Char,
  Schar,
  Uchar,
  Bool,
  Complex,
  Generic,
  Imaginary,
  Long,
  Unsigned_Long,
  Long_Long,
  Unsigned_Long_Long,
  Short,
  Ushort
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

enum class TYPE_STORAGE_QUALIFIER {
  Typedef,
  Static,
  Extern,
  Register,
  Auto,
  Thread_Local,
  None
};

enum class TYPE_QUALIFIER {
  None,
  Const,
  Restrict,
  Volatile,
  Atomic
};

struct SourcePos {
  std::string file;
  uint32_t line{0};
  uint32_t column{0};
};

struct SourceRange {
  SourcePos begin{};
  SourcePos end{};
};

struct ASTWalker;

struct ASTNode {
  ASTNode* parent = nullptr;
  SourceRange loc{};
  ~ASTNode() = default;
  virtual void accept(ASTWalker &v) = 0;

  void set_parent(ASTNode* p) { parent = p; }
  ASTNode* get_parent() const { return parent; }
};

// pretty printer
void prettyprint(ASTNode &n, std::ostream &os);

// // Types
// struct BuiltinType;
// struct PointerType;
// struct ArrayType;
// struct FunctionType;
// struct RecordType;
// struct EnumType;
// struct TypedefType;

// // Expr
// struct IntegerLiteral;
// struct FloatingLiteral;
// struct CharLiteral;
// struct StringLiteral;
// struct IdentifierExpr;
// struct MemberExpr;
// struct ArraySubscriptExpr;
// struct CallExpr;
// struct CastExpr;
// struct UnaryExpression;
// struct BinaryExpression;
// struct ConditionalOp;
// struct InitListExpr;
// struct CompoundLiteralExpr;

// // Stmt
// struct NullStmt;
// struct ExprStmt;
// struct CompoundStmt;
// struct IfStmt;
// struct WhileStmt;
// struct DoWhileStmt;
// struct ForStmt;
// struct SwitchStmt;
// struct CaseStmt;
// struct DefaultStmt;
// struct LabelStmt;
// struct BreakStmt;
// struct ContinueStmt;
// struct GotoStmt;
// struct ReturnStmt;

// // Decl
// struct VarDecl;
// struct ParamDecl;
// struct TypedefDecl;
// struct FieldDecl;
// struct RecordDecl;
// struct EnumDecl;
// struct FunctionDecl;
// struct DeclStmt;
// struct TranslationUnit;

struct Stmt;
struct Decl;
struct TypeNode;

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
  TypeNode* typeOperand; // for sizeof, alignof
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

struct CompoundStmt : public Stmt {
  std::vector<std::unique_ptr<Stmt>> statements;
  void addStmt(std::unique_ptr<Stmt> stmt) {
    statements.push_back( std::move(stmt) );
  }
  void accept(ASTWalker &v) override;
};

struct WhileStmt : public Stmt {
  Expr* condition;
  Stmt* body;
  void accept(ASTWalker &v) override;
};

struct DoWhileStmt : public Stmt {
  Stmt* body;
  Expr* condition;
  void accept(ASTWalker &v) override;
};

struct ForStmt : public Stmt {
  Stmt* init;
  Expr* condition;
  Expr* increment;
  Stmt* body;
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

struct VarSpecifiers {
  std::vector<TYPE_STORAGE_QUALIFIER> storage;
  bool isInline{false};
  bool isNoreturn{false};
};
struct VarDecl : public Decl {
  std::string name;
  TypeNode* type;
  Expr* init;
  VarSpecifiers specs;
  void accept(ASTWalker &v) override;
};

struct ParamDecl : public Decl {
  std::string name;
  TypeNode* type;
  // indefinite arity;; idk if we implement tthat
  bool isVariadic{false};
  void accept(ASTWalker &v) override;
};

struct FieldDecl : public Decl {
  std::string name;
  TypeNode* type;
  Expr* bitWidth;
  void accept(ASTWalker &v) override;
};

  //   {
  //     // Build AST::FunctionDecl(specs, declarator, body, locals = $3)
  //     auto fn = std::make_unique<AST::FunctionDecl>(/* use $1, $2 */);
  //     fn->set_body(std::move($4));                 // if your body is a Stmt unique_ptr
  //     fn->set_locals(std::move($3));               // vector<unique_ptr<Decl>>
  //     $$ = std::move(fn);                          // unique_ptr<Decl>
  //   }
  // | declaration_specifiers declarator compound_statement
  //   {
  //     auto fn = std::make_unique<AST::FunctionDecl>(/* use $1, $2 */);
  //     fn->set_body(std::move($3));
  //     $$ = std::move(fn);                          // unique_ptr<Decl>
  //   }

struct FunctionDecl : public Decl {
  std::string name;
  TypeNode* returnType;
  std::vector<std::unique_ptr<ParamDecl>> params;
  bool isDefinition{false};
  // indefinite arity;; idk if we implement tthat
  bool isVariadic{false};
  Stmt* body;
  void set_body(Stmt* b) { body = b; }
  void set_params(std::vector<std::unique_ptr<ParamDecl>> p) {
    params = std::move(p);
  }
  void accept(ASTWalker &v) override;
};

struct DeclStmt : public Stmt {
  Decl* declaration;
  void accept(ASTWalker &v) override;
};

struct TranslationUnit : public Decl {
  std::vector<std::unique_ptr<Decl>> declarations;
  void addDecl(std::unique_ptr<Decl> decl) {
    declarations.push_back( std::move(decl) );
  }
  void accept(ASTWalker &v) override;
};

// types

struct TypeNode : public ASTNode {
  std::vector<TYPE_QUALIFIER> qualifiers;
  std::string name;
  int sizeInBytes;
  int alignmentInBytes;
};

struct BuiltinType : public TypeNode {
  BUILTIN_TYPE type;
  void accept(ASTWalker &v) override;
};

struct PointerType : public TypeNode {
  TypeNode* baseType;
  void accept(ASTWalker &v) override;
};

struct ArrayType : public TypeNode {
  TypeNode* elementType;
  void accept(ASTWalker &v) override;
};

struct FunctionType : public TypeNode {
  TypeNode* returnType;
  void accept(ASTWalker &v) override;
};

// struct RecordType : public TypeNode {
//   std::string recordName;
//   std::vector<FieldDecl*> fields;
//   void accept(ASTWalker &v) override;
// };

struct EnumType : public TypeNode {
  std::string enumName;
  void accept(ASTWalker &v) override;
};

struct TypedefType : public TypeNode {
  std::string typedefName;
  void accept(ASTWalker &v) override;
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
  virtual void visit(CompoundStmt&) {}
  virtual void visit(IfStmt&) {}
  virtual void visit(WhileStmt&) {}
  virtual void visit(DoWhileStmt&) {}
  virtual void visit(ForStmt&) {}
  virtual void visit(SwitchStmt&) {}
  // virtual void visit(CaseStmt&) {}
  // virtual void visit(DefaultStmt&) {}
  // virtual void visit(LabelStmt&) {}
  // virtual void visit(BreakStmt&) {}
  // virtual void visit(ContinueStmt&) {}
  // virtual void visit(GotoStmt&) {}
  // virtual void visit(ReturnStmt&) {}

  // Decl
  virtual void visit(VarDecl&) {}
  virtual void visit(ParamDecl&) {}
  // virtual void visit(TypedefDecl&) {}
  virtual void visit(FieldDecl&) {}
  // virtual void visit(RecordDecl&) {}
  // virtual void visit(EnumDecl&) {}
  virtual void visit(FunctionDecl&) {}
  virtual void visit(DeclStmt&) {}
  virtual void visit(TranslationUnit&) {}
};

} // namespace AST