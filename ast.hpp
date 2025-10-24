#pragma once

#include <string>
#include <iostream>
#include <ostream>
#include <vector>
#include <memory>
#include <algorithm>
#include <cassert>

namespace cAST {

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
  Unsigned,
  Signed,
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
  using ChildPtr = std::unique_ptr<ASTNode>;
  using Children = std::vector<ChildPtr>;

  ASTNode* parent = nullptr;
  SourceRange loc{};

  virtual ~ASTNode() = default;
  virtual void accept(ASTWalker& v) = 0;

  // Add an existing child (takes ownership).
  ASTNode* add_child(ChildPtr n) {
    if (!n) return nullptr;
    n->parent = this;
    children_.emplace_back(std::move(n));
    return children_.back().get();
  }

  // Construct a child in place and add it.
  template <class T, class... Args>
  T* emplace_child(Args&&... args) {
    auto n = std::make_unique<T>(std::forward<Args>(args)...);
    n->parent = this;
    T* raw = n.get();
    children_.emplace_back(std::move(n));
    return raw;
  }

  // Read-only accessors (enough to iterate/inspect)
  const Children& children() const noexcept { return children_; }
  std::size_t size() const noexcept { return children_.size(); }
  const ASTNode* child(std::size_t i) const noexcept { return children_[i].get(); }
  ASTNode*       child(std::size_t i)       noexcept { return children_[i].get(); }

  // Optional (if you still want these)
  void set_parent(ASTNode* p) { parent = p; }
  ASTNode* get_parent()       { return parent; }
  const ASTNode* get_parent() const { return parent; }

private:
  Children children_{};
};

// struct ASTNode {
//   ASTNode* parent = nullptr;
//   SourceRange loc{};
//   ~ASTNode() = default;
//   virtual void accept(ASTWalker &v) = 0;

//   void set_parent(ASTNode* p) {
//     parent = p;
//   }
//   ASTNode* get_parent() {
//     return parent;
//   }
// };

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
// struct AssignExpr;
// struct ArraySubscriptExpr;
// struct CallExpr;
// struct CastExpr;
// struct UnaryExpr;
// struct BinaryExpr;
// struct ConditionalExpr;
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
// struct FieldDecl;
// struct FunctionDecl;
// struct DeclStmt;
// struct TranslationUnit;

struct Stmt;
struct Decl;
struct TypeNode;

struct DeclSpecs {
  std::vector<TYPE_STORAGE_QUALIFIER> storage;
  std::vector<TYPE_QUALIFIER> qualifiers;
  BUILTIN_TYPE type;

  void set_from_builtin_type(BUILTIN_TYPE bt) {
    type = bt;
  }

  void set_from_type_node(std::unique_ptr<TypeNode> typeNode) {
    type = BUILTIN_TYPE::Int;

    // // For simplicity, only handle BuiltinType here
    // if (auto* bt = dynamic_cast<BuiltinType*>(typeNode.get())) {
    //   type = bt->type;
    //   qualifiers = bt->qualifiers;
    // }
    // Handle other TypeNode derived types as needed
  }
  // std::unique_ptr<TypeNode> type;
  bool isInline{false};
  bool isNoreturn{false};
};

// expressions
struct Expr : public ASTNode {
  virtual ~Expr() = default;
};

struct IntegerLiteral : Expr {
  std::string literal;
  void set_value(int val) {
    literal = std::to_string(val);
    intValue = val;
  }
  int intValue;
  void accept(ASTWalker &v) override;
};

struct FloatingLiteral : Expr {
  std::string literal;
  double floatValue;
  void set_value(float val) {
    literal = std::to_string(val);
    floatValue = val;
  }
  void accept(ASTWalker &v) override;
};

struct IdentifierExpr : Expr {
  std::string literal;
  void set_name(const std::string& name) {
    literal = name;
  }
  void accept(ASTWalker &v) override;
};

struct CharLiteral : Expr {
  std::string literal;
  char charValue;
  void accept(ASTWalker &v) override;
};

struct StringLiteral : Expr {
  std::string literal;
  void set_literal(const std::string& lit) {
    literal = lit;
  }
  void accept(ASTWalker &v) override;
};

struct BinaryExpr : public Expr {
  BINARY_OPERATOR op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  void set_left(std::unique_ptr<Expr> l) {
    left = std::move(l);
  }
  void set_right(std::unique_ptr<Expr> r) {
    right = std::move(r);
  }
  void set_op(BINARY_OPERATOR o) {
    op = o;
  }
  void accept(ASTWalker &v) override;
};

struct UnaryExpr : public Expr {
  UNARY_OPERATOR op;
  std::unique_ptr<Expr> operand;
  std::unique_ptr<TypeNode> typeOperand;
  void set_operand(std::unique_ptr<Expr> opd) {
    operand = std::move(opd);
  }
  void set_typeOperand(std::unique_ptr<TypeNode> t) {
    typeOperand = std::move(t);
  }
  void set_op(UNARY_OPERATOR o) {
    op = o;
  }

  void accept(ASTWalker &v) override;
};

struct CastExpr : public Expr {
  std::unique_ptr<TypeNode> typeOperand;
  std::unique_ptr<Expr> operand;
  void set_typeOperand(std::unique_ptr<TypeNode> t) {
    typeOperand = std::move(t);
  }
  void set_operand(std::unique_ptr<Expr> opd) {
    operand = std::move(opd);
  }
  void accept(ASTWalker &v) override;
};

struct AssignExpr : public Expr {
  AssignOp op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;
  void set_op(AssignOp o) {
    op = o;
  }
  void set_left(std::unique_ptr<Expr> l) {
    left = std::move(l);
  }
  void set_right(std::unique_ptr<Expr> r) {
    right = std::move(r);
  }
  void accept(ASTWalker &v) override;
};

struct ConditionalExpr : public Expr {
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Expr> thenExpr;
  std::unique_ptr<Expr> elseExpr;
  void set_cond(std::unique_ptr<Expr> cond) {
    cond = std::move(cond);
  }
  void set_thenExpr(std::unique_ptr<Expr> thenE) {
    thenExpr = std::move(thenE);
  }
  void set_elseExpr(std::unique_ptr<Expr> elseE) {
    elseExpr = std::move(elseE);
  }
  void accept(ASTWalker &v) override;
};

struct MemberExpr : public Expr {
  std::unique_ptr<Expr> base;
  std::string memberName;
  bool isPointer{false};
  void set_base(std::unique_ptr<Expr> b) {
    base = std::move(b);
  }
  void accept(ASTWalker &v) override;
};

struct ArraySubscriptExpr : public Expr {
  std::unique_ptr<Expr> base;
  std::unique_ptr<Expr> index;
  void set_base(std::unique_ptr<Expr> b) {
    base = std::move(b);
  }
  void set_index(std::unique_ptr<Expr> idx) {
    index = std::move(idx);
  }
  void accept(ASTWalker &v) override;
};

struct CallExpr : public Expr {
  std::unique_ptr<Expr> callee;
  std::vector<std::unique_ptr<Expr>> arguments;
  void set_callee(std::unique_ptr<Expr> c) {
    callee = std::move(c);
  }
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
  std::unique_ptr<Expr> expr;
  void set_expr(std::unique_ptr<Expr> e) {
    expr = std::move(e);
  }
  void accept(ASTWalker &v) override;
};

struct CompoundStmt : public Stmt {
  std::vector<std::unique_ptr<ASTNode>> children;
  void addStmtOrExpr(std::unique_ptr<ASTNode> stmt) {
    children.push_back( std::move(stmt) );
  }
  std::vector<std::unique_ptr<ASTNode>>& getChildren() {
    return children;
  }
  void accept(ASTWalker &v) override;
};

struct WhileStmt : public Stmt {
  std::unique_ptr<Expr> condition;
  std::unique_ptr<Stmt> body;
  void accept(ASTWalker &v) override;
};

struct DoWhileStmt : public Stmt {
  std::unique_ptr<Stmt> body;
  std::unique_ptr<Expr> condition;
  void accept(ASTWalker &v) override;
};

struct ForStmt : public Stmt {
  std::unique_ptr<Stmt> init;
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Expr> incr;
  std::unique_ptr<Stmt> body;
  void accept(ASTWalker &v) override;
};
struct IfStmt : public Stmt {
  std::unique_ptr<Expr> cond;
  Stmt* thenBranch;
  Stmt* elseBranch;
  void accept(ASTWalker &v) override;
};

struct ReturnStmt : public Stmt {
  std::unique_ptr<Expr> value;
  void accept(ASTWalker &v) override;
};

struct ContinueStmt : public Stmt {
  void accept(ASTWalker &v) override;
};

struct BreakStmt : public Stmt {
  void accept(ASTWalker &v) override;
};

// declarations
struct Decl : public ASTNode {
  virtual ~Decl() = default;
};

struct DeclStmt : public Stmt {
  std::unique_ptr<Decl> declaration;
  void set_decl(std::unique_ptr<Decl> decl) {
    declaration = std::move(decl);
  }
  void accept(ASTWalker &v) override;
};

struct VarDecl : public Decl {
  std::string name;
  std::unique_ptr<TypeNode> type;
  std::unique_ptr<Expr> init;
  std::unique_ptr<DeclSpecs> specs;

  void set_type(std::unique_ptr<TypeNode> t) {
    type = std::move(t);
  }

  void set_init(std::unique_ptr<Expr> i) {
    init = std::move(i);
  }

  void set_specs(std::unique_ptr<DeclSpecs> s) {
    specs = std::move(s);
  }

  void accept(ASTWalker &v) override;
};

struct DeclGroup : public Decl {
  // TODO would be nice to change this to smart pointer if possible
  std::vector<Decl*> decls;

  void set_decls(std::vector<Decl*> d) {
    decls = d;
  }

  void accept(ASTWalker &v) override;
};

struct ParamDecl : public Decl {
  std::string name;
  std::unique_ptr<TypeNode> type;
  // indefinite arity;; idk if we implement tthat
  bool isVariadic{false};
  void accept(ASTWalker &v) override;
};

struct FieldDecl : public Decl {
  std::string name;
  std::unique_ptr<TypeNode> type;
  std::unique_ptr<Expr> bitWidth;
  void accept(ASTWalker &v) override;
};

struct FunctionDecl : public Decl {
  std::string name;
  std::unique_ptr<TypeNode> type;
  std::vector<std::unique_ptr<ParamDecl>> params;
  DeclSpecs specs;
  std::unique_ptr<ASTNode> body;
  // indefinite arity;; idk if we implement tthat
  bool isVariadic{false};

  void set_type(std::unique_ptr<TypeNode> t) {
    type = std::move(t);
  }

  void set_specs(DeclSpecs s) {
    specs = s;
  }

  void set_body(std::unique_ptr<ASTNode> b) {
    body = std::move(b);
  }

  void set_params(std::vector<std::unique_ptr<ParamDecl>> p) {
    params = std::move(p);
  }

  void accept(ASTWalker &v) override;
};

struct TranslationUnit : public Decl {
  std::vector<Decl*> declarations;
  void addDecl(Decl* decl) {
    declarations.push_back( decl );
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
  void set_type(BUILTIN_TYPE t) {
    type = t;
  }
  void accept(ASTWalker &v) override;
};

struct PointerType : public TypeNode {
  std::unique_ptr<TypeNode> baseType;
  void accept(ASTWalker &v) override;
};

struct ArrayType : public TypeNode {
  std::unique_ptr<TypeNode> elementType;
  void accept(ASTWalker &v) override;
};

struct FunctionType : public TypeNode {
  std::unique_ptr<TypeNode> returnType;
  void accept(ASTWalker &v) override;
};

struct ASTWalker {
  virtual ~ASTWalker() = default;

  // Types
  virtual void visit(BuiltinType&) {}
  virtual void visit(PointerType&) {}
  virtual void visit(ArrayType&) {}
  virtual void visit(FunctionType&) {}

  // Expr
  virtual void visit(IntegerLiteral&) {}
  virtual void visit(FloatingLiteral&) {}
  virtual void visit(CharLiteral&) {}
  virtual void visit(StringLiteral&) {}
  virtual void visit(IdentifierExpr&) {}
  virtual void visit(MemberExpr&) {}
  virtual void visit(ArraySubscriptExpr&) {}
  virtual void visit(CallExpr&) {}
  virtual void visit(CastExpr&) {}
  virtual void visit(AssignExpr&) {}
  virtual void visit(UnaryExpr&) {}
  virtual void visit(BinaryExpr&) {}
  virtual void visit(ConditionalExpr&) {}
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
  // virtual void visit(CaseStmt&) {}
  // virtual void visit(DefaultStmt&) {}
  // virtual void visit(LabelStmt&) {}
  virtual void visit(BreakStmt&) {}
  virtual void visit(ContinueStmt&) {}
  // virtual void visit(GotoStmt&) {}
  virtual void visit(ReturnStmt&) {}

  // Decl
  virtual void visit(VarDecl&) {}
  virtual void visit(ParamDecl&) {}
  virtual void visit(DeclGroup&) {}
  // virtual void visit(TypedefDecl&) {}
  virtual void visit(FieldDecl&) {}
  // virtual void visit(RecordDecl&) {}
  // virtual void visit(EnumDecl&) {}
  virtual void visit(FunctionDecl&) {}
  virtual void visit(DeclStmt&) {}
  virtual void visit(TranslationUnit&) {}
};

} // namespace cAST