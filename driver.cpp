#include "driver.hpp"
#include <iostream>
#include <cctype>
#include <fstream>
#include <cassert>

cAST::Driver::~Driver() {
  delete(scanner);
  scanner = nullptr;
  delete(parser);
  parser = nullptr;
  delete(head_);
  head_ = nullptr;
}

void cAST::Driver::parse( const char * const filename ) {
  assert( filename != nullptr );
  std::ifstream in_file( filename );
  if( ! in_file.good() )
  {
    exit( EXIT_FAILURE );
  }
  ensure_root();
  parse_helper( in_file );
  return;
}

void cAST::Driver::parse( std::istream &stream ) {
  if( ! stream.good()  && stream.eof() ) {
    return;
  }
  //else
  ensure_root();
  parse_helper( stream );
  return;
}

void cAST::Driver::parse_helper( std::istream &stream ){
  delete(scanner);
  try {
  scanner = new cAST::cASTScanner( &stream );
  }
  catch( std::bad_alloc &ba ) {
    std::cerr << "Failed to allocate scanner: (" << ba.what() << "), exiting!!\n";
    exit( EXIT_FAILURE );
  }

  delete(parser);
  try {
  parser = new cAST::cASTParser( (*scanner) /* scanner */, (*this) /* driver */ );
  }
  catch( std::bad_alloc &ba ) {
    std::cerr << "Failed to allocate parser: (" << ba.what() << "), exiting!!\n";
    exit( EXIT_FAILURE );
  }
  const int accept = 0;
  const int rc = parser->parse();
  if( rc != accept ) {
    std::cerr << "Parse failed with code: " << rc << std::endl;
    had_error_ = true;
    // dump_diagnostics( std::cerr );
  }
  return;
}

void cAST::Driver::ensure_root() {
  if (!head_) {
    head_ = new cAST::TranslationUnit();
  }
  if (parent_stack_.empty() || parent_stack_.front() != head_) {
    parent_stack_.clear();
    parent_stack_.push_back(head_);
  }
}

cAST::TypeNode* cAST::Driver::makeBuiltinType(cAST::BUILTIN_TYPE bt) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for builtin type");

  auto* type = parent->emplace_child<cAST::BuiltinType>();
  type->set_type(bt);

  return static_cast<cAST::TypeNode*>(type);
}

cAST::Expr* cAST::Driver::makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for unary expression");

  auto* unary = parent->emplace_child<cAST::UnaryExpr>();
  unary->set_op(op);
  unary->set_operand(std::move(expr));

  return static_cast<cAST::Expr*>(unary);
}

cAST::Expr* cAST::Driver::makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for binary expression");

  auto* binary = parent->emplace_child<cAST::BinaryExpr>();
  binary->set_op(op);
  binary->set_left(std::move(left));
  binary->set_right(std::move(right));

  return static_cast<cAST::Expr*>(binary);
}

cAST::Expr* cAST::Driver::makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for cast expression");

  auto* cast = parent->emplace_child<cAST::CastExpr>();
  cast->set_typeOperand(std::move(type));
  cast->set_operand(std::move(expr));

  return static_cast<cAST::Expr*>(cast);
}

cAST::Expr* cAST::Driver::makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for conditional expression");

  auto* condExpr = parent->emplace_child<cAST::ConditionalExpr>();
  condExpr->set_cond(std::move(cond));
  condExpr->set_thenExpr(std::move(thenExpr));
  condExpr->set_elseExpr(std::move(elseExpr));

  return static_cast<cAST::Expr*>(condExpr);
}

cAST::Expr* cAST::Driver::makeIdentifierExpr(const std::string& name) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for identifier expression");

  auto* ident = parent->emplace_child<cAST::IdentifierExpr>();
  ident->set_name(name);

  return static_cast<cAST::Expr*>(ident);
}

cAST::Expr* cAST::Driver::makeConstantIntExpr(int value) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for integer literal");

  auto* lit = parent->emplace_child<cAST::IntegerLiteral>();
  lit->set_value(value);

  return static_cast<cAST::Expr*>(lit);
}

cAST::Expr* cAST::Driver::makeConstantFloatExpr(float value) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for floating literal");

  auto* lit = parent->emplace_child<cAST::FloatingLiteral>();
  lit->set_value(value);

  return static_cast<cAST::Expr*>(lit);
}

cAST::Expr* cAST::Driver::makeStringLiteral(const std::string& value) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for string literal");

  auto* lit = parent->emplace_child<cAST::StringLiteral>();
  lit->set_literal(value);

  return static_cast<cAST::Expr*>(lit);
}

cAST::Stmt* cAST::Driver::makeNullStmt() {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for null statement");

  auto* stmt = parent->emplace_child<cAST::NullStmt>();

  return static_cast<cAST::Stmt*>(stmt);
}

cAST::Stmt* cAST::Driver::makeExprStmt(std::unique_ptr<cAST::Expr> expr) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for expression statement");

  auto* stmt = parent->emplace_child<cAST::ExprStmt>();
  stmt->set_expr(std::move(expr));

  return static_cast<cAST::Stmt*>(stmt);
}

cAST::Stmt* cAST::Driver::makeCompoundStmt(std::vector<std::unique_ptr<cAST::Stmt>> stmts) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for compound statement");

  auto* stmt = parent->emplace_child<cAST::CompoundStmt>();
  for (auto& s : stmts) {
    stmt->addStmt(std::move(s));
  }

  return static_cast<cAST::Stmt*>(stmt);
}

cAST::DeclSpecs cAST::Driver::makeSpecsFromBuiltinType(cAST::BUILTIN_TYPE bt) {
  cAST::DeclSpecs specs;
  specs.set_from_builtin_type(bt);
  return specs;
}

cAST::DeclSpecs cAST::Driver::makeSpecsFromTypeQual(cAST::TYPE_QUALIFIER tq) {
  cAST::DeclSpecs specs;
  specs.qualifiers.push_back(tq);
  return specs;
}

cAST::DeclSpecs cAST::Driver::makeSpecsFromStorageClass(cAST::TYPE_STORAGE_QUALIFIER sc) {
  cAST::DeclSpecs specs;
  specs.storage.push_back(sc);
  return specs;
}

cAST::Decl* cAST::Driver::makeDeclFromSpecs(cAST::DeclSpecs specs) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for declaration");

  auto* decl = parent->emplace_child<cAST::VarDecl>();
  decl->set_specs(std::make_unique<cAST::DeclSpecs>(std::move(specs)));

  return static_cast<cAST::Decl*>(decl);
}

cAST::Stmt* cAST::Driver::makeDeclStmt(std::unique_ptr<cAST::Decl> decl) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for declaration statement");

  auto* stmt = parent->emplace_child<cAST::DeclStmt>();
  stmt->set_decl(std::move(decl));

  return static_cast<cAST::Stmt*>(stmt);
}

// cAST::Expr* cAST::Driver::singleton(std::unique_ptr<cAST::Expr> expr) {
//   return push_expression(std::move(expr));
// }

cAST::Expr* cAST::Driver::makeMember(std::unique_ptr<cAST::Expr> base, const std::string& memberName, bool isPointer) {
  auto node = std::make_unique<cAST::MemberExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_base(std::move(base));
  if (node->base) node->base->set_parent(raw);
  node->memberName = memberName;
  node->isPointer = isPointer;
  return raw;
}

cAST::Expr* cAST::Driver::makeSubscript(std::unique_ptr<cAST::Expr> base, std::unique_ptr<cAST::Expr> index) {

}

cAST::Expr* cAST::Driver::makeCall(std::unique_ptr<cAST::Expr> callee, std::vector<std::unique_ptr<cAST::Expr>> args) {

}

cAST::DeclSpecs cAST::Driver::makeSpecsFromTypeNode(std::unique_ptr<cAST::TypeNode> type) {
  DeclSpecs specs;
  specs.set_from_type_node(std::move(type));
  return specs;
}

cAST::DeclSpecs cAST::Driver::combineSpecs(cAST::DeclSpecs a, cAST::DeclSpecs b) {
  cAST::DeclSpecs combined;
  // Combine type info
  if (a.type == cAST::BUILTIN_TYPE::Void) combined.type = b.type;
  else if (b.type == cAST::BUILTIN_TYPE::Void) combined.type = a.type;
  else if (a.type == cAST::BUILTIN_TYPE::Unsigned && b.type == cAST::BUILTIN_TYPE::Signed) combined.type = cAST::BUILTIN_TYPE::Int;
  else if (a.type == cAST::BUILTIN_TYPE::Signed && b.type == cAST::BUILTIN_TYPE::Unsigned) combined.type = cAST::BUILTIN_TYPE::Int;
  else if (a.type == cAST::BUILTIN_TYPE::Signed && b.type == cAST::BUILTIN_TYPE::Int) combined.type = cAST::BUILTIN_TYPE::UInt;
  else if (a.type == cAST::BUILTIN_TYPE::Int && b.type == cAST::BUILTIN_TYPE::Signed) combined.type = cAST::BUILTIN_TYPE::UInt;
  else if (a.type == cAST::BUILTIN_TYPE::Unsigned && b.type == cAST::BUILTIN_TYPE::Int) combined.type = cAST::BUILTIN_TYPE::UInt;
  else if (a.type == cAST::BUILTIN_TYPE::Int && b.type == cAST::BUILTIN_TYPE::Unsigned) combined.type = cAST::BUILTIN_TYPE::UInt;
  else combined.type = a.type;
  return combined;
}

void cAST::Driver::dump_ast(std::ostream& os) {
  if (!head_) {
    os << "<no AST>\n"; return;
  }
  cAST::prettyprint(*head_, os);
}