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
    dump_diagnostics( std::cerr );
  }
  return;
}

void cAST::Driver::reset() {
  head_.reset();
  diags_.clear();
  had_error_ = false;
}

std::unique_ptr<cAST::TranslationUnit> cAST::Driver::take() {
  return std::move(head_);
}

void cAST::Driver::ensure_root() {
  if (!head_) {
    head_ = std::make_unique<cAST::TranslationUnit>();
  }
}

template <class Parent, class Child>
static Child* attach_child_to_vec(std::unique_ptr<Child> child, Parent& parent, std::vector<std::unique_ptr<Child>>& vec) {
  child->set_parent(&parent);
  Child* raw = child.get();
  vec.emplace_back(std::move(child));
  return raw;
}

template <class Parent, class Child>
static Child* set_child_slot(std::unique_ptr<Child>& slot, std::unique_ptr<Child> child, Parent& parent) {
  child->set_parent(&parent);
  Child* raw = child.get();
  slot = std::move(child);
  return raw;
}

cAST::Decl* cAST::Driver::push_declaration(std::unique_ptr<cAST::Decl> node) {
  auto* h = head();
  if (!h) return nullptr;

  if (auto* tu = dynamic_cast<cAST::TranslationUnit*>(h)) {
    return attach_child_to_vec(std::move(node), *tu, tu->declarations);
  }
  throw std::logic_error("no valid declaration parent at head()");
}

cAST::Stmt* cAST::Driver::push_statement(std::unique_ptr<cAST::Stmt> node) {
  auto* h = head();
  if (!h) return nullptr;

  throw std::logic_error("no valid statement parent at head()");
}

cAST::Expr* cAST::Driver::push_expression(std::unique_ptr<cAST::Expr> node) {
  auto* h = head();
  if (!h) return nullptr;

  // Expressions are usually assigned into *fields* (not vectors):
  // e.g., ReturnStmt::value, IfStmt::cond, AssignExpr::rhs, etc.
  // Pattern: detect the exact parent type and set the slot.

  if (auto* ret = dynamic_cast<ReturnStmt*>(h)) {
    return set_child_slot(ret->value, std::move(node), *ret);
  }
  if (auto* ifs = dynamic_cast<cAST::IfStmt*>(h)) {
    if (!ifs->cond) return set_child_slot(ifs->cond, std::move(node), *ifs);
    // else it’s probably then/else statement time; expr not expected here
  }
  // Add more cases (WhileStmt::cond, ForStmt parts, BinaryExpr children, etc.)

  throw std::logic_error("no valid expression slot at head()");
}

cAST::TypeNode* cAST::Driver::push_type(std::unique_ptr<cAST::TypeNode> node) {
  auto* h = head();
  if (!h) return nullptr;

  // Types are usually slots too (e.g., VarDecl::type, FunctionDecl::return_type)
  if (auto* vd = dynamic_cast<cAST::VarDecl*>(h)) {
    return set_child_slot(vd->type, std::move(node), *vd);
  }
  if (auto* fn = dynamic_cast<cAST::FunctionDecl*>(h)) {
    if (!fn->return_type)
      return set_child_slot(fn->return_type, std::move(node), *fn);
  }

  throw std::logic_error("no valid type slot at head()");
}

cAST::TypeNode* cAST::Driver::makeBuiltinType(cAST::BUILTIN_TYPE bt) {
  std::cout << "Making builtin type " << static_cast<int>(bt) << "\n";
  auto node = std::make_unique<cAST::BuiltinType>();
  node->set_parent(head());
  cAST::TypeNode* raw = node.get();
  node->type = bt;
  return raw;
}

cAST::Expr* cAST::Driver::makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr) {
  std::cout << "Making unary expr with op " << static_cast<int>(op) << "\n";
  auto node = std::make_unique<cAST::UnaryExpr>();
  node->set_op(op);
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_operand(std::move(expr));
  if (node->operand) node->operand->set_parent(raw);
  return raw;
}

cAST::Expr* cAST::Driver::makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right) {
  std::cout << "Making binary expr with op " << static_cast<int>(op) << "\n";
  auto node = std::make_unique<cAST::BinaryExpr>();
  node->set_op(op);
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_left(std::move(left));
  if (node->left) node->left->set_parent(raw);
  node->set_right(std::move(right));
  if (node->right) node->right->set_parent(raw);
  return raw;
}

cAST::Expr* cAST::Driver::makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr) {
  std::cout << "Making cast expr\n";
  auto node = std::make_unique<cAST::CastExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_typeOperand(std::move(type));
  if (node->typeOperand) node->typeOperand->set_parent(raw);
  node->set_operand(std::move(expr));
  if (node->operand) node->operand->set_parent(raw);
  return raw;
}

cAST::Expr* cAST::Driver::makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr) {
  std::cout << "Making conditional expr\n";
  auto node = std::make_unique<cAST::ConditionalExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_cond(std::move(cond));
  if (node->cond) node->cond->set_parent(raw);
  node->set_thenExpr(std::move(thenExpr));
  if (node->thenExpr) node->thenExpr->set_parent(raw);
  node->set_elseExpr(std::move(elseExpr));
  if (node->elseExpr) node->elseExpr->set_parent(raw);
  return raw;
}

cAST::Expr* cAST::Driver::makeIdentifierExpr(const std::string& name) {
  std::cout << "Making identifier expr with name " << name << "\n";
  auto node = std::make_unique<cAST::IdentifierExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->literal = name;
  return raw;
}

cAST::Expr* cAST::Driver::makeConstantIntExpr(int value) {
  std::cout << "Making integer literal expr with value " << value << "\n";
  auto node = std::make_unique<cAST::IntegerLiteral>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->intValue = value;
  node->literal = std::to_string(value);
  return raw;
}

cAST::Expr* cAST::Driver::makeConstantFloatExpr(float value) {
  std::cout << "Making floating literal expr with value " << value << "\n";
  auto node = std::make_unique<cAST::FloatingLiteral>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->floatValue = value;
  node->literal = std::to_string(value);
  return raw;
}

cAST::Expr* cAST::Driver::makeStringLiteral(const std::string& value) {
  std::cout << "Making string literal expr with value " << value << "\n";
  auto node = std::make_unique<cAST::StringLiteral>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->literal = value;
  return raw;
}

cAST::Expr* cAST::Driver::singleton(std::unique_ptr<cAST::Expr> expr) {
  return push_expression(std::move(expr));
}

cAST::DeclSpecs cAST::Driver::makeSpecsFromType(std::unique_ptr<cAST::TypeNode> type) {
  DeclSpecs specs;
  specs.type = std::move(type);
  return specs;
}

cAST::DeclSpecs cAST::Driver::combineSpecs(cAST::DeclSpecs a, cAST::DeclSpecs b) {
  cAST::DeclSpecs combined;
  // Combine type info
  combined.type = b.type ? std::move(b.type) : std::move(a.type);
  // Combine other spec fields as needed
  return combined;
}


// void cAST::Driver::report_error(const std::string& message) {
//   had_error_ = true;
//   diags_.push_back(Diag{ -1, -1, message });
// }

// void cAST::Driver::report_error(int line, int column, const std::string& message) {
//   had_error_ = true;
//   diags_.push_back(Diag{ line, column, message });
// }

void cAST::Driver::dump_ast(std::ostream& os) {
  if (!head_) {
    os << "<no AST>\n"; return;
  }
  os << "AST Dump:\n";
  cAST::prettyprint(*head_, os);
}

void cAST::Driver::dump_diagnostics(std::ostream& os) {
  for (const auto& d : diags_) {
    if (d.line >= 0)
      os << source_name_ << ":" << d.line << ":" << d.col << ": error: " << d.msg << "\n";
    else
      os << source_name_ << ": error: " << d.msg << "\n";
  }
}
