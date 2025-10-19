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

std::unique_ptr<AST::TranslationUnit> cAST::Driver::take() {
  return std::move(head_);
}

void cAST::Driver::ensure_root() {
  if (!head_) {
    head_ = std::make_unique<AST::TranslationUnit>();
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


AST::Decl* cAST::Driver::push_declaration(std::unique_ptr<Decl> node) {
  auto* h = head();
  if (!h) return nullptr;

  if (auto* tu = dynamic_cast<TranslationUnit*>(h)) {
    return attach_child_to_vec(std::move(node), *tu, tu->declarations);
  }
  if (auto* blk = dynamic_cast<BlockStmt*>(h)) {
    // If your language allows block-scope declarations:
    // You'll need BlockStmt::declarations in AST, or reject here.
    // Example with optional support:
    // return attach_child_to_vec(std::move(node), *blk, blk->declarations);
    // For now, signal misuse:
    throw std::logic_error("declaration not allowed in current context");
  }

  throw std::logic_error("no valid declaration parent at head()");
}

AST::Stmt* cAST::Driver::push_statement(std::unique_ptr<Stmt> node) {
  auto* h = head();
  if (!h) return nullptr;

  if (auto* blk = dynamic_cast<BlockStmt*>(h)) {
    return attach_child_to_vec(std::move(node), *blk, blk->statements);
  }
  // If statements can appear directly under TU (e.g., top-level stmt in some langs),
  // add that case. For C, they belong in blocks or bodies.

  throw std::logic_error("no valid statement parent at head()");
}

AST::Expr* cAST::Driver::push_expression(std::unique_ptr<Expr> node) {
  auto* h = head();
  if (!h) return nullptr;

  // Expressions are usually assigned into *fields* (not vectors):
  // e.g., ReturnStmt::value, IfStmt::cond, AssignExpr::rhs, etc.
  // Pattern: detect the exact parent type and set the slot.

  if (auto* ret = dynamic_cast<ReturnStmt*>(h)) {
    return set_child_slot(ret->value, std::move(node), *ret);
  }
  if (auto* ifs = dynamic_cast<IfStmt*>(h)) {
    if (!ifs->cond) return set_child_slot(ifs->cond, std::move(node), *ifs);
    // else it’s probably then/else statement time; expr not expected here
  }
  // Add more cases (WhileStmt::cond, ForStmt parts, BinaryExpr children, etc.)

  throw std::logic_error("no valid expression slot at head()");
}

AST::TypeNode* cAST::Driver::push_type(std::unique_ptr<TypeNode> node) {
  auto* h = head();
  if (!h) return nullptr;

  // Types are usually slots too (e.g., VarDecl::type, FunctionDecl::return_type)
  if (auto* vd = dynamic_cast<VarDecl*>(h)) {
    return set_child_slot(vd->type, std::move(node), *vd);
  }
  if (auto* fn = dynamic_cast<FunctionDecl*>(h)) {
    if (!fn->return_type)
      return set_child_slot(fn->return_type, std::move(node), *fn);
  }

  throw std::logic_error("no valid type slot at head()");
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
  AST::prettyprint(*head_, os);
}

void cAST::Driver::dump_diagnostics(std::ostream& os) {
  for (const auto& d : diags_) {
    if (d.line >= 0)
      os << source_name_ << ":" << d.line << ":" << d.col << ": error: " << d.msg << "\n";
    else
      os << source_name_ << ": error: " << d.msg << "\n";
  }
}
