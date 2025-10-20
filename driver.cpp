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
  std::cout << "Making builtin type " << static_cast<int>(bt) << "\n";

  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for builtin type");

  auto* type = parent->emplace_child<cAST::BuiltinType>();
  type->set_type(bt);

  return static_cast<cAST::TypeNode*>(type);
}

// cAST::Expr* cAST::Driver::makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr) {
//   return nullptr;
// }

// cAST::Expr* cAST::Driver::makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right) {
//   return nullptr;
// }

// cAST::Expr* cAST::Driver::makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr) {
//   return nullptr;
// }

// cAST::Expr* cAST::Driver::makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr) {
//   return nullptr;
// }

// cAST::Expr* cAST::Driver::makeIdentifierExpr(const std::string& name) {
//   return nullptr;
// }

// cAST::Expr* cAST::Driver::makeConstantIntExpr(int value) {
//   return nullptr;

// cAST::Expr* cAST::Driver::makeConstantFloatExpr(float value) {
//   return nullptr;
// }

cAST::Expr* cAST::Driver::makeStringLiteral(const std::string& value) {
  std::cout << "Making string literal expr with value " << value << "\n";

  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for string literal");

  auto* lit = parent->emplace_child<cAST::StringLiteral>();
  lit->set_literal(value);

  return static_cast<cAST::Expr*>(lit);
}

// cAST::Expr* cAST::Driver::singleton(std::unique_ptr<cAST::Expr> expr) {
//   return push_expression(std::move(expr));
// }

cAST::Expr* cAST::Driver::makeMember(std::unique_ptr<cAST::Expr> base, const std::string& memberName, bool isPointer) {
  std::cout << "Making member access expr for member " << memberName << (isPointer ? " via pointer\n" : "\n");
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
  std::cout << "Making array subscript expr\n";
  auto node = std::make_unique<cAST::ArraySubscriptExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_base(std::move(base));
  if (node->base) node->base->set_parent(raw);
  node->set_index(std::move(index));
  if (node->index) node->index->set_parent(raw);
  return raw;
}

cAST::Expr* cAST::Driver::makeCall(std::unique_ptr<cAST::Expr> callee, std::vector<std::unique_ptr<cAST::Expr>> args) {
  std::cout << "Making function call expr\n";
  auto node = std::make_unique<cAST::CallExpr>();
  node->set_parent(head());
  cAST::Expr* raw = node.get();
  node->set_callee(std::move(callee));
  if (node->callee) node->callee->set_parent(raw);
  for (auto& arg : args) {
    arg->set_parent(raw);
    node->arguments.push_back(std::move(arg));
  }
  return raw;
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

void cAST::Driver::dump_ast(std::ostream& os) {
  if (!head_) {
    os << "<no AST>\n"; return;
  }
  os << "AST Dump:\n";
  cAST::prettyprint(*head_, os);
}