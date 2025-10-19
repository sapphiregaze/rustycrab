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
  parse_helper( in_file );
  return;
}

void cAST::Driver::parse( std::istream &stream ) {
  if( ! stream.good()  && stream.eof() ) {
    return;
  }
  //else
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
  if (!head_) head_ = std::make_unique<AST::TranslationUnit>();
}

// void cAST::Driver::push_declaration(std::unique_ptr<AST::Decl> node) {
//   ensure_root();
//   head_->declarations.emplace_back(std::move(node));
// }

// void cAST::Driver::report_error(const std::string& message) {
//   had_error_ = true;
//   diags_.push_back(Diag{ -1, -1, message });
// }

// void cAST::Driver::report_error(int line, int column, const std::string& message) {
//   had_error_ = true;
//   diags_.push_back(Diag{ line, column, message });
// }

void cAST::Driver::dump_ast(std::ostream& os) const {
  if (!head_) {
    os << "<no AST>\n"; return;
  }
  os << "AST Dump:\n";
  // head_->dump(os, 0);
  // AST::print(*head_, os);
}

void cAST::Driver::dump_diagnostics(std::ostream& os) const {
  for (const auto& d : diags_) {
    if (d.line >= 0)
      os << source_name_ << ":" << d.line << ":" << d.col << ": error: " << d.msg << "\n";
    else
      os << source_name_ << ": error: " << d.msg << "\n";
  }
}
