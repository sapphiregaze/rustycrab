#pragma once
#include <memory>
#include <string>
#include <vector>
#include <ostream>

#include "scanner.hpp"
#include "parser.tab.hh"
#include "ast.hpp"
namespace cAST {

class Driver {
public:

  Driver() = default;

  virtual ~Driver();

  void parse( const char * const filename );
  void parse( std::istream &iss );

  // Reset/prepare for a new parse
  void reset();

  // Access or take the built AST
  const AST::TranslationUnit* root() const { return head_.get(); }
  std::unique_ptr<AST::TranslationUnit> take();

  // Hooks used by grammar actions (adapt to your node types)
  void ensure_root();
  void push_declaration(std::unique_ptr<AST::Decl> node);

  // Optional helpers for common constructs
  // void set_source_name(std::string name) { source_name_ = std::move(name); }
  // const std::string& source_name() const { return source_name_; }

  // Error / diagnostic collection (parser can call these)
  // void report_error(const std::string& message);
  // void report_error(int line, int column, const std::string& message);

  // Optional: print diagnostics or the AST
  void dump_ast(std::ostream& os) const;
  void dump_diagnostics(std::ostream& os) const;

  // Did any error occur?
  bool had_error() const { return had_error_; }

private:
  void parse_helper( std::istream &stream );
  cAST::cASTParser *parser = nullptr;
  cAST::cASTScanner *scanner = nullptr;

  std::unique_ptr<AST::TranslationUnit> head_;
  std::string source_name_;
  bool had_error_ = false;

  struct Diag { int line, col; std::string msg; };
  std::vector<Diag> diags_;
};

} // namespace cAST