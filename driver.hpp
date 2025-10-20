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

  // scope management (call from grammar actions)
  void enter(cAST::ASTNode* n) { parent_stack_.push_back(n); }
  void leave()                { parent_stack_.pop_back();  }

  // current container node
  cAST::ASTNode* head() const {
    return parent_stack_.empty() ? nullptr : parent_stack_.back();
  }

  void parse( const char * const filename );
  void parse( std::istream &iss );

  // Reset/prepare for a new parse
  void reset();

  // Access or take the built AST
  const cAST::TranslationUnit* root() const { return head_.get(); }
  std::unique_ptr<cAST::TranslationUnit> take();

  void ensure_root();
  cAST::Decl* push_declaration(std::unique_ptr<cAST::Decl> node);
  cAST::Stmt* push_statement(std::unique_ptr<cAST::Stmt> node);
  cAST::Expr* push_expression(std::unique_ptr<cAST::Expr> node);
  cAST::TypeNode* push_type(std::unique_ptr<cAST::TypeNode> node);
  cAST::TypeNode* makeBuiltinType(cAST::BUILTIN_TYPE bt);
  cAST::Expr* makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right);
  cAST::Expr* makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr);
  cAST::Expr* makeIdentifierExpr(const std::string& name);
  cAST::Expr* singleton(std::unique_ptr<cAST::Expr> expr);
  cAST::Expr* makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr);
  cAST::Expr* makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr);
  cAST::Expr* makeConstantIntExpr(int value);
  cAST::Expr* makeConstantFloatExpr(float value);
  cAST::Expr* makeStringLiteral(const std::string& value);

  cAST::DeclSpecs makeSpecsFromType(std::unique_ptr<cAST::TypeNode> type);
  cAST::DeclSpecs combineSpecs(cAST::DeclSpecs a, cAST::DeclSpecs b);

  // root
  void set_translation_unit(std::unique_ptr<cAST::TranslationUnit> tu) {
    head_ = std::move(tu);
    enter(head_.get());
  }
  cAST::TranslationUnit* tu() const { return head_.get(); }

  // Optional helpers for common constructs
  // void set_source_name(std::string name) { source_name_ = std::move(name); }
  // const std::string& source_name() const { return source_name_; }

  // Error / diagnostic collection (parser can call these)
  // void report_error(const std::string& message);
  // void report_error(int line, int column, const std::string& message);

  // Optional: print diagnostics or the AST
  void dump_ast(std::ostream& os);
  void dump_diagnostics(std::ostream& os);

  // Did any error occur?
  bool had_error() const { return had_error_; }

private:
  void parse_helper( std::istream &stream );
  cAST::cASTParser *parser = nullptr;
  cAST::cASTScanner *scanner = nullptr;

  std::unique_ptr<cAST::TranslationUnit> head_;
  std::vector<cAST::ASTNode*> parent_stack_;
  std::string source_name_;
  bool had_error_ = false;

  struct Diag { int line, col; std::string msg; };
  std::vector<Diag> diags_;
};

} // namespace cAST