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

  void enter(cAST::ASTNode* n) {
    parent_stack_.push_back(n);
  }
  void leave() {
    if (!parent_stack_.empty()) parent_stack_.pop_back();
  }

  cAST::ASTNode* head() const {
    if (!parent_stack_.empty()) return parent_stack_.back();
    return head_;
  }
  cAST::ASTNode* root() const {
    return parent_stack_.empty() ? head_ : parent_stack_.front();
  }

  void parse( const char * const filename );
  void parse( std::istream &iss );

  void ensure_root();
  cAST::TypeNode* makeBuiltinType(cAST::BUILTIN_TYPE bt);
  cAST::Expr* makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right);
  cAST::Expr* makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr);
  cAST::Expr* makeIdentifierExpr(const std::string& name);
  cAST::Expr* makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr);
  cAST::Expr* makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr);
  cAST::Expr* makeConstantIntExpr(int value);
  cAST::Expr* makeConstantFloatExpr(float value);
  cAST::Expr* makeAssign(cAST::AssignOp op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right);
  cAST::Expr* makeStringLiteral(const std::string& value);
  cAST::Expr* makeMember(std::unique_ptr<cAST::Expr> base, const std::string& memberName, bool isPointer);
  cAST::Expr* makeSubscript(std::unique_ptr<cAST::Expr> base, std::unique_ptr<cAST::Expr> index);
  cAST::Expr* makeCall(std::unique_ptr<cAST::Expr> callee, std::vector<std::unique_ptr<cAST::Expr>> args);
  cAST::Stmt* makeNullStmt();
  cAST::Decl* makeIdentDeclarator(const std::string& name);
  cAST::Stmt* makeExprStmt(std::unique_ptr<cAST::Expr> expr);
  cAST::Stmt* makeCompoundStmt(std::vector<std::unique_ptr<cAST::Stmt>> stmts);
  cAST::DeclSpecs makeSpecsFromBuiltinType(cAST::BUILTIN_TYPE bt);
  cAST::DeclSpecs makeSpecsFromTypeQual(cAST::TYPE_QUALIFIER tq);
  cAST::DeclSpecs makeSpecsFromStorageClass(cAST::TYPE_STORAGE_QUALIFIER sc);
  cAST::Decl* makeDeclFromSpecs(cAST::DeclSpecs specs);
  cAST::Decl* makeDeclListFromSpecsAndInits(cAST::DeclSpecs specs, std::vector<cAST::Decl*> initDecls);
  cAST::Stmt* makeDeclStmt(std::unique_ptr<cAST::Decl> decl);

  cAST::DeclSpecs makeSpecsFromTypeNode(std::unique_ptr<cAST::TypeNode> type);
  cAST::DeclSpecs combineSpecs(cAST::DeclSpecs a, cAST::DeclSpecs b);

  void dump_ast(std::ostream& os);

  bool had_error() const { return had_error_; }

private:
  void parse_helper( std::istream &stream );
  cAST::cASTParser *parser = nullptr;
  cAST::cASTScanner *scanner = nullptr;

  cAST::TranslationUnit* head_ = nullptr;
  std::vector<cAST::ASTNode*> parent_stack_;
  std::string source_name_;
  bool had_error_ = false;
};

} // namespace cAST