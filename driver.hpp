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

  std::unique_ptr<cAST::Expr> makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right);
  std::unique_ptr<cAST::Expr> makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr);
  std::unique_ptr<cAST::Expr> makeIdentifierExpr(const std::string& name);
  std::unique_ptr<cAST::Expr> makeConstantIntExpr(int value);
  std::unique_ptr<cAST::Expr> makeConstantFloatExpr(float value);
  std::unique_ptr<cAST::Expr> makeAssign(cAST::AssignOp op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right);
  std::unique_ptr<cAST::Expr> makeStringLiteral(const std::string& value);
  std::unique_ptr<cAST::Expr> makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr);

  std::unique_ptr<cAST::Expr> makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr);
  std::unique_ptr<cAST::Expr> makeMember(std::unique_ptr<cAST::Expr> base, const std::string& memberName, bool isPointer);
  std::unique_ptr<cAST::Expr> makeSubscript(std::unique_ptr<cAST::Expr> base, std::unique_ptr<cAST::Expr> index);
  std::unique_ptr<cAST::Expr> makeCall(std::unique_ptr<cAST::Expr> callee, std::vector<std::unique_ptr<cAST::Expr>> args);

  std::unique_ptr<cAST::Stmt> makeExprStmt(std::unique_ptr<cAST::Expr> expr);
  std::unique_ptr<cAST::Stmt> makeDeclStmt(std::unique_ptr<cAST::Decl> decl);
  std::unique_ptr<cAST::Stmt> makeNullStmt();

  std::unique_ptr<cAST::Stmt> makeCompoundStmt(std::vector<std::unique_ptr<cAST::Stmt>> stmts);

  std::unique_ptr<cAST::Stmt> makeContinue();
  std::unique_ptr<cAST::Stmt> makeBreak();
  std::unique_ptr<cAST::Stmt> makeReturn(std::unique_ptr<cAST::Expr> expr);

  std::unique_ptr<cAST::Decl> makeFunctionDeclarator(std::unique_ptr<cAST::Decl> baseDecl, std::vector<std::unique_ptr<cAST::ParamDecl>> params, bool isVariadic);
  cAST::Decl* emplaceFunctionDefinition(
    std::unique_ptr<cAST::Decl> baseDecl,
    std::unique_ptr<cAST::Stmt> body,
    DeclSpecs specs,
    bool isVariadic
  );
  std::unique_ptr<cAST::ParamDecl> makeParam(cAST::DeclSpecs specs, std::unique_ptr<cAST::Decl> decl);
  std::unique_ptr<cAST::Decl> makeIdentDeclarator(const std::string& name);
  std::unique_ptr<cAST::Decl> makeInitDecl(std::unique_ptr<cAST::Decl> decl, std::unique_ptr<cAST::Expr> init);

  cAST::Decl* emplaceDeclFromSpecs(cAST::DeclSpecs specs);
  cAST::Decl* emplaceDeclListFromSpecsAndInits(cAST::DeclSpecs specs, std::vector<std::unique_ptr<cAST::Decl>> initDecls);

  cAST::DeclSpecs makeSpecsFromBuiltinType(cAST::BUILTIN_TYPE bt);
  cAST::DeclSpecs makeSpecsFromTypeQual(cAST::TYPE_QUALIFIER tq);
  cAST::DeclSpecs makeSpecsFromStorageClass(cAST::TYPE_STORAGE_QUALIFIER sc);
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