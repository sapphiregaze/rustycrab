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
  auto* type = new cAST::BuiltinType();
  type->set_type(bt);

  return static_cast<cAST::TypeNode*>(type);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeUnary(cAST::UNARY_OPERATOR op, std::unique_ptr<cAST::Expr> expr) {
  auto* unary = new cAST::UnaryExpr();
  unary->set_op(op);
  unary->set_operand(std::move(expr));

  return std::unique_ptr<cAST::Expr>(unary);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeBinary(cAST::BINARY_OPERATOR op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right) {
  auto* binary = new cAST::BinaryExpr();
  binary->set_op(op);
  binary->set_left(std::move(left));
  binary->set_right(std::move(right));

  return std::unique_ptr<cAST::Expr>(binary);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeCast(std::unique_ptr<cAST::TypeNode> type, std::unique_ptr<cAST::Expr> expr) {
  auto* cast = new cAST::CastExpr();
  cast->set_typeOperand(std::move(type));
  cast->set_operand(std::move(expr));

  return std::unique_ptr<cAST::Expr>(cast);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeCond(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Expr> thenExpr, std::unique_ptr<cAST::Expr> elseExpr) {
  auto* condExpr = new cAST::ConditionalExpr();
  condExpr->set_cond(std::move(cond));
  condExpr->set_thenExpr(std::move(thenExpr));
  condExpr->set_elseExpr(std::move(elseExpr));

  // return static_cast<cAST::Expr*>(condExpr);
  return std::unique_ptr<cAST::Expr>(condExpr);
}

std::unique_ptr<cAST::Decl> cAST::Driver::makeIdentDeclarator(const std::string& name) {
  auto* decl = new cAST::VarDecl();
  decl->name = name;

  return std::unique_ptr<cAST::Decl>(decl);
}

std::unique_ptr<cAST::Decl> cAST::Driver::makeFunctionDeclarator(
  std::unique_ptr<cAST::Decl> baseDecl,
  std::vector<std::unique_ptr<cAST::ParamDecl>> params,
  bool isVariadic
) {
  auto* funcDecl = new cAST::FunctionDecl();
  // Set the base declarator (e.g., the function name)
  if (auto* varDecl = dynamic_cast<cAST::VarDecl*>(baseDecl.get())) {
    funcDecl->name = varDecl->name;
  } else {
    throw std::logic_error("Base declarator is not a VarDecl for function declarator");
  }
  funcDecl->set_params(std::move(params));
  funcDecl->isVariadic = isVariadic;

  return std::unique_ptr<cAST::Decl>(funcDecl);
}

cAST::Decl* cAST::Driver::makeFunctionDefinition(
  std::unique_ptr<cAST::Decl> baseDecl,
  std::unique_ptr<cAST::Stmt> body,
  DeclSpecs specs,
  bool isVariadic
) {
  auto* funcDecl = new cAST::FunctionDecl();

  // TODO it seems like this will always lead to a previous function declaration found
  if (auto* varDecl = dynamic_cast<cAST::VarDecl*>(baseDecl.get())) {
    std::cout << "Function name from VarDecl: " << varDecl->name << std::endl;
    funcDecl->name = varDecl->name;
  } else if (auto* prevFnDecl = dynamic_cast<cAST::FunctionDecl*>(baseDecl.get())) {
    std::cout << "Previous function declaration found: " << prevFnDecl->name << std::endl;
    funcDecl->name = prevFnDecl->name;
    funcDecl->set_params(std::move(prevFnDecl->params));
  } else {
    throw std::logic_error("Base declarator is not a VarDecl or FunctionDecl for function definition");
  }

  funcDecl->set_specs(specs);
  funcDecl->set_body(std::move(body));
  // funcDecl->isVariadic = isVariadic;

  return static_cast<cAST::Decl*>(funcDecl);
}

std::unique_ptr<cAST::ParamDecl> cAST::Driver::makeParam(cAST::DeclSpecs specs, std::unique_ptr<cAST::Decl> decl) {
  cAST::ASTNode* parent = head();

  auto param = new cAST::ParamDecl();
  auto* ident = dynamic_cast<cAST::VarDecl*>(decl.get());
  auto* type = new cAST::BuiltinType();

  param->name = ident->name;

  // TODO i'm worried that there is some other information that needs to be contained in type
  type->set_type(specs.type);
  param->type = std::unique_ptr<cAST::BuiltinType>(type);

  return std::unique_ptr<cAST::ParamDecl>(param);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeIdentifierExpr(const std::string& name) {
  auto* ident = new cAST::IdentifierExpr();
  ident->set_name(name);

  return std::unique_ptr<cAST::Expr>(ident);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeConstantIntExpr(int value) {
  auto* ident = new cAST::IntegerLiteral();
  ident->set_value(value);

  return std::unique_ptr<cAST::Expr>(ident);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeConstantFloatExpr(float value) {
  auto* lit = new cAST::FloatingLiteral();
  lit->set_value(value);

  return std::unique_ptr<cAST::Expr>(lit);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeStringLiteral(const std::string& value) {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for string literal");

  auto* lit = new cAST::StringLiteral();
  lit->set_literal(value);

  return std::unique_ptr<cAST::Expr>(lit);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeNullStmt() {
  cAST::ASTNode* parent = head();
  assert(parent && "ensure_root failed to provide a parent");
  if (!parent) throw std::logic_error("No valid parent at head() for null statement");

  auto* stmt = new cAST::NullStmt();

  return std::unique_ptr<cAST::Stmt>(stmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeExprStmt(std::unique_ptr<cAST::Expr> expr) {
  auto* stmt = new cAST::ExprStmt();
  stmt->set_expr(std::move(expr));

  return std::unique_ptr<cAST::Stmt>(stmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeCompoundStmt(std::vector<std::unique_ptr<cAST::Stmt>> stmts) {
  auto* compoundStmt = new cAST::CompoundStmt();
  for (auto& s : stmts) {
    compoundStmt->addStmtOrExpr(std::move(s));
  }

  return std::unique_ptr<cAST::Stmt>(compoundStmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeContinue() {
  auto* stmt = new cAST::ContinueStmt();
  return std::unique_ptr<cAST::Stmt>(stmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeBreak() {
  auto* stmt = new cAST::BreakStmt();
  return std::unique_ptr<cAST::Stmt>(stmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeReturn(std::unique_ptr<cAST::Expr> expr) {
  auto* stmt = new cAST::ReturnStmt();
  stmt->value = std::move(expr);
  return std::unique_ptr<cAST::Stmt>(stmt);
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
  auto* decl = new cAST::VarDecl();
  decl->set_specs(std::make_unique<cAST::DeclSpecs>(std::move(specs)));

  return static_cast<cAST::Decl*>(decl);
}

std::unique_ptr<cAST::DeclGroup> cAST::Driver::makeDeclGroupFromSpecsAndInits(cAST::DeclSpecs specs, std::vector<std::unique_ptr<cAST::Decl>> initDecls) {
  auto* group = new DeclGroup();
  auto decls = std::vector<std::unique_ptr<Decl>>();

  for (auto& decl : initDecls) {
    if (auto* varDeclRaw = dynamic_cast<VarDecl*>(decl.get())) {
      auto varDecl(static_cast<VarDecl*>(decl.release()));

      varDecl->set_specs(std::make_unique<DeclSpecs>(specs));
      decls.push_back(std::unique_ptr<cAST::Decl>(varDecl)); // TODO this hints at the fact that maybe all pointers should be smart pointers?
    } else if (auto* functionDeclRaw = dynamic_cast<FunctionDecl*>(decl.get())) {
      auto functionDecl(static_cast<FunctionDecl*>(decl.release()));

      functionDecl->set_specs(specs);
      decls.push_back(std::unique_ptr<cAST::Decl>(functionDecl)); // TODO this hints at the fact that maybe all pointers should be smart pointers?
    } else {
      throw std::logic_error("Expected VarDecl or FuncDecl in initDecls");
    }
  }

  group->set_decls(std::move(decls));
  return std::unique_ptr<cAST::DeclGroup>(group);
}

// inits vardecl, errors for functiondecl if init provided
std::unique_ptr<cAST::Decl> cAST::Driver::makeInitDecl(std::unique_ptr<cAST::Decl> decl, std::unique_ptr<cAST::Expr> init) {
  if (auto* varDecl = dynamic_cast<cAST::VarDecl*>(decl.get())) {
    varDecl->set_init(std::move(init));
    return decl;
  } else if (auto* functionDecl = dynamic_cast<cAST::FunctionDecl*>(decl.get())) {
    if (init) {
      throw std::logic_error("FunctionDecl cannot have an initializer");
    }
    return decl;
  } else {
    throw std::logic_error("Expected VarDecl or FuncDecl in makeInitDecl");
  }
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeDeclStmt(std::unique_ptr<cAST::Decl> decl) {
  auto* stmt = new cAST::DeclStmt();
  stmt->set_decl(std::move(decl));

  return std::unique_ptr<cAST::Stmt>(stmt);
}

std::unique_ptr<cAST::Stmt> cAST::Driver::makeIfStmt(std::unique_ptr<cAST::Expr> cond, std::unique_ptr<cAST::Stmt> thenStmt, std::unique_ptr<cAST::Stmt> elseStmt) {
  auto* ifStmt = new cAST::IfStmt();

  ifStmt->set_condition(std::move(cond));
  ifStmt->set_thenStmt(std::move(thenStmt));
  ifStmt->set_elseStmt(std::move(elseStmt));

  return std::unique_ptr<cAST::Stmt>(ifStmt);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeMember(std::unique_ptr<cAST::Expr> base, const std::string& memberName, bool isPointer) {
  auto* node = new cAST::MemberExpr();
  node->set_base(std::move(base));
  if (node->base) node->base->set_parent(node);
  node->memberName = memberName;
  node->isPointer = isPointer;

  return std::unique_ptr<cAST::Expr>(node);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeAssign(cAST::AssignOp op, std::unique_ptr<cAST::Expr> left, std::unique_ptr<cAST::Expr> right) {
  auto* assign = new cAST::AssignExpr();
  assign->set_op(op);
  assign->set_left(std::move(left));
  assign->set_right(std::move(right));

  return std::unique_ptr<cAST::Expr>(assign);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeSubscript(std::unique_ptr<cAST::Expr> base, std::unique_ptr<cAST::Expr> index) {
  auto node = new cAST::ArraySubscriptExpr();

  node->set_base(std::move(base));
  if (node->base) node->base->set_parent(node);
  node->set_index(std::move(index));

  return std::unique_ptr<cAST::Expr>(node);
}

std::unique_ptr<cAST::Expr> cAST::Driver::makeCall(std::unique_ptr<cAST::Expr> callee, std::vector<std::unique_ptr<cAST::Expr>> args) {
  auto* call = new cAST::CallExpr();
  call->set_callee(std::move(callee));
  if (call->callee) call->callee->set_parent(call); // TODO careful here

  call->arguments = std::move(args);

  return std::unique_ptr<cAST::Expr>(call);
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