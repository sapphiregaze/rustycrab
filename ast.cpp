#include "ast.hpp"
using namespace cAST;

void BuiltinType::accept(ASTWalker &v){ v.visit(*this); }
void PointerType::accept(ASTWalker &v){ v.visit(*this); }
void ArrayType::accept(ASTWalker &v){ v.visit(*this); }
void FunctionType::accept(ASTWalker &v){ v.visit(*this); }

// Expr
void IntegerLiteral::accept(ASTWalker &v){ v.visit(*this); }
void FloatingLiteral::accept(ASTWalker &v){ v.visit(*this); }
void CharLiteral::accept(ASTWalker &v){ v.visit(*this); }
void StringLiteral::accept(ASTWalker &v){ v.visit(*this); }
void IdentifierExpr::accept(ASTWalker &v){ v.visit(*this); }
void MemberExpr::accept(ASTWalker &v){ v.visit(*this); }
void ArraySubscriptExpr::accept(ASTWalker &v){ v.visit(*this); }
void CallExpr::accept(ASTWalker &v){ v.visit(*this); }
void CastExpr::accept(ASTWalker &v){ v.visit(*this); }
void UnaryExpr::accept(ASTWalker &v){ v.visit(*this); }
void BinaryExpr::accept(ASTWalker &v){ v.visit(*this); }
void ConditionalExpr::accept(ASTWalker &v){ v.visit(*this); }
// void InitListExpr::accept(ASTWalker &v){ v.visit(*this); }
// void CompoundLiteralExpr::accept(ASTWalker &v){ v.visit(*this); }


// Stmt
void NullStmt::accept(ASTWalker &v){ v.visit(*this); }
void ExprStmt::accept(ASTWalker &v){ v.visit(*this); }
void CompoundStmt::accept(ASTWalker &v){ v.visit(*this); }
void IfStmt::accept(ASTWalker &v){ v.visit(*this); }
void WhileStmt::accept(ASTWalker &v){ v.visit(*this); }
void DoWhileStmt::accept(ASTWalker &v){ v.visit(*this); }
void ForStmt::accept(ASTWalker &v){ v.visit(*this); }
// void BreakStmt::accept(ASTWalker &v){ v.visit(*this); }
// void ContinueStmt::accept(ASTWalker &v){ v.visit(*this); }
// void GotoStmt::accept(ASTWalker &v){ v.visit(*this); }
void ReturnStmt::accept(ASTWalker &v){ v.visit(*this); }


// Decl
void VarDecl::accept(ASTWalker &v){ v.visit(*this); }
void ParamDecl::accept(ASTWalker &v){ v.visit(*this); }
void FieldDecl::accept(ASTWalker &v){ v.visit(*this); }
void FunctionDecl::accept(ASTWalker &v){ v.visit(*this); }
void DeclStmt::accept(ASTWalker &v){ v.visit(*this); }
void TranslationUnit::accept(ASTWalker &v){ v.visit(*this); }

static const char* builtinToString(BUILTIN_TYPE b){
  switch(b){
    case BUILTIN_TYPE::Void: return "void";
    case BUILTIN_TYPE::Bool: return "_Bool";
    case BUILTIN_TYPE::Char: return "char";
    case BUILTIN_TYPE::Complex: return "complex";
    case BUILTIN_TYPE::Generic: return "generic";
    case BUILTIN_TYPE::Imaginary: return "imaginary";
    case BUILTIN_TYPE::Schar: return "signed char";
    case BUILTIN_TYPE::Uchar: return "unsigned char";
    case BUILTIN_TYPE::Short: return "short";
    case BUILTIN_TYPE::Ushort: return "unsigned short";
    case BUILTIN_TYPE::Int: return "int";
    case BUILTIN_TYPE::UInt: return "unsigned int";
    case BUILTIN_TYPE::Long: return "long";
    case BUILTIN_TYPE::Unsigned_Long: return "unsigned long";
    case BUILTIN_TYPE::Long_Long: return "long long";
    case BUILTIN_TYPE::Unsigned_Long_Long: return "unsigned long long";
    case BUILTIN_TYPE::Float: return "float";
    case BUILTIN_TYPE::Double: return "double";
    case BUILTIN_TYPE::Long_Double: return "long double";
  }
  return "<builtin?>";
}

struct Printer : ASTWalker {
  std::ostream &os;
  int indentLevel = 0;

  explicit Printer(std::ostream &output) : os(output) {}

  void setIndentLevel(int level) {
    indentLevel = level;
  }

  void resetIndentLevel() {
    indentLevel = 0;
  }

  void indent() {
    for(int i=0;i<indentLevel;++i){
      os << " ";
    }
  }

  void printQual(std::vector<TYPE_QUALIFIER> q){
    for(auto qual : q){
      switch(qual){
        case TYPE_QUALIFIER::Const: os << "const "; break;
        case TYPE_QUALIFIER::Restrict: os << "restrict "; break;
        case TYPE_QUALIFIER::Volatile: os << "volatile "; break;
        case TYPE_QUALIFIER::Atomic: os << "_Atomic "; break;
        default: break;
      }
    }
  }

  void printStorage(const std::vector<TYPE_STORAGE_QUALIFIER> &storage){
    for(auto qual : storage){
      switch(qual){
        case TYPE_STORAGE_QUALIFIER::Typedef: os << "typedef "; break;
        case TYPE_STORAGE_QUALIFIER::Static: os << "static "; break;
        case TYPE_STORAGE_QUALIFIER::Extern: os << "extern "; break;
        case TYPE_STORAGE_QUALIFIER::Register: os << "register "; break;
        case TYPE_STORAGE_QUALIFIER::Auto: os << "auto "; break;
        case TYPE_STORAGE_QUALIFIER::Thread_Local: os << "_Thread_local "; break;
        case TYPE_STORAGE_QUALIFIER::None: break;
      }
    }
  }

  static const char* binOpToString(BINARY_OPERATOR k){
    switch(k){
      case BINARY_OPERATOR::MULTIPLY: return "*";
      case BINARY_OPERATOR::DIVIDE: return "/";
      case BINARY_OPERATOR::MODULO: return "%";
      case BINARY_OPERATOR::ADD: return "+";
      case BINARY_OPERATOR::SUBTRACT: return "-";
      case BINARY_OPERATOR::LEFT_SHIFT: return "<<";
      case BINARY_OPERATOR::RIGHT_SHIFT: return ">>";
      case BINARY_OPERATOR::LESS_THAN: return "<";
      case BINARY_OPERATOR::GREATER_THAN: return ">";
      case BINARY_OPERATOR::LESS_EQUAL: return "<=";
      case BINARY_OPERATOR::GREATER_EQUAL: return ">=";
      case BINARY_OPERATOR::EQUAL: return "==";
      case BINARY_OPERATOR::NOT_EQUAL: return "!=";
      case BINARY_OPERATOR::BITWISE_AND: return "&";
      case BINARY_OPERATOR::BITWISE_XOR: return "^";
      case BINARY_OPERATOR::BITWISE_OR: return "|";
      case BINARY_OPERATOR::LOGICAL_AND: return "&&";
      case BINARY_OPERATOR::LOGICAL_OR: return "||";
      case BINARY_OPERATOR::ASSIGN: return "=";
      case BINARY_OPERATOR::MULTASSIGN: return "*=";
      case BINARY_OPERATOR::DIVASSIGN: return "/=";
      case BINARY_OPERATOR::MODASSIGN: return "%=";
      case BINARY_OPERATOR::ADDASSIGN: return "+=";
      case BINARY_OPERATOR::SUBASSIGN: return "-=";
      case BINARY_OPERATOR::LEFTSHIFTASSIGN: return "<<=";
      case BINARY_OPERATOR::RIGHTSHIFTASSIGN: return ">>=";
      case BINARY_OPERATOR::ANDASSIGN: return "&=";
      case BINARY_OPERATOR::XORASSIGN: return "^=";
      case BINARY_OPERATOR::ORASSIGN: return "|=";
      case BINARY_OPERATOR::COMMA: return ",";
    }
    return "?";
  }

  void visit(UnaryExpr &e) override {
    switch(e.op){
      case UNARY_OPERATOR::INCREMENT: if(e.operand){ e.operand->accept(*this); os << "++";} break;
      case UNARY_OPERATOR::DECREMENT: if(e.operand){ e.operand->accept(*this); os << "--";} break;
      case UNARY_OPERATOR::PRE_INC: os << "++"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::PRE_DEC: os << "--"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::ADDRESS_OF: os << "&"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::DEREFERENCE: os << "*"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::PLUS: os << "+"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::MINUS: os << "-"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::BITWISE_NOT: os << "~"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::LOGICAL_NOT: os << "!"; if(e.operand) e.operand->accept(*this); break;
      case UNARY_OPERATOR::SIZEOF:
        os << "sizeof";
        if(e.typeOperand){ os << "("; e.typeOperand->accept(*this); os << ")"; }
        else if(e.operand){ os << " "; e.operand->accept(*this); }
        break;
      case UNARY_OPERATOR::ALIGNOF:
        os << "_Alignof";
        if(e.typeOperand){ os << "("; e.typeOperand->accept(*this); os << ")"; }
        else if(e.operand){ os << " "; e.operand->accept(*this); }
        break;
    }
  }

  void visit(IntegerLiteral &e) override {
    indent();
    os << "Integer Literal: ";
    os << e.literal;
    os << "\n";
  }

  void visit(FloatingLiteral &e) override {
    indent();
    os << "Floating Literal: ";
    os << e.literal;
    os << "\n";
  }

  void visit(CharLiteral &e) override {
    indent();
    os << "Character Literal: ";
    os << e.literal;
    os << "\n";
  }

  void visit(StringLiteral &e) override {
    indent();
    os << "String Literal: ";
    os << '"' << e.literal << '"';
    os << "\n";
  }

  void visit(IdentifierExpr &e) override {
    indent();
    os << "Identifier: ";
    os << e.literal;
    os << "\n";
  }

  void visit(BuiltinType &t) override {
    indent();
    os << "Builtin Type: ";
    printQual(t.qualifiers);
    os << builtinToString(t.type);
    os << "\n";
  }

  void visit(CastExpr &e) override {
    os << "(";
    if(e.typeOperand) e.typeOperand->accept(*this);
    os << ") ";
    if(e.operand) e.operand->accept(*this);
  }

  void visit(BinaryExpr &e) override {
    indent();
    os << "Binary Expression: ";
    setIndentLevel(indentLevel + 2);
    if(e.left){
      e.left->accept(*this);
    }
    os << " " << binOpToString(e.op) << " ";
    if(e.right) {
      e.right->accept(*this);
    }
  }

  void visit(MemberExpr &e) override {
    if(e.base) e.base->accept(*this);
    os << (e.isPointer ? "->" : ".");
    os << e.memberName;
  }

  void visit(ArraySubscriptExpr &e) override {
    if(e.base) e.base->accept(*this);
    os << "[";
    if(e.index) e.index->accept(*this);
    os << "]";
  }

  void visit(CallExpr &e) override {
    indent();
    os << "Function Call: ";
    setIndentLevel(indentLevel + 2);
    if(e.callee) e.callee->accept(*this);
    os << "(";
    for(size_t i = 0; i < e.arguments.size(); ++i){
      if(i) os << ", ";
      if(e.arguments[i]) e.arguments[i]->accept(*this);
    }
    os << ")";
  }

  void visit(NullStmt&) override {
    indent();
    os << "Null Statement";
    os << ";\n";
  }

  void visit(ExprStmt &s) override {
    indent();
    os << "Expression Statement: ";
    setIndentLevel(indentLevel + 2);
    if(s.expr) s.expr->accept(*this);
    os << ";\n";
  }

  void visit(IfStmt &s) override {
    indent();
    os << "If Statement:\n";
    setIndentLevel(indentLevel + 2);
    os << "if (";
    if(s.cond) s.cond->accept(*this);
    os << ") ";
    if(s.thenBranch) {
      s.thenBranch->accept(*this);
    } else {
      os << "{}";
    }
    if(s.elseBranch) {
      os << " else ";
      s.elseBranch->accept(*this);
    }
  }

  void visit(VarDecl &d) override {
    indent();
    os << "Variable Declaration: ";
    setIndentLevel(indentLevel + 2);
    printStorage(d.specs.storage);
    if(d.specs.isInline) os << "inline ";
    if(d.specs.isNoreturn) os << "_Noreturn ";
    if(d.type) d.type->accept(*this);

    if(!d.name.empty()) os << " " << d.name;

    if(d.init){
      os << " = ";
      d.init->accept(*this);
    }
    os << ";\n";
  }

  void visit(ParamDecl &d) override {
    indent();
    os << "Parameter Declaration:\n";
    setIndentLevel(indentLevel + 2);
    if(d.type) d.type->accept(*this);
    if(!d.name.empty()) os << " " << d.name;
    if(d.isVariadic) os << " ...";
  }

  void visit(FieldDecl &d) override {
    indent();
    os << "Field Declaration: ";
    setIndentLevel(indentLevel + 2);
    if(d.type) d.type->accept(*this);
    if(!d.name.empty()) os << " " << d.name;
    // if(d.bitWidth && *d.bitWidth){
    //   os << " : ";
    //   (*d.bitWidth)->get()->accept(*this);
    // }
    os << ";\n";
  }

  void visit(FunctionDecl &d) override {
    indent();
    os << "Function Declaration:\n";
    setIndentLevel(indentLevel + 2);
    if(d.return_type) d.return_type->accept(*this);
    os << " " << d.name << "(";

    for(size_t i = 0; i < d.params.size(); ++i){
      if(i) os << ", ";
      auto &p = *d.params[i];
      p.accept(*this);
    }

    if(d.isVariadic){
      if(!d.params.empty()) {
        os << ", ";
      }
      os << "...";
    }

    os << ") ";

    if(d.isDefinition && d.body){
      d.body->accept(*this);
    } else {
      os << ";\n";
    }
  }

  void visit(DeclStmt &s) override {
    indent();
    os << "Declaration Statement:\n";
    if(s.declaration) s.declaration->accept(*this);
  }

  void visit(ConditionalExpr &e) override {
    indent();
    os << "Conditional Expression:\n";
    setIndentLevel(indentLevel + 2);
    if(e.cond) e.cond->accept(*this);
    os << " ? ";
    if(e.thenExpr) e.thenExpr->accept(*this);
    os << " : ";
    if(e.elseExpr) e.elseExpr->accept(*this);
  }

  void visit(ReturnStmt &s) override {
    indent();
    os << "Return Statement:\n";
    if(s.value){
      setIndentLevel(indentLevel + 2);
      s.value->accept(*this);
    }
    os << ";\n";
  }

  void visit(TranslationUnit &tu) override {
    std::cout << "AST Head:" << std::endl;
    setIndentLevel(indentLevel + 2);
    for(const auto& child : tu.children()){
      if(child){
        child->accept(*this);
      }
    }
  }

};

void cAST::prettyprint(cAST::ASTNode &n, std::ostream &os){
  Printer p{os};
  n.accept(p);
}