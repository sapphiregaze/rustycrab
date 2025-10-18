#include "ast.h"

void BuiltinType::accept(ASTWalker &v){ v.visit(*this); }
void PointerType::accept(ASTWalker &v){ v.visit(*this); }
void ArrayType::accept(ASTWalker &v){ v.visit(*this); }
void FunctionType::accept(ASTWalker &v){ v.visit(*this); }
// void RecordType::accept(ASTWalker &v){ v.visit(*this); }
void EnumType::accept(ASTWalker &v){ v.visit(*this); }
void TypedefType::accept(ASTWalker &v){ v.visit(*this); }

// Expr
void IntegerLiteral::accept(ASTWalker &v){ v.visit(*this); }
void FloatingLiteral::accept(ASTWalker &v){ v.visit(*this); }
void CharLiteral::accept(ASTWalker &v){ v.visit(*this); }
void StringLiteral::accept(ASTWalker &v){ v.visit(*this); }
void IdentifierExpr::accept(ASTWalker &v){ v.visit(*this); }
// void MemberExpr::accept(ASTWalker &v){ v.visit(*this); }
// void ArraySubscriptExpr::accept(ASTWalker &v){ v.visit(*this); }
// void CallExpr::accept(ASTWalker &v){ v.visit(*this); }
// void CastExpr::accept(ASTWalker &v){ v.visit(*this); }
void UnaryExpression::accept(ASTWalker &v){ v.visit(*this); }
void BinaryExpression::accept(ASTWalker &v){ v.visit(*this); }
// void ConditionalOp::accept(ASTWalker &v){ v.visit(*this); }
// void InitListExpr::accept(ASTWalker &v){ v.visit(*this); }
// void CompoundLiteralExpr::accept(ASTWalker &v){ v.visit(*this); }


// Stmt
void NullStmt::accept(ASTWalker &v){ v.visit(*this); }
void ExprStmt::accept(ASTWalker &v){ v.visit(*this); }
// void CompoundStmt::accept(ASTWalker &v){ v.visit(*this); }
void IfStmt::accept(ASTWalker &v){ v.visit(*this); }
// void WhileStmt::accept(ASTWalker &v){ v.visit(*this); }
// void DoWhileStmt::accept(ASTWalker &v){ v.visit(*this); }
// void ForStmt::accept(ASTWalker &v){ v.visit(*this); }
void SwitchStmt::accept(ASTWalker &v){ v.visit(*this); }
// void CaseStmt::accept(ASTWalker &v){ v.visit(*this); }
// void DefaultStmt::accept(ASTWalker &v){ v.visit(*this); }
// void LabelStmt::accept(ASTWalker &v){ v.visit(*this); }
// void BreakStmt::accept(ASTWalker &v){ v.visit(*this); }
// void ContinueStmt::accept(ASTWalker &v){ v.visit(*this); }
// void GotoStmt::accept(ASTWalker &v){ v.visit(*this); }
// void ReturnStmt::accept(ASTWalker &v){ v.visit(*this); }


// Decl
// void VarDecl::accept(ASTWalker &v){ v.visit(*this); }
// void ParamDecl::accept(ASTWalker &v){ v.visit(*this); }
// void TypedefDecl::accept(ASTWalker &v){ v.visit(*this); }
// void FieldDecl::accept(ASTWalker &v){ v.visit(*this); }
// void RecordDecl::accept(ASTWalker &v){ v.visit(*this); }
// void EnumDecl::accept(ASTWalker &v){ v.visit(*this); }
// void FunctionDecl::accept(ASTWalker &v){ v.visit(*this); }
// void DeclStmt::accept(ASTWalker &v){ v.visit(*this); }
void TranslationUnit::accept(ASTWalker &v){ v.visit(*this); }

static const char* builtinToString(BUILTIN_TYPE b){
  switch(b){
    case BUILTIN_TYPE::VOID: return "void";
    case BUILTIN_TYPE::BOOL: return "_Bool";
    case BUILTIN_TYPE::CHAR: return "char";
    case BUILTIN_TYPE::SCHAR: return "signed char";
    case BUILTIN_TYPE::UCHAR: return "unsigned char";
    case BUILTIN_TYPE::SHORT: return "short";
    case BUILTIN_TYPE::USHORT: return "unsigned short";
    case BUILTIN_TYPE::INT: return "int";
    case BUILTIN_TYPE::UINT: return "unsigned int";
    case BUILTIN_TYPE::LONG: return "long";
    case BUILTIN_TYPE::UNSIGNED_LONG: return "unsigned long";
    case BUILTIN_TYPE::LONG_LONG: return "long long";
    case BUILTIN_TYPE::UNSIGNED_LONG_LONG: return "unsigned long long";
    case BUILTIN_TYPE::FLOAT: return "float";
    case BUILTIN_TYPE::DOUBLE: return "double";
    case BUILTIN_TYPE::LONG_DOUBLE: return "long double";
  }
  return "<builtin?>";
}

struct Printer : ASTWalker {
  std::ostream &os;
  int indent{0};

  Printer(std::ostream &output) : os(output) {}

  void printQual(std::vector<TYPE_QUALIFIER> q){
    for(auto qual : q){
      switch(qual){
        case TYPE_QUALIFIER::CONST: os << "const "; break;
        case TYPE_QUALIFIER::RESTRICT: os << "restrict "; break;
        case TYPE_QUALIFIER::VOLATILE: os << "volatile "; break;
        case TYPE_QUALIFIER::ATOMIC: os << "_Atomic "; break;
      }
    }
  }

  void visit(BuiltinType &t) override {
    printQual(t.qualifiers);
    os << builtinToString(t.type);
  }

  // void visit(PointerType &t) override {
  //   printQual(t.qualifiers);
  //   if(t.pointee) t.pointee->accept(*this);
  //   os << " *";
  // }

  // void visit(ArrayType &t) override {
  //   if(t.element) t.element->accept(*this);
  //   os << "[";
  //   if(t.isVLA) os << "*";
  //   else if(t.sizeExpr && *t.sizeExpr){ (*t.sizeExpr)->get()->accept(*this); }
  //   os << "]";
  // }

  // void visit(FunctionType &t) override {
  //   if(t.returnType) t.returnType->accept(*this);
  //   os << " (";
  //   for(size_t i=0;i<t.params.size();++i){
  //     if(i) os << ", ";
  //     auto &p = *t.params[i];
  //     if(p.type) p.type->accept(*this);
  //     if(!p.name.empty()) os << " " << p.name;
  //   }
  //   if(t.isVarArg){ if(!t.params.empty()) os << ", "; os << "..."; }
  //   os << ")";
  // }

  // void visit(RecordType &t) override {
  //   printQual(t.qualifiers);
  //   os << (t.kind==RecordKind::Struct?"struct ":"union ");
  //   if(!t.tag.empty()) os << t.tag;
  //   if(!t.fields.empty()) {
  //     os << " {\n"; IndentGuard g(indent);
  //     for(auto &f : t.fields){ pad(); if(f.type) f.type->accept(*this); os << " " << f.name << ";\n"; }
  //     pad(); os << "}";
  //   }
  // }

  // void visit(EnumType &t) override {
  //   printQual(t.qualifiers);
  //   os << "enum "; if(!t.tag.empty()) os << t.tag;
  //   if(!t.constants.empty()){
  //     os << " {";
  //     for(size_t i=0;i<t.constants.size();++i){
  //       auto &c = t.constants[i];
  //       os << (i?", ":" ") << c.name;
  //       if(c.value && *c.value){ os << " = "; (*c.value)->get()->accept(*this); }
  //     }
  //     os << " }";
  //   }
  // }

  void visit(TypedefType &t) override { printQual(t.qualifiers); os << t.name; }

  //=== Expr ===
  void visit(IntegerLiteral &e) override { os << e.literal; }
  void visit(FloatingLiteral &e) override { os << e.literal; }
  void visit(CharLiteral &e) override { os << e.literal; }
  void visit(StringLiteral &e) override { os << '"' << e.literal << '"'; }
  void visit(IdentifierExpr &e) override { os << e.literal; }

  void visit(UnaryExpression &e) override {
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

  void visit(BinaryExpression &e) override {
    if(e.left) e.left->accept(*this);
    os << " " << binOpToString(e.op) << " ";
    if(e.right) e.right->accept(*this);
  }

  // void visit(ConditionalOp &e) override {
  //   if(e.cond) e.cond->accept(*this);
  //   os << " ? "; if(e.thenExpr) e.thenExpr->accept(*this);
  //   os << " : "; if(e.elseExpr) e.elseExpr->accept(*this);
  // }

  void visit(NullStmt&) override {
    os << ";\n";
  }

  void visit(ExprStmt &s) override {
    if(s.expr) s.expr->accept(*this);
    os << ";\n";
  }

  void visit(IfStmt &s) override {
    os << "if (";
    if(s.condition) s.condition->accept(*this);
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

  void visit(SwitchStmt &s) override {
    os << "switch (";
    if(s.condition) s.condition->accept(*this);
    os << ") ";
    if(s.body) {
      s.body->accept(*this);
    } else {
      os << "{}";
    }
  }

  // void visit(VarDecl &d) override {
  //   printStorage(d.specs.storage);
  //   if(d.specs.isInline) os << "inline ";
  //   if(d.specs.isNoreturn) os << "_Noreturn ";
  //   if(d.type) d.type->accept(*this);
  //   if(!d.name.empty()) os << " " << d.name;
  //   if(d.init){ os << " = "; d.init->accept(*this);} os << ";\n";
  // }

  // void visit(ParamDecl &d) override {
  //   if(d.type) d.type->accept(*this);
  //   if(!d.name.empty()) os << " " << d.name;
  //   if(d.isVariadic) os << " ...";
  // }

  // void visit(TypedefDecl &d) override {
  //   os << "typedef "; if(d.underlying) d.underlying->accept(*this); os << " " << d.name << ";\n";
  // }

  // void visit(FieldDecl &d) override {
  //   if(d.type) d.type->accept(*this);
  //   if(!d.name.empty()) os << " " << d.name;
  //   if(d.bitWidth && *d.bitWidth){ os << " : "; (*d.bitWidth)->get()->accept(*this);} os << ";\n";
  // }

  // void visit(EnumDecl &d) override {
  //   os << "enum "; if(!d.name.empty()) os << d.name;
  //   if(!d.enumerators.empty()){
  //     os << " {";
  //     for(size_t i=0;i<d.enumerators.size();++i){ auto &c = d.enumerators[i]; os << (i?", ":" ") << c.name; if(c.value && *c.value){ os << " = "; (*c.value)->get()->accept(*this);} }
  //     os << " }";
  //   }
  //   os << ";\n";
  // }

  // void TranslationUnit::accept(ASTWalker &v){ v.visit(*this); }
  void visit(TranslationUnit &tu) override {
    for(auto &decl : tu.declarations){
      if(decl) decl->accept(*this);
    }
  }

};

void print(ASTNode &n, std::ostream &os){
  Printer p{os};
  n.accept(p);
}