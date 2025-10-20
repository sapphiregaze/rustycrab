%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define parse.error verbose
%define api.namespace {cAST}
%define api.parser.class {cASTParser}
%define api.value.type variant
%define parse.assert

%code requires {
  #include "ast.hpp"

  namespace cAST {
    class Driver;
    class cASTScanner;
  }

  # ifndef YY_NULLPTR
  #  if defined __cplusplus && 201103L <= __cplusplus
  #   define YY_NULLPTR nullptr
  #  else
  #   define YY_NULLPTR 0
  #  endif
  # endif
}

%parse-param { cASTScanner &scanner }
%parse-param { Driver &driver }

%code{
  #include <string>
  #include <iostream>
  #include <cstdlib>
  #include <fstream>
  /* include for all driver functions */
  #include "driver.hpp"

  #undef yylex
  #define yylex scanner.yylex
}

%token END 0 "end of file"

%token <int> I_CONSTANT
%token <double> F_CONSTANT
%token <std::string> STRING_LITERAL IDENTIFIER FUNC_NAME ENUMERATION_CONSTANT

%token SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN
%token TYPEDEF_NAME

%token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token CONST RESTRICT VOLATILE
%token BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token COMPLEX IMAGINARY
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <std::unique_ptr<cAST::Expr>>
  primary_expression constant string initializer
  postfix_expression unary_expression cast_expression
  multiplicative_expression additive_expression shift_expression
  relational_expression equality_expression and_expression
  exclusive_or_expression inclusive_or_expression
  logical_and_expression logical_or_expression
  conditional_expression assignment_expression expression

%type <std::vector<std::unique_ptr<cAST::Expr>>> argument_expression_list initializer_list

%type <cAST::UNARY_OPERATOR> unary_operator
%type <cAST::AssignOp> assignment_operator
%type <cAST::TYPE_STORAGE_QUALIFIER> storage_class_specifier
%type <cAST::TYPE_QUALIFIER> type_qualifier
%type <cAST::BUILTIN_TYPE> type_specifier

%type <std::unique_ptr<cAST::Stmt>>
  statement compound_statement
  block_item expression_statement selection_statement
  iteration_statement jump_statement

%type <std::vector<std::unique_ptr<cAST::Stmt>>> block_item_list

%type <std::unique_ptr<cAST::Decl>> declaration external_declaration function_definition

%type <cAST::DeclSpecs> declaration_specifiers specifier_qualifier_list

%type <cAST::TypeNode*> type_name

%type <cAST::Decl*> declarator direct_declarator pointer init_declarator

%type <std::vector<cAST::Decl*>> init_declarator_list;

%type <std::vector<std::unique_ptr<cAST::ParamDecl>>> parameter_list parameter_type_list
%type <std::unique_ptr<cAST::ParamDecl>> parameter_declaration

%start translation_unit

%locations
%%
primary_expression
  : IDENTIFIER { $$ = driver.makeIdentifierExpr(*$1); }
  | constant { $$ = std::move($1); }
  | string { $$ = std::move($1); }
  | '(' expression ')' { $$ = std::move($2); }
  ;

constant
  : I_CONSTANT { $$ = driver.makeConstantIntExpr(*$1); }
  | F_CONSTANT { $$ = driver.makeConstantFloatExpr(*$1); }
  ;

string
  : STRING_LITERAL { $$ = driver.makeStringLiteral(*$1); }
  ;

postfix_expression
  : primary_expression { $$ = std::move($1); }
  | postfix_expression '[' expression ']' { $$ = driver.makeSubscript(std::move($1), std::move($3)); }
  | postfix_expression '(' ')' { $$ = driver.makeCall(std::move($1), {}); }
  | postfix_expression '(' argument_expression_list ')' { $$ = driver.makeCall(std::move($1), std::move($3)); }
  | postfix_expression '.' IDENTIFIER { $$ = driver.makeMember(std::move($1), *$3, false); }
  | postfix_expression PTR_OP IDENTIFIER { $$ = driver.makeMember(std::move($1), *$3, true); }
  | postfix_expression INC_OP { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::POST_INC, std::move($1)); }
  | postfix_expression DEC_OP { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::POST_DEC, std::move($1)); }
  ;

argument_expression_list
  : assignment_expression { $$ = driver.singleton(std::move($1)); }
  | argument_expression_list ',' assignment_expression { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

unary_expression
  : postfix_expression { $$ = std::move($1); }
  | INC_OP unary_expression { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::PRE_INC, std::move($2)); }
  | DEC_OP unary_expression { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::PRE_DEC, std::move($2)); }
  | unary_operator cast_expression { $$ = driver.makeUnary($1, $2); }
  /* | SIZEOF unary_expression                      { make it a builtin unary if you model it$$ = driver.makeUnary(AST::UnaryOp::SizeofExpr custom, std::move($2), @1); } */
  /* | SIZEOF '(' type_name ')'                     { $$ = driver.makeCast(sizeof-type as expr std::move($3), nullptr, @1); or a dedicated node } */
  /* | ALIGNOF '(' type_name ')'                    { $$ = driver.makeCast(alignof-type expr std::move($3), nullptr, @1); } */
  ;

unary_operator
  : '&' { $$ = cAST::UNARY_OPERATOR::ADDRESS_OF; }
  | '*' { $$ = cAST::UNARY_OPERATOR::DEREFERENCE; }
  | '+' { $$ = cAST::UNARY_OPERATOR::PLUS; }
  | '-' { $$ = cAST::UNARY_OPERATOR::MINUS; }
  | '~' { $$ = cAST::UNARY_OPERATOR::BITWISE_NOT; }
  | '!' { $$ = cAST::UNARY_OPERATOR::LOGICAL_NOT; }
  ;

cast_expression
  : unary_expression { $$ = $1; }
  | '(' type_name ')' cast_expression { $$ = driver.makeCast(std::move($2), std::move($4)); }
  ;

multiplicative_expression
  : cast_expression { $$ = std::move($1); }
  | multiplicative_expression '*' cast_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::MULTIPLY, std::move($1), std::move($3)); }
  | multiplicative_expression '/' cast_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::DIVIDE, std::move($1), std::move($3)); }
  | multiplicative_expression '%' cast_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::MODULO, std::move($1), std::move($3)); }
  ;

additive_expression
  : multiplicative_expression { $$ = std::move($1); }
  | additive_expression '+' multiplicative_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::ADD, std::move($1), std::move($3)); }
  | additive_expression '-' multiplicative_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::SUBTRACT, std::move($1), std::move($3)); }
  ;

shift_expression
  : additive_expression { $$ = std::move($1); }
  | shift_expression LEFT_OP additive_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LEFT_SHIFT, std::move($1), std::move($3)); }
  | shift_expression RIGHT_OP additive_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::RIGHT_SHIFT, std::move($1), std::move($3)); }
  ;

relational_expression
  : shift_expression { $$ = std::move($1); }
  | relational_expression '<' shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LESS_THAN, std::move($1), std::move($3)); }
  | relational_expression '>' shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::GREATER_THAN, std::move($1), std::move($3)); }
  | relational_expression LE_OP shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LESS_EQUAL, std::move($1), std::move($3)); }
  | relational_expression GE_OP shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::GREATER_EQUAL, std::move($1), std::move($3)); }
  ;

equality_expression
  : relational_expression { $$ = std::move($1); }
  | equality_expression EQ_OP relational_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::EQUAL, std::move($1), std::move($3)); }
  | equality_expression NE_OP relational_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::NOT_EQUAL, std::move($1), std::move($3)); }
  ;

and_expression
  : equality_expression { $$ = std::move($1); }
  | and_expression '&' equality_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_AND, std::move($1), std::move($3)); }
  ;

exclusive_or_expression
  : and_expression { $$ = std::move($1); }
  | exclusive_or_expression '^' and_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_XOR, std::move($1), std::move($3)); }
  ;

inclusive_or_expression
  : exclusive_or_expression { $$ = std::move($1); }
  | inclusive_or_expression '|' exclusive_or_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_OR, std::move($1), std::move($3)); }
  ;

logical_and_expression
  : inclusive_or_expression { $$ = std::move($1); }
  | logical_and_expression AND_OP inclusive_or_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LOGICAL_AND, std::move($1), std::move($3)); }
  ;

logical_or_expression
  : logical_and_expression { $$ = std::move($1); }
  | logical_or_expression OR_OP logical_and_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LOGICAL_OR, std::move($1), std::move($3)); }
  ;

conditional_expression
  : logical_or_expression { $$ = std::move($1); }
  | logical_or_expression '?' expression ':' conditional_expression { $$ = driver.makeCond(std::move($1), std::move($3), std::move($5)); }
  ;

assignment_expression
  : conditional_expression { $$ = std::move($1); }
  | unary_expression assignment_operator assignment_expression { $$ = driver.makeAssign($2, std::move($1), std::move($3)); }
  ;

assignment_operator
  : '=' { $$ = cAST::AssignOp::Assign; }
  | MUL_ASSIGN { $$ = cAST::AssignOp::Mul; }
  | DIV_ASSIGN { $$ = cAST::AssignOp::Div; }
  | MOD_ASSIGN { $$ = cAST::AssignOp::Mod; }
  | ADD_ASSIGN { $$ = cAST::AssignOp::Add; }
  | SUB_ASSIGN { $$ = cAST::AssignOp::Sub; }
  | LEFT_ASSIGN { $$ = cAST::AssignOp::Shl; }
  | RIGHT_ASSIGN { $$ = cAST::AssignOp::Shr; }
  | AND_ASSIGN { $$ = cAST::AssignOp::And; }
  | XOR_ASSIGN { $$ = cAST::AssignOp::Xor; }
  | OR_ASSIGN { $$ = cAST::AssignOp::Or; }
  ;

expression
  : assignment_expression { $$ = std::move($1); }
  ;

declaration
  : declaration_specifiers ';' {
    // driver.makeDeclStmt(driver.makeDeclFromSpecs($1));
    driver.makeDeclFromSpecs($1);
  }
  | declaration_specifiers init_declarator_list ';' {
    // driver.makeInitDeclList(std::move($1));
    driver.makeDeclListFromSpecsAndInits($1, std::move($2));
  }
  ;

declaration_specifiers
  : type_specifier {
    $$ = driver.makeSpecsFromBuiltinType($1);
  }
  | type_qualifier {
    $$ = driver.makeSpecsFromTypeQual($1);
  }
  | storage_class_specifier {
    $$ = driver.makeSpecsFromStorageClass($1);
  }
  /* | declaration_specifiers type_specifier */
  /* | declaration_specifiers type_qualifier */
  /* | declaration_specifiers storage_class_specifier */
  ;

storage_class_specifier
  : TYPEDEF { $$ = cAST::TYPE_STORAGE_QUALIFIER::Typedef; }
  | EXTERN  { $$ = cAST::TYPE_STORAGE_QUALIFIER::Extern; }
  | STATIC  { $$ = cAST::TYPE_STORAGE_QUALIFIER::Static; }
  | THREAD_LOCAL { $$ = cAST::TYPE_STORAGE_QUALIFIER::Thread_Local; }
  | AUTO  { $$ = cAST::TYPE_STORAGE_QUALIFIER::Auto; }
  | REGISTER { $$ = cAST::TYPE_STORAGE_QUALIFIER::Register; }
  ;

type_specifier
  : VOID { $$ = cAST::BUILTIN_TYPE::Void; }
  | CHAR { $$ = cAST::BUILTIN_TYPE::Char; }
  | SHORT { $$ = cAST::BUILTIN_TYPE::Short; }
  | INT { $$ = cAST::BUILTIN_TYPE::Int; }
  | LONG { $$ = cAST::BUILTIN_TYPE::Long; }
  | FLOAT { $$ = cAST::BUILTIN_TYPE::Float; }
  | DOUBLE { $$ = cAST::BUILTIN_TYPE::Double; }
  | SIGNED { $$ = cAST::BUILTIN_TYPE::Signed; }
  | UNSIGNED { $$ = cAST::BUILTIN_TYPE::Unsigned; }
  | BOOL { $$ = cAST::BUILTIN_TYPE::Bool; }
  | COMPLEX { $$ = cAST::BUILTIN_TYPE::Complex; }
  | IMAGINARY { $$ = cAST::BUILTIN_TYPE::Imaginary; }
  ;

type_qualifier
  : CONST { $$ = cAST::TYPE_QUALIFIER::Const; }
  | RESTRICT { $$ = cAST::TYPE_QUALIFIER::Restrict; }
  | VOLATILE { $$ = cAST::TYPE_QUALIFIER::Volatile; }
  | ATOMIC { $$ = cAST::TYPE_QUALIFIER::Atomic; }
  ;

init_declarator_list
  : init_declarator {
    $$ = std::vector<cAST::Decl*>{ std::move($1) };
  }
  | init_declarator_list ',' init_declarator { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

init_declarator
  : declarator { $$ = std::move($1); }
  /* | declarator '=' initializer */
  ;

declarator
  : direct_declarator { $$ = std::move($1); }
  /* | pointer direct_declarator { $$ = driver.wrapPointer($2); } */
  ;

/* pointer
  : '*' { $$ = driver.wrapPointer(cAST::ParamDecl{}); }
  | '*' pointer { $$ = driver.wrapPointer($2); }
  | '*' type_qualifier pointer
  | '*' type_qualifier
  ; */

direct_declarator
  : IDENTIFIER { $$ = driver.makeIdentDeclarator($1); }
  /* | '(' declarator ')' { $$ = std::move($2); } */
  /* | direct_declarator '(' ')' { $$ = driver.wrapFunction($1, {}, false); } */
  /* | direct_declarator '(' parameter_type_list ')' { $$ = driver.wrapFunction($1, std::move($3), {}, @2); } */
  ;

parameter_type_list
  : parameter_list { $$ = std::move($1); }
  ;

parameter_list
  : parameter_declaration { $$ = std::vector{ std::move($1) }; }
  | parameter_list ',' parameter_declaration { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

parameter_declaration
  : declaration_specifiers declarator { $$ = driver.makeParam($1, $2); }
  | declaration_specifiers { $$ = driver.makeParam($1, cAST::ParamDecl{}); }
  ;

type_name
  : specifier_qualifier_list { $$ = std::move($1.type); }
  ;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list { $$ = driver.combineSpecs(driver.makeSpecsFromTypeNode(std::move($1)), $2); }
	| type_specifier { $$ = driver.makeSpecsFromTypeNode(std::move($1)); }
	| type_qualifier specifier_qualifier_list { $$ = driver.combineSpecs(driver.makeSpecsFromTypeNode(driver.makeBuiltinType()), $2); }
	| type_qualifier { $$ = driver.makeSpecsFromTypeNode(driver.makeBuiltinType()); }
	;

initializer
  : assignment_expression { $$ = std::move($1); }
  | '{' initializer_list '}' { $$ = driver.makeInitList(std::move($2), @1); }
  | '{' initializer_list ',' '}' { $$ = driver.makeInitList(std::move($2), @1); }
  ;

initializer_list
  : initializer { $$ = driver.singleton(std::move($1)); }
  | initializer_list ',' initializer { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

statement
  : compound_statement { $$ = std::move($1); }
  | expression_statement { $$ = std::move($1); }
  | selection_statement { $$ = std::move($1); }
  | iteration_statement { $$ = std::move($1); }
  | jump_statement { $$ = std::move($1); }
  ;

compound_statement
  : '{' '}' { $$ = driver.makeCompoundStmt({}); }
  | '{' block_item_list '}' { $$ = driver.makeCompoundStmt(std::move($2)); }
  ;

block_item_list
  : block_item { $$ = driver.singleton(std::move($1)); }
  | block_item_list block_item { $1.emplace_back(std::move($2)); $$ = std::move($1); }
  ;

block_item
  : declaration { $$ = std::make_unique<cAST::DeclStmt>(std::move($1));}
  | statement { $$ = std::move($1); }
  ;

expression_statement
  : ';' { $$ = driver.makeNullStmt(); }
  | expression ';' { $$ = driver.makeExprStmt(std::move($1)); }
  ;

selection_statement
  : IF '(' expression ')' statement ELSE statement { $$ = driver.makeIf(std::move($3), std::move($5), std::move($7)); }
  | IF '(' expression ')' statement { $$ = driver.makeIf(std::move($3), std::move($5), nullptr); }
  ;

iteration_statement
  : WHILE '(' expression ')' statement { $$ = driver.makeWhile(std::move($3), std::move($5)); }
  | DO statement WHILE '(' expression ')' ';' { $$ = driver.makeDoWhile(std::move($2), std::move($5)); }
  | FOR '(' expression_statement expression_statement ')' statement { $$ = driver.makeFor(std::move($3), nullptr, nullptr, std::move($6));}
  | FOR '(' expression_statement expression_statement expression ')' statement { $$ = driver.makeFor(std::move($3), nullptr, std::move($5), std::move($7)); }
  | FOR '(' declaration expression_statement ')' statement { $$ = driver.makeFor(std::make_unique<cAST::DeclStmt>(std::move($3)), nullptr, nullptr, std::move($6)); }
  | FOR '(' declaration expression_statement expression ')' statement { $$ = driver.makeFor(std::make_unique<cAST::DeclStmt>(std::move($3)), nullptr, std::move($5), std::move($7)); }
  ;

jump_statement
  : CONTINUE ';' { $$ = driver.makeContinue(); }
  | BREAK ';' { $$ = driver.makeBreak(); }
  | RETURN ';' { $$ = driver.makeReturn(nullptr); }
  | RETURN expression ';'  { $$ = driver.makeReturn(std::move($2)); }
  ;

translation_unit
	: external_declaration {
    }
	| translation_unit external_declaration {
    }
	;

external_declaration
  : declaration {
      // $$ = std::move($1);
    }
  /* | function_definition {
    std::cout << "Function definition parsed." << std::endl;
    $$ = std::move($1);
  } */
  ;

function_definition
  : declaration_specifiers declarator compound_statement
    {
      auto fn = std::make_unique<cAST::FunctionDecl>($1, $2);
      fn->set_body(std::move($3));
      $$ = std::move(fn);
    }
  ;
%%

void cAST::cASTParser::error( const location_type &l, const std::string &err_message ) {
  std::cerr << "Error: " << err_message << " at " << l << "\n";
}

/* void cAST::cASTParser::error(const location_type& l, const std::string& m) {
  driver.report_error(l.begin.line, l.begin.column, m);
} */
