%skeleton "lalr1.cc"
%require "3.8"

%define api.namespace { cparser }
%define api.parser.class { Parser }
%define api.value.type variant
%define parse.error verbose
%locations

%parse-param { yyscan_t scanner } { cparser::Driver& driver }
%lex-param   { yyscan_t scanner } { cparser::Driver& driver }

%defines
%locations
%define parse.trace

%code requires {
  #include <string>
  #include <memory>
  #include <vector>
  #include <string>

  namespace cAST { struct TranslationUnit; }

  #ifndef YY_TYPEDEF_YY_SCANNER_T
  #define YY_TYPEDEF_YY_SCANNER_T
  typedef void* yyscan_t;
  #endif

  namespace cparser { class Driver; }
}

%code {
  #include "driver.hpp"
  #include "ast.h"
  extern int yylex(cparser::Parser::semantic_type*,
                   cparser::Parser::location_type*,
                   yyscan_t scanner,
                   cparser::Driver& driver);
}

/* %union {
  std::string* str;

  std::unique_ptr<cAST::Expr> expr;
  std::unique_ptr<cAST::Stmt> stmt;
  std::unique_ptr<cAST::Decl> decl;
  std::unique_ptr<cAST::Type> type;
  std::unique_ptr<cAST::TranslationUnit> tu;

  enum class UNARY_OPERATOR;
  UNARY_OPERATOR unary_operator;

  enum class TYPE_QUALIFIER;
  TYPE_QUALIFIER type_qualifier;
} */

/* %code requires {
  namespace cparser {
    struct Driver;
    class Parser;
  }

  #ifndef YY_TYPEDEF_YY_SCANNER_T
  #define YY_TYPEDEF_YY_SCANNER_T
  typedef void* yyscan_t;

  #define YY_NULLPTR 0
  struct yyguts_t;

  // int yylex(cparser::Parser::semantic_type*,
  //           cparser::Parser::location_type*,
  //           yyscan_t scanner,
  //           cparser::Driver& driver);
} */

/* %code provides {
  int yylex(cparser::Parser::semantic_type*,
            cparser::Parser::location_type*,
            yyscan_t scanner,
            cparser::Driver& driver);
} */

%token	IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL FUNC_NAME SIZEOF
%token	PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token	AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token	SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token	XOR_ASSIGN OR_ASSIGN
%token	TYPEDEF_NAME ENUMERATION_CONSTANT

%token	TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token	CONST RESTRICT VOLATILE
%token	BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token	COMPLEX IMAGINARY
%token	STRUCT UNION ENUM ELLIPSIS

%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token	ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <std::unique_ptr<cAST::Stmt>> expression_statement selection_statement iteration_statement jump_statement statement

%type <std::unique_ptr<cAST::Expr>> primary_expression equality_expression relational_expression exclusive_or_expression
%type <std::unique_ptr<cAST::Expr>> constant enumeration_constant string and_expression shift_expression
%type <std::unique_ptr<cAST::Expr>> unary_expression multiplicative_expression additive_expression inclusive_or_expression
%type <std::unique_ptr<cAST::Expr>> expression conditional_expression assignment_expression postfix_expression cast_expression
%type <std::unique_ptr<cAST::Expr>> IDENTIFIER I_CONSTANT F_CONSTANT STRING_LITERAL ENUMERATION_CONSTANT logical_and_expression logical_or_expression

%type <cAST::UNARY_OPERATOR> unary_operator
%type <cAST::TYPE_QUALIFIER> type_qualifier

%type <std::unique_ptr<cAST::Decl>> external_declaration declaration
/* %type <std::vector<std::unique_ptr<Decl>>> declaration_list */
%type <std::unique_ptr<cAST::TranslationUnit>> translation_unit

%type <std::unique_ptr<cAST::Type>> declaration_specifiers type_specifier

%type <std::unique_ptr<cAST::TYPE_STORAGE_QUALIFIER>> storage_class_specifier

%start translation_unit

/* translation_unit
  :  { $$ = driver.make_empty_tu(@$); }
  | translation_unit stmt     { $$ = driver.append_stmt(std::move($1), driver.take_stmt()); }
  ;

stmt
  : expr SEMI                 { driver.finish_stmt_from_expr($1); }
  ;

expr
  : INTEGER                   { $$ = driver.make_int($1, @1); }
  | IDENTIFIER                { $$ = driver.make_ident($1, @1); }
  | expr PLUS expr            { $$ = driver.make_binop("+", std::move($1), std::move($3), @1); }
  | expr MINUS expr           { $$ = driver.make_binop("-", std::move($1), std::move($3), @1); }
  | expr STAR expr            { $$ = driver.make_binop("*", std::move($1), std::move($3), @1); }
  | expr SLASH expr           { $$ = driver.make_binop("/", std::move($1), std::move($3), @1); }
  | LPAREN expr RPAREN        { $$ = std::move($2); @$ = @2; }
  ; */

%%

primary_expression
	: IDENTIFIER {
      auto expr = std::make_unique<cAST::IdentifierExpr>( $1 );
      // @1 -> location of 1
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| constant {
      $$ = std::move($1);
    }
	| string {
      $$ = std::move($1);
    }
	| '(' expression ')' {
      $$ = std::move($2);
    }
	/* | generic_selection {
      $$ = std::move($1);
    } */
	;

constant
	: I_CONSTANT {
      auto expr = std::make_unique<cAST::IntegerConstantExpr>( $1 );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| F_CONSTANT {
      auto expr = std::make_unique<cAST::FloatConstantExpr>( $1 );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| ENUMERATION_CONSTANT {
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move($1);
    }
	;

enumeration_constant		/* before it has been defined as such */
	: IDENTIFIER {
      auto expr = std::make_unique<cAST::EnumerationConstantExpr>( $1 );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	;

string
	: STRING_LITERAL {
      auto expr = std::make_unique<cAST::StringLiteralExpr>( $1 );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| FUNC_NAME {
      auto expr = std::make_unique<cAST::FuncNameExpr>();
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	;

/* generic_selection
	: GENERIC '(' assignment_expression ',' generic_assoc_list ')'
	; */

generic_assoc_list
	: generic_association
	| generic_assoc_list ',' generic_association
	;

generic_association
	: type_name ':' assignment_expression
	| DEFAULT ':' assignment_expression
	;

postfix_expression
	: primary_expression
	| postfix_expression '[' expression ']'
	| postfix_expression '(' ')'
	| postfix_expression '(' argument_expression_list ')'
	| postfix_expression '.' IDENTIFIER
	| postfix_expression PTR_OP IDENTIFIER
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	/* | '(' type_name ')' '{' initializer_list '}'
	| '(' type_name ')' '{' initializer_list ',' '}' */
	;

argument_expression_list
	: assignment_expression
	| argument_expression_list ',' assignment_expression
	;

unary_expression
	: postfix_expression {
      $$ = std::move($1);
    }
	| INC_OP unary_expression {
      auto expr = std::make_unique<cAST::UnaryExpr>( cAST::UnaryExpr::PreInc, std::move($2) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| DEC_OP unary_expression {
      auto expr = std::make_unique<cAST::UnaryExpr>( cAST::UnaryExpr::PreDec, std::move($2) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| unary_operator cast_expression {
      auto expr = std::make_unique<cAST::UnaryExpr>( $1, std::move($2) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| SIZEOF unary_expression {
      auto expr = std::make_unique<cAST::SizeOfExpr>( std::move($2) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	/* | SIZEOF '(' type_name ')' {
      auto expr = std::make_unique<cAST::SizeOfTypeExpr>( std::move($3) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    }
	| ALIGNOF '(' type_name ')' {
      auto expr = std::make_unique<cAST::AlignOfTypeExpr>( std::move($3) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    } */
	;

unary_operator
	: '&' {
      $$ = cAST::UNARY_OPERATOR::ADDRESS_OF;
    }
	| '*' {
      $$ = cAST::UNARY_OPERATOR::DEREFERENCE;
    }
	| '+' {
      $$ = cAST::UNARY_OPERATOR::PLUS;
    }
	| '-' {
      $$ = cAST::UNARY_OPERATOR::MINUS;
    }
	| '~' {
      $$ = cAST::UNARY_OPERATOR::BITWISE_NOT;
    }
	| '!' {
      $$ = cAST::UNARY_OPERATOR::LOGICAL_NOT;
    }
	;

cast_expression
	: unary_expression {
      $$ = std::move($1);
    }
	/* | '(' type_name ')' cast_expression {
      auto expr = std::make_unique<CastExpr>( std::move($2), std::move($4) );
      Driver::setLoc(expr, @1, driver.file);
      $$ = std::move(expr);
    } */
	;

multiplicative_expression
	: cast_expression {
      $$ = std::move($1);
    }
	| multiplicative_expression '*' cast_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Mul, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| multiplicative_expression '/' cast_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Div, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| multiplicative_expression '%' cast_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Mod, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

additive_expression
	: multiplicative_expression {
      $$ = std::move($1);
    }
	| additive_expression '+' multiplicative_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Add, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| additive_expression '-' multiplicative_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Sub, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

shift_expression
	: additive_expression {
      $$ = std::move($1);
    }
	| shift_expression LEFT_OP additive_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::LeftShift, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| shift_expression RIGHT_OP additive_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::RightShift, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

relational_expression
	: shift_expression {
      $$ = std::move($1);
    }
	| relational_expression '<' shift_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Lt, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| relational_expression '>' shift_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Gt, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| relational_expression LE_OP shift_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Le, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| relational_expression GE_OP shift_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Ge, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

equality_expression
	: relational_expression {
      $$ = std::move($1);
    }
	| equality_expression EQ_OP relational_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Eq, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	| equality_expression NE_OP relational_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Ne, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

and_expression
	: equality_expression {
      $$ = std::move($1);
    }
	| and_expression '&' equality_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::And, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

exclusive_or_expression
	: and_expression {
      $$ = std::move($1);
    }
	| exclusive_or_expression '^' and_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Xor, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

inclusive_or_expression
	: exclusive_or_expression {
      $$ = std::move($1);
    }
	| inclusive_or_expression '|' exclusive_or_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Or, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

logical_and_expression
	: inclusive_or_expression {
      $$ = std::move($1);
    }
	| logical_and_expression AND_OP inclusive_or_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::LogicalAnd, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

logical_or_expression
	: logical_and_expression {
      $$ = std::move($1);
    }
	| logical_or_expression OR_OP logical_and_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::LogicalOr, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

conditional_expression
	: logical_or_expression {
      $$ = std::move($1);
    }
	| logical_or_expression '?' expression ':' conditional_expression {
      auto expr =  std::make_unique<cAST::ConditionalExpr>( std::move($1), std::move($3), std::move($5) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

assignment_expression
	: conditional_expression {
      $$ = std::move($1);
    }
	| unary_expression assignment_operator assignment_expression {
      auto expr = std::make_unique<cAST::BinaryExpr>( cAST::BinaryExpr::Assign, std::move($1), std::move($3) );
      Driver::setLoc(expr, @2, driver.file);
      $$ = std::move(expr);
    }
	;

assignment_operator
	: '='
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;

expression
	: assignment_expression
	| expression ',' assignment_expression
	;

constant_expression
	: conditional_expression	/* with constraints */
	;

declaration
	: declaration_specifiers ';'
	| declaration_specifiers init_declarator_list ';'
	| static_assert_declaration
	;

declaration_specifiers
	: storage_class_specifier declaration_specifiers
	| storage_class_specifier
	| type_specifier declaration_specifiers
	| type_specifier
	| type_qualifier declaration_specifiers
	| type_qualifier
	| function_specifier declaration_specifiers
	| function_specifier
	| alignment_specifier declaration_specifiers
	| alignment_specifier
	;

init_declarator_list
	: init_declarator
	| init_declarator_list ',' init_declarator
	;

init_declarator
	: declarator '=' initializer
	| declarator
	;

storage_class_specifier
	: TYPEDEF	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Typedef;
    }
	| EXTERN	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Extern;
    }
	| STATIC	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Static;
    }
	| THREAD_LOCAL	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Thread_Local;
    }
	| AUTO	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Auto;
    }
	| REGISTER	{
      $$ = cAST::TYPE_STORAGE_QUALIFIER::Register;
    }
	;

type_specifier
	: VOID {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Void );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| CHAR {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Char );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| SHORT {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Short );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| INT {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Int );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| LONG {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Long );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| FLOAT {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Float );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| DOUBLE {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Double );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| SIGNED {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Signed );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| UNSIGNED {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Unsigned );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| BOOL {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Bool );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| COMPLEX {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Complex );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| IMAGINARY {
      auto type = std::make_unique<cAST::BuiltinType>( cAST::BUILTIN_TYPE::Imaginary );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }	/* non-mandated extension */
	/* | atomic_type_specifier {
      $$ = std::move($1);
    } */
	/* | struct_or_union_specifier {
      auto type = std::make_unique<StructOrUnionType>();
      type->structOrUnionSpec = std::move($1);
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| enum_specifier {
      auto type = std::make_unique<EnumType>();
      type->enumSpec = std::move($1);
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    }
	| TYPEDEF_NAME {
      auto type = std::make_unique<NamedType>( $1 );
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    } */
	;

struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}'
	| struct_or_union IDENTIFIER
	;

struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'	/* for anonymous struct/union */
	| specifier_qualifier_list struct_declarator_list ';'
	| static_assert_declaration
	;

specifier_qualifier_list
	: type_specifier specifier_qualifier_list
	| type_specifier
	| type_qualifier specifier_qualifier_list
	| type_qualifier
	;

struct_declarator_list
	: struct_declarator
	| struct_declarator_list ',' struct_declarator
	;

struct_declarator
	: ':' constant_expression
	| declarator ':' constant_expression
	| declarator
	;

enum_specifier
	: ENUM '{' enumerator_list '}'
	| ENUM '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER '{' enumerator_list '}'
	| ENUM IDENTIFIER '{' enumerator_list ',' '}'
	| ENUM IDENTIFIER
	;

enumerator_list
	: enumerator
	| enumerator_list ',' enumerator
	;

/* identifiers must be flagged as ENUMERATION_CONSTANT */
enumerator
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;

atomic_type_specifier
	: ATOMIC '(' type_name ')'
	;

type_qualifier
	: CONST {
      $$ = cAST::TYPE_QUALIFIER::CONST;
    }
	| RESTRICT {
      $$ = cAST::TYPE_QUALIFIER::RESTRICT;
    }
	| VOLATILE {
      $$ = cAST::TYPE_QUALIFIER::VOLATILE;
    }
	| ATOMIC {
      $$ = cAST::TYPE_QUALIFIER::ATOMIC;
    }
	;

function_specifier
	: INLINE
	| NORETURN
	;

alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	;

declarator
	: pointer direct_declarator
	| direct_declarator
	;

direct_declarator
	: IDENTIFIER
	| '(' declarator ')'
	| direct_declarator '[' ']'
	| direct_declarator '[' '*' ']'
	| direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_declarator '[' STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list '*' ']'
	| direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_declarator '[' type_qualifier_list ']'
	| direct_declarator '[' assignment_expression ']'
	| direct_declarator '(' parameter_type_list ')'
	| direct_declarator '(' ')'
	| direct_declarator '(' identifier_list ')'
	;

pointer
	: '*' type_qualifier_list pointer
	| '*' type_qualifier_list
	| '*' pointer
	| '*'
	;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;


parameter_type_list
	: parameter_list ',' ELLIPSIS
	| parameter_list
	;

parameter_list
	: parameter_declaration
	| parameter_list ',' parameter_declaration
	;

parameter_declaration
	: declaration_specifiers declarator
	| declaration_specifiers abstract_declarator
	| declaration_specifiers
	;

identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
	;

type_name
	: specifier_qualifier_list abstract_declarator
    /* {
      auto type = std::make_unique<NamedType>();
      type->baseType = std::move($1);
      type->abstractDecl = std::move($2);
      Driver::setLoc(type, @1, driver.file);
      $$ = std::move(type);
    } */
	| specifier_qualifier_list
    /* {
      $$ = std::move($1);
    } */
	;

abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	;

direct_abstract_declarator
	: '(' abstract_declarator ')'
	| '[' ']'
	| '[' '*' ']'
	| '[' STATIC type_qualifier_list assignment_expression ']'
	| '[' STATIC assignment_expression ']'
	| '[' type_qualifier_list STATIC assignment_expression ']'
	| '[' type_qualifier_list assignment_expression ']'
	| '[' type_qualifier_list ']'
	| '[' assignment_expression ']'
	| direct_abstract_declarator '[' ']'
	| direct_abstract_declarator '[' '*' ']'
	| direct_abstract_declarator '[' STATIC type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list STATIC assignment_expression ']'
	| direct_abstract_declarator '[' type_qualifier_list ']'
	| direct_abstract_declarator '[' assignment_expression ']'
	| '(' ')'
	| '(' parameter_type_list ')'
	| direct_abstract_declarator '(' ')'
	| direct_abstract_declarator '(' parameter_type_list ')'
	;

initializer
	: '{' initializer_list '}'
	| '{' initializer_list ',' '}'
	| assignment_expression
	;

initializer_list
	: designation initializer
	| initializer
	| initializer_list ',' designation initializer
	| initializer_list ',' initializer
	;

designation
	: designator_list '='
	;

designator_list
	: designator
	| designator_list designator
	;

designator
	: '[' constant_expression ']'
	| '.' IDENTIFIER
	;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;

statement
	: labeled_statement
	| compound_statement
	| expression_statement
	| selection_statement
	| iteration_statement
	| jump_statement
	;

labeled_statement
	: IDENTIFIER ':' statement
	| CASE constant_expression ':' statement
	| DEFAULT ':' statement
	;

compound_statement
	: '{' '}'
	| '{'  block_item_list '}'
	;

block_item_list
	: block_item
	| block_item_list block_item
	;

block_item
	: declaration
	| statement
	;

expression_statement
	: ';' {
      auto nullStmt = std::make_unique<cAST::NullStmt>();
      Driver::setLoc(nullStmt, @1, driver.file);
      $$ = std::move(nullStmt);
    }
	| expression ';' {
      auto exprStmt = std::make_unique<cAST::ExprStmt>();
      exprStmt->expr = std::move($1);
      Driver::setLoc(exprStmt, @2, driver.file);
      $$ = std::move(exprStmt);
    }
	;

selection_statement
	: IF '(' expression ')' statement ELSE statement {
      auto stmt = std::make_unique<cAST::IfStmt>();
      stmt->condition = std::move($3);
      stmt->thenBranch = std::move($5);
      stmt->elseBranch = std::move($7);
      $$ = std::move(stmt);
  }
	| IF '(' expression ')' statement {
      auto stmt = std::make_unique<cAST::IfStmt>();
      stmt->condition =  std::move($3);
      stmt->thenBranch = std::move($5);
      $$ = std::move(stmt);
  }
	| SWITCH '(' expression ')' statement {
      auto stmt = std::make_unique<cAST::SwitchStmt>();
      stmt->condition = std::move($3);
      stmt->body = std::move($5);
      $$ = std::move(stmt);
  }
	;

iteration_statement
	: WHILE '(' expression ')' statement
	| DO statement WHILE '(' expression ')' ';'
	| FOR '(' expression_statement expression_statement ')' statement
	| FOR '(' expression_statement expression_statement expression ')' statement
	| FOR '(' declaration expression_statement ')' statement
	| FOR '(' declaration expression_statement expression ')' statement
	;

jump_statement
	: GOTO IDENTIFIER ';'
	| CONTINUE ';'
	| BREAK ';'
	| RETURN ';'
	| RETURN expression ';'
	;

translation_unit
	: external_declaration {
      $$ = std::make_unique<cAST::TranslationUnit>();
      $$->addDecl( std::move($1) );
    }
	| translation_unit external_declaration {
      $$ = std::move($1);
      $$->addDecl( std::move($2) );
    }
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: declaration_specifiers declarator declaration_list compound_statement
	| declaration_specifiers declarator compound_statement
	;

declaration_list
	: declaration
	| declaration_list declaration
	;

%%

void yyerror(const char *s)
{
	fflush(stdout);
	fprintf(stderr, "*** %s\n", s);
}

void Parser::error(const location_type &loc, const std::string &msg) {
  std::cerr << "Parse error at " << loc.begin.line << ":" << loc.begin.column << ": " << msg << "\n";
}

/* void cparser::Parser::error(const location_type& loc, const std::string& msg) {
  driver.report_error(loc, msg);
} */
