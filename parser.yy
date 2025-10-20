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
  primary_expression constant string enumeration_constant generic_selection
  postfix_expression unary_expression cast_expression
  multiplicative_expression additive_expression shift_expression
  relational_expression equality_expression and_expression
  exclusive_or_expression inclusive_or_expression
  logical_and_expression logical_or_expression
  conditional_expression assignment_expression expression
  constant_expression initializer

%type <std::vector<std::unique_ptr<cAST::Expr>>> argument_expression_list
/* %type <std::vector<std::unique_ptr<AST::Expr>>> initializer_list */

%type <cAST::UNARY_OPERATOR> unary_operator
%type <cAST::AssignOp> assignment_operator
%type <cAST::TYPE_STORAGE_QUALIFIER> storage_class_specifier
%type <cAST::TYPE_QUALIFIER> type_qualifier

%type <std::unique_ptr<cAST::Stmt>>
  statement labeled_statement compound_statement
  block_item expression_statement selection_statement
  iteration_statement jump_statement

%type <std::vector<std::unique_ptr<cAST::Stmt>>> block_item_list

%type <std::unique_ptr<cAST::Decl>> declaration external_declaration function_definition
/* %type <std::unique_ptr<AST::Decl>> init_declarator */

%type <std::vector<std::unique_ptr<cAST::Decl>>> declaration_list
/* %type <std::vector<std::unique_ptr<AST::Decl>>> init_declarator_list */

%type <cAST::DeclSpecs> declaration_specifiers specifier_qualifier_list
/* %type <AST::DeclSpecs> specifier_qualifier_list */

%type <std::unique_ptr<cAST::TypeNode>> type_specifier type_name
/* %type <std::unique_ptr<AST::TypeNode>> atomic_type_specifier struct_or_union_specifier */

%type <std::unique_ptr<cAST::Decl>> declarator direct_declarator pointer
/* %type <AST::Declarator> abstract_declarator direct_abstract_declarator */

%type <std::vector<std::unique_ptr<cAST::ParamDecl>>> parameter_list parameter_type_list
%type <std::unique_ptr<cAST::ParamDecl>> parameter_declaration

%type <std::vector<std::pair<std::unique_ptr<cAST::TypeNode>, std::unique_ptr<cAST::Expr>>>> generic_assoc_list
%type <std::pair<std::unique_ptr<cAST::TypeNode>, std::unique_ptr<cAST::Expr>>> generic_association

%type <std::vector<std::string>> identifier_list

%start translation_unit

%locations
%%
primary_expression
  : IDENTIFIER { $$ = driver.makeIdentifierExpr(*$1); }
  | constant { $$ = std::move($1); }
  | string { $$ = std::move($1); }
  | '(' expression ')' { $$ = std::move($2); }
  | generic_selection { $$ = std::move($1); }
  ;

constant
  : I_CONSTANT { $$ = driver.makeConstantIntExpr(*$1); }
  | F_CONSTANT { $$ = driver.makeConstantFloatExpr(*$1); }
  | ENUMERATION_CONSTANT { $$ = driver.makeStringLiteral(*$1); }
  ;

enumeration_constant
  : IDENTIFIER { $$ = std::move($1); }
  ;

string
  : STRING_LITERAL { $$ = driver.makeStringLiteral(*$1); }
  | FUNC_NAME { $$ = driver.makeStringLiteral(*$1); }
  ;

generic_selection
  : GENERIC '(' assignment_expression ',' generic_assoc_list ')'
    {
      // generic_assoc_list packs pairs of (type, expr); default is handled as {nullptr, expr}
      std::vector<std::pair<std::unique_ptr<cAST::TypeNode>, std::unique_ptr<cAST::Expr>>> assocs;
      std::unique_ptr<cAST::Expr> def;
      for (auto &p : $5) {
        if (p.first) driver.emplace_back(std::move(p));
        else def = std::move(p.second);
      }
      $$ = driver.makeGenericSelection(std::move($3), std::move(assocs), std::move(def));
    }
  ;

generic_assoc_list
  : generic_association { $$ = std::vector{ std::move($1) }; }
  | generic_assoc_list ',' generic_association { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

generic_association
  : type_name ':' assignment_expression { $$ = { std::move($1), std::move($3) }; }
  | DEFAULT ':' assignment_expression { $$ = { nullptr, std::move($3) }; }
  ;

postfix_expression
  : primary_expression                          { $$ = std::move($1); }
  | postfix_expression '[' expression ']'       { $$ = driver.makeSubscript(std::move($1), std::move($3)); }
  | postfix_expression '(' ')'                  { $$ = driver.makeCall(std::move($1), {}); }
  | postfix_expression '(' argument_expression_list ')'
                                                { $$ = driver.makeCall(std::move($1), std::move($3)); }
  | postfix_expression '.' IDENTIFIER           { $$ = driver.makeMember(std::move($1), *$3, false); }
  | postfix_expression PTR_OP IDENTIFIER        { $$ = driver.makeMember(std::move($1), *$3, true); }
  | postfix_expression INC_OP                   { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::POST_INC, std::move($1)); }
  | postfix_expression DEC_OP                   { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::POST_DEC, std::move($1)); }
  /* | '(' type_name ')' '{' initializer_list '}'  { $$ = driver.makeCompoundLiteral(std::move($2), driver.makeInitList(std::move($5))); }
  | '(' type_name ')' '{' initializer_list ',' '}' { $$ = driver.makeCompoundLiteral(std::move($2), driver.makeInitList(std::move($5))); } */
  ;

argument_expression_list
  : assignment_expression { $$ = driver.singleton(std::move($1)); }
  | argument_expression_list ',' assignment_expression { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

unary_expression
  : postfix_expression                           { $$ = std::move($1); }
  | INC_OP unary_expression                      { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::PRE_INC, std::move($2)); }
  | DEC_OP unary_expression                      { $$ = driver.makeUnary(cAST::UNARY_OPERATOR::PRE_DEC, std::move($2)); }
  | unary_operator cast_expression               { $$ = driver.makeUnary($1, $2); }
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
  : unary_expression                              { $$ = $1; }
  | '(' type_name ')' cast_expression             { $$ = driver.makeCast(std::move($2), std::move($4)); }
  ;

multiplicative_expression
  : cast_expression                              { $$ = std::move($1); }
  | multiplicative_expression '*' cast_expression{ $$ = driver.makeBinary(cAST::BINARY_OPERATOR::MULTIPLY, std::move($1), std::move($3)); }
  | multiplicative_expression '/' cast_expression{ $$ = driver.makeBinary(cAST::BINARY_OPERATOR::DIVIDE, std::move($1), std::move($3)); }
  | multiplicative_expression '%' cast_expression{ $$ = driver.makeBinary(cAST::BINARY_OPERATOR::MODULO, std::move($1), std::move($3)); }
  ;

additive_expression
  : multiplicative_expression                    { $$ = std::move($1); }
  | additive_expression '+' multiplicative_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::ADD, std::move($1), std::move($3)); }
  | additive_expression '-' multiplicative_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::SUBTRACT, std::move($1), std::move($3)); }
  ;

shift_expression
  : additive_expression                          { $$ = std::move($1); }
  | shift_expression LEFT_OP additive_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LEFT_SHIFT, std::move($1), std::move($3)); }
  | shift_expression RIGHT_OP additive_expression{ $$ = driver.makeBinary(cAST::BINARY_OPERATOR::RIGHT_SHIFT, std::move($1), std::move($3)); }
  ;

relational_expression
  : shift_expression                             { $$ = std::move($1); }
  | relational_expression '<' shift_expression   { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LESS_THAN, std::move($1), std::move($3)); }
  | relational_expression '>' shift_expression   { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::GREATER_THAN, std::move($1), std::move($3)); }
  | relational_expression LE_OP shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LESS_EQUAL, std::move($1), std::move($3)); }
  | relational_expression GE_OP shift_expression { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::GREATER_EQUAL, std::move($1), std::move($3)); }
  ;

equality_expression
  : relational_expression                        { $$ = std::move($1); }
  | equality_expression EQ_OP relational_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::EQUAL, std::move($1), std::move($3)); }
  | equality_expression NE_OP relational_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::NOT_EQUAL, std::move($1), std::move($3)); }
  ;

and_expression
  : equality_expression                          { $$ = std::move($1); }
  | and_expression '&' equality_expression       { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_AND, std::move($1), std::move($3)); }
  ;

exclusive_or_expression
  : and_expression                               { $$ = std::move($1); }
  | exclusive_or_expression '^' and_expression   { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_XOR, std::move($1), std::move($3)); }
  ;

inclusive_or_expression
  : exclusive_or_expression                      { $$ = std::move($1); }
  | inclusive_or_expression '|' exclusive_or_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::BITWISE_OR, std::move($1), std::move($3)); }
  ;

logical_and_expression
  : inclusive_or_expression                      { $$ = std::move($1); }
  | logical_and_expression AND_OP inclusive_or_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LOGICAL_AND, std::move($1), std::move($3)); }
  ;

logical_or_expression
  : logical_and_expression                       { $$ = std::move($1); }
  | logical_or_expression OR_OP logical_and_expression
                                                { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::LOGICAL_OR, std::move($1), std::move($3)); }
  ;

conditional_expression
  : logical_or_expression                        { $$ = std::move($1); }
  | logical_or_expression '?' expression ':' conditional_expression
                                                { $$ = driver.makeCond(std::move($1), std::move($3), std::move($5)); }
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

assignment_expression
  : conditional_expression                        { $$ = std::move($1); }
  | unary_expression assignment_operator assignment_expression
                                                 { $$ = driver.makeAssign($2, std::move($1), std::move($3)); }
  ;

expression
  : assignment_expression                         { $$ = std::move($1); }
  | expression ',' assignment_expression          { $$ = driver.makeBinary(cAST::BINARY_OPERATOR::COMMA, std::move($1), std::move($3)); }
  ;

constant_expression
  : conditional_expression                        { $$ = std::move($1); }
  ;

declaration
  : declaration_specifiers ';'
      {
        std::cout << "Declaration Specifier without init declarator list: Not Implemented!" << std::endl;
        // e.g., "int ;" — not valid in ISO C, but keep node if you want
        $$ = nullptr; /* or create a dummy Decl */
      }
  /* | declaration_specifiers init_declarator_list ';'
      {
        std::cout << "Declaration Specifier with init declarator list: Not Implemented!" << std::endl; */
        // Expand list into either a DeclStmt with multiple VarDecls,
        // or if a function with body appears elsewhere (function_definition rule).
        // Here we return a single Decl that wraps many (DeclStmt) or the first, per your AST design.
        // Minimal: return the first Decl; push others via driver/builder outside.
        /* $$ = std::move($2.front()); // skeleton; adapt to your TU collector
        $2.erase($2.begin());
        for (auto &more : $2) { */
          /* driver.enqueueExternal(std::move(more)); */ /* or push into TU later */
      // }
  /* if you model it as a Decl */
  /* | static_assert_declaration       { $$ = std::move($1); } */
  ;

declaration_specifiers
  : type_specifier declaration_specifiers
      { $$ = driver.combineSpecs(driver.makeSpecsFromType(std::move($1)), $2); }
  | type_specifier
      { $$ = driver.makeSpecsFromType(std::move($1)); }
  /* | type_qualifier declaration_specifiers
      { $$ = driver.combineSpecs(driver.makeSpecsFromType(driver.makeBuiltinType()), $2); } */
  /* | type_qualifier { $$ = driver.makeSpecsFromType(driver.makeBuiltinType()); } */
  /*storage wraps*/
  /* | storage_class_specifier declaration_specifiers */
      /* { $$ = driver.combineSpecs(driver.makeSpecsFromType( nullptr), $2); } */
  /*storage*/
  /* | storage_class_specifier */
      /* { $$ = driver.makeSpecsFromType(nullptr); } */
  /* | function_specifier declaration_specifiers { $$ = driver.combineSpecs(driver.makeSpecsFromType(nullptr), $2); } */
  /* | function_specifier { $$ = driver.makeSpecsFromType(nullptr); } */
  /* | alignment_specifier declaration_specifiers */
      /*align*/
      /* { $$ = driver.combineSpecs(driver.makeSpecsFromType( nullptr), $2); } */
      /*align*/
  /* | alignment_specifier */
      /* { $$ = driver.makeSpecsFromType( nullptr); } */
  ;

/* init_declarator_list */
  /* : init_declarator                         { $$ = std::vector{ std::move($1) }; } */
  /* | init_declarator_list ',' init_declarator */
                                           /* { $1.emplace_back(std::move($3)); $$ = std::move($1); } */
  ;

/* init_declarator
  : declarator '=' initializer
      { */
        // Var or function declarator with initializer (functions can’t init here, so it’s a VarDecl)
        /*specs must be threaded from context — see typical C grammar split*/
        // $$ = driver.makeVarDecl(, $1, std::move($3));
        /* In standard C grammars, specs + list are in the same production; if you keep them split,
           capture 'current specs' in parser state or pass them down via midrule. */
      // }
  /* | declarator */
      /* { */
        // No init; could be a function proto or a variable
        // $$ = driver.makeVarDecl(, $1, nullptr, @1);
      /* } */
  /* ; */

storage_class_specifier
	: TYPEDEF	{ $$ = cAST::TYPE_STORAGE_QUALIFIER::Typedef; }
	| EXTERN	{ $$ = cAST::TYPE_STORAGE_QUALIFIER::Extern; }
	| STATIC	{ $$ = cAST::TYPE_STORAGE_QUALIFIER::Static; }
	| THREAD_LOCAL { $$ = cAST::TYPE_STORAGE_QUALIFIER::Thread_Local; }
	| AUTO  { $$ = cAST::TYPE_STORAGE_QUALIFIER::Auto; }
	| REGISTER { $$ = cAST::TYPE_STORAGE_QUALIFIER::Register; }
	;

type_specifier
	: VOID { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Void); }
	| CHAR { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Char); }
	/* | SHORT { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Short, @1.first_line); } */
	| INT { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Int); }
	| LONG { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Long); }
	| FLOAT { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Float); }
	| DOUBLE { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Double); }
	/* | SIGNED { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Signed, @1.first_line); } */
	/* | UNSIGNED { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Unsigned, @1.first_line); } */
	| BOOL { $$ = driver.makeBuiltinType(cAST::BUILTIN_TYPE::Bool); }
	/* | COMPLEX { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Complex, @1.first_line); } */
	/* | IMAGINARY { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Imaginary, @1.first_line); } */
	/* | atomic_type_specifier { $$ = std::move($1); } */
	/* | struct_or_union_specifier { $$ = std::move($1); } */
	/* | enum_specifier { $$ = std::move($1); } */
	/* | TYPEDEF_NAME { $$ = driver.makeTypeDefType(*$1, @1); } */
	;

/* struct_or_union_specifier
	: struct_or_union '{' struct_declaration_list '}' {
        // anonymous struct/union
        $$ = driver.makeStructUnion(nullptr, $1, std::move($3), @2);
      }
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}' {
        // named struct/union
        $$ = driver.makeStructUnion(std::move($2), $1, std::move($4), @2);
      }
	| struct_or_union IDENTIFIER {
        // forward declaration
        $$ = driver.makeStructUnion(std::move($2), $1, {}, @1);
      }
	;
*/

/* struct_or_union
	: STRUCT
	| UNION
	;

struct_declaration_list
	: struct_declaration
	| struct_declaration_list struct_declaration
	;

struct_declaration
	: specifier_qualifier_list ';'
	| specifier_qualifier_list struct_declarator_list ';'
	| static_assert_declaration
	;
*/

specifier_qualifier_list
	: type_specifier specifier_qualifier_list { $$ = driver.combineSpecs(driver.makeSpecsFromType(std::move($1)), $2); }
	| type_specifier { $$ = driver.makeSpecsFromType(std::move($1)); }
	| type_qualifier specifier_qualifier_list { $$ = driver.combineSpecs(driver.makeSpecsFromType(driver.makeBuiltinType()), $2); }
	| type_qualifier { $$ = driver.makeSpecsFromType(driver.makeBuiltinType()); }
	;

/*
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

enumerator
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;


atomic_type_specifier
	: ATOMIC '(' type_name ')'
	; */

type_qualifier
	: CONST { $$ = cAST::TYPE_QUALIFIER::Const; }
	| RESTRICT { $$ = cAST::TYPE_QUALIFIER::Restrict; }
	| VOLATILE { $$ = cAST::TYPE_QUALIFIER::Volatile; }
	| ATOMIC { $$ = cAST::TYPE_QUALIFIER::Atomic; }
	;

function_specifier
	: INLINE
	| NORETURN
	;

/* alignment_specifier
	: ALIGNAS '(' type_name ')'
	| ALIGNAS '(' constant_expression ')'
	; */

declarator
  : pointer direct_declarator { $$ = driver.wrapPointer($2); /* wrap chain already in $1 or combine both; adjust */ }
  | direct_declarator { $$ = std::move($1); }
  ;

direct_declarator
	: IDENTIFIER { $$ = driver.makeIdentDeclarator(*$1); }
	| '(' declarator ')' { $$ = std::move($2); }
	| direct_declarator '[' ']' { $$ = driver.wrapArray($1, nullptr); }
	/* | direct_declarator '[' '*' ']' { $$ = driver.wrapArray($1, nullptr, @2); } */
	/* | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']' { $$ = driver.wrapArray($1, std::move($4), @2); } */
	/* | direct_declarator '[' STATIC assignment_expression ']' { $$ = driver.wrapArray($1, std::move($3), @2); } */
	/* | direct_declarator '[' type_qualifier_list '*' ']' { $$ = driver.wrapArray($1, nullptr, @2); } */
	/* | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']' { $$ = driver.wrapArray($1, std::move($4), @2); } */
	| direct_declarator '[' type_qualifier_list assignment_expression ']' { $$ = driver.wrapArray($1, std::move($4)); }
  // unsized
	/* | direct_declarator '[' type_qualifier_list ']' { $$ = driver.wrapArray($1, nullptr, @2); } */
	| direct_declarator '[' assignment_expression ']' { $$ = driver.wrapArray($1, std::move($3)); }
  // variadic set in list
	/* | direct_declarator '(' parameter_type_list ')' { $$ = driver.wrapFunction($1, std::move($3), , @2); } */
	| direct_declarator '(' ')' { $$ = driver.wrapFunction($1, {}, /*variadic*/ false); }
	/* K&R parameters — map to ParamDecl with 'int' default or leave TODO */
  /* | direct_declarator '(' identifier_list ')' {  $$ = driver.wrapFunction($1, {TODO}, false, @2); } */

pointer : '*' type_qualifier_list { $$ = driver.wrapPointer(cAST::ParamDecl{}); }
  | '*' pointer { $$ = driver.wrapPointer($2); }
  | '*' { $$ = driver.wrapPointer(cAST::ParamDecl{}); }
  // fold quals into inner then wrap
  /* : '*' type_qualifier_list pointer     { $$ = $3; $$ = driver.wrapPointer($$, @1); } */
  ;

parameter_type_list : parameter_list { $$ = std::move($1); }
/* mark variadic true in wrapFunction site */
  /* : parameter_list ',' ELLIPSIS         { $$ = std::move($1);  } */
  ;

parameter_list
  : parameter_declaration { $$ = std::vector{ std::move($1) }; }
  | parameter_list ',' parameter_declaration { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

parameter_declaration
  : declaration_specifiers declarator { $$ = driver.makeParam($1, $2); }
  /* | declaration_specifiers abstract_declarator { $$ = driver.makeParam($1, $2); } */
  | declaration_specifiers { $$ = driver.makeParam($1, cAST::ParamDecl{}); }
  ;

type_name : specifier_qualifier_list { $$ = std::move($1.type); }
  /* apply abstract declarator to base type into a TypeNode */
  /* : specifier_qualifier_list abstract_declarator {$$ = driver.applyAbstract($1, $2) std::move($1.type); } */
  ;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

identifier_list
	: IDENTIFIER { $$ = std::vector{ *$1 }; }
	| identifier_list ',' IDENTIFIER { $1.emplace_back(*$3); $$ = std::move($1); }
	;

/* abstract_declarator
	: pointer direct_abstract_declarator
	| pointer
	| direct_abstract_declarator
	; */

/* direct_abstract_declarator
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
	; */

initializer: assignment_expression { $$ = std::move($1); }
  /* : '{' initializer_list '}'           { $$ = driver.makeInitList(std::move($2), @1); }
  | '{' initializer_list ',' '}'       { $$ = driver.makeInitList(std::move($2), @1); } */
  ;

/* initializer_list
	: designation initializer
	| initializer { $$ = driver.singleton(std::move($1)); } */
	/* | initializer_list ',' designation initializer
	| initializer_list ',' initializer { $1.emplace_back(std::move($3)); $$ = std::move($1); } */
	/* ; */

/* You can extend InitListExpr to carry designators if you model them */
// set a pending designator context in builder if needed
/* designation : designator_list '='
  ;
*/

designator_list
  : designator
  | designator_list designator
  ;

designator
  : '[' constant_expression ']' { /* record array designator */ }
  | '.' IDENTIFIER { /* record field designator */ }
  ;

/* static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	; */

statement
  : labeled_statement { $$ = std::move($1); }
  | compound_statement { $$ = std::move($1); }
  | expression_statement { $$ = std::move($1); }
  | selection_statement { $$ = std::move($1); }
  | iteration_statement { $$ = std::move($1); }
  | jump_statement { $$ = std::move($1); }
  ;

labeled_statement
  : IDENTIFIER ':' statement        { $$ = driver.makeLabel(*$1, std::move($3)); }
  | CASE constant_expression ':' statement
                                   { $$ = driver.makeCase(std::move($2), std::move($4)); }
  | DEFAULT ':' statement          { $$ = driver.makeDefault(std::move($3)); }
  ;

compound_statement
  : '{' '}'                        { $$ = driver.makeCompoundStmt({}); }
  | '{' block_item_list '}'        { $$ = driver.makeCompoundStmt(std::move($2)); }
  ;

block_item_list
  : block_item                     { $$ = driver.singleton(std::move($1)); }
  | block_item_list block_item     { $1.emplace_back(std::move($2)); $$ = std::move($1); }
  ;

block_item
  : declaration                    { $$ = std::make_unique<cAST::DeclStmt>(std::move($1));}
  | statement                      { $$ = std::move($1); }
  ;

expression_statement
  : ';'                            { $$ = driver.makeNullStmt(); }
  | expression ';'                 { $$ = driver.makeExprStmt(std::move($1)); }
  ;

selection_statement
  : IF '(' expression ')' statement ELSE statement
                                   { $$ = driver.makeIf(std::move($3), std::move($5), std::move($7)); }
  | IF '(' expression ')' statement
                                   { $$ = driver.makeIf(std::move($3), std::move($5), nullptr); }
  | SWITCH '(' expression ')' statement
                                   { $$ = driver.makeSwitch(std::move($3), std::move($5)); }
  ;

iteration_statement
  : WHILE '(' expression ')' statement
                                   { $$ = driver.makeWhile(std::move($3), std::move($5)); }
  | DO statement WHILE '(' expression ')' ';'
                                   { $$ = driver.makeDoWhile(std::move($2), std::move($5)); }
  | FOR '(' expression_statement expression_statement ')' statement
                                   { $$ = driver.makeFor(std::move($3), nullptr, nullptr, std::move($6));}
  | FOR '(' expression_statement expression_statement expression ')' statement
                                   { $$ = driver.makeFor(std::move($3), nullptr, std::move($5), std::move($7)); }
  | FOR '(' declaration expression_statement ')' statement
                                   { $$ = driver.makeFor(std::make_unique<cAST::DeclStmt>(std::move($3)), nullptr, nullptr, std::move($6)); }
  | FOR '(' declaration expression_statement expression ')' statement
                                   { $$ = driver.makeFor(std::make_unique<cAST::DeclStmt>(std::move($3)), nullptr, std::move($5), std::move($7)); }
  ;

jump_statement : CONTINUE ';' { $$ = driver.makeContinue(); }
  | BREAK ';' { $$ = driver.makeBreak(); }
  | RETURN ';' { $$ = driver.makeReturn(nullptr); }
  | RETURN expression ';'  { $$ = driver.makeReturn(std::move($2)); }
  /* : GOTO IDENTIFIER ';' { $$ = driver.makeGoto(*$2, @1); } */
  ;

translation_unit
	: external_declaration {
      std::cout << "Adding external declaration to translation unit." << std::endl;
      driver.push_declaration(std::move($1));
    }
	| translation_unit external_declaration {
      std::cout << "Adding external declaration to translation unit." << std::endl;
      driver.push_declaration(std::move($2));
    }
	;

external_declaration
  : function_definition {
      std::cout << "Function definition parsed." << std::endl;
      $$ = std::move($1);
    }
  | declaration {
      std::cout << "Declaration parsed." << std::endl;
      $$ = std::move($1);
    }
  ;

function_definition
  : declaration_specifiers declarator declaration_list compound_statement
    {
      // Build AST::FunctionDecl(specs, declarator, body, locals = $3)
      auto fn = std::make_unique<cAST::FunctionDecl>($1, $2);
      fn->set_body(std::move($4));
      fn->set_params(std::move($3));
      $$ = std::move(fn);
    }
  | declaration_specifiers declarator compound_statement
    {
      auto fn = std::make_unique<cAST::FunctionDecl>($1, $2);
      fn->set_body(std::move($3));
      $$ = std::move(fn);
    }
  ;

declaration_list
  : declaration
    {
      std::vector<std::unique_ptr<cAST::Decl>> v;
      v.emplace_back(std::move($1));
      $$ = std::move(v);
    }
  | declaration_list declaration
    {
      $1.emplace_back(std::move($2));
      $$ = std::move($1);
    }
  ;
%%

void cAST::cASTParser::error( const location_type &l, const std::string &err_message ) {
  std::cerr << "Error: " << err_message << " at " << l << "\n";
}

/* void cAST::cASTParser::error(const location_type& l, const std::string& m) {
  driver.report_error(l.begin.line, l.begin.column, m);
} */
