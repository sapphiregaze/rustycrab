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
%token <std::string> STRING_LITERAL IDENTIFIER

%token FUNC_NAME SIZEOF
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN
%token TYPEDEF_NAME ENUMERATION_CONSTANT

%token TYPEDEF EXTERN STATIC AUTO REGISTER INLINE
%token CONST RESTRICT VOLATILE
%token BOOL CHAR SHORT INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID
%token COMPLEX IMAGINARY
%token STRUCT UNION ENUM ELLIPSIS

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token ALIGNAS ALIGNOF ATOMIC GENERIC NORETURN STATIC_ASSERT THREAD_LOCAL

%type <std::unique_ptr<AST::Expr>>
  primary_expression constant string enumeration_constant generic_selection
  postfix_expression unary_expression cast_expression
  multiplicative_expression additive_expression shift_expression
  relational_expression equality_expression and_expression
  exclusive_or_expression inclusive_or_expression
  logical_and_expression logical_or_expression
  conditional_expression assignment_expression expression
  constant_expression

%type <std::vector<std::unique_ptr<AST::Expr>>> argument_expression_list initializer_list

%type <AST::UNARY_OPERATOR> unary_operator
%type <AST::AssignOp> assignment_operator
%type <AST::TYPE_STORAGE_QUALIFIER> storage_class_specifier

%type <std::unique_ptr<AST::Stmt>>
  statement labeled_statement compound_statement
  block_item expression_statement selection_statement
  iteration_statement jump_statement

%type <std::vector<std::unique_ptr<AST::Stmt>>> block_item_list

%type <std::unique_ptr<AST::Decl>> declaration init_declarator external_declaration function_definition
%type <std::vector<std::unique_ptr<AST::Decl>>> init_declarator_list declaration_list

%type <AST::DeclSpecs> declaration_specifiers specifier_qualifier_list
%type <std::unique_ptr<AST::TypeNode>> type_specifier type_name atomic_type_specifier struct_or_union_specifier
%type <AST::Declarator> declarator direct_declarator abstract_declarator direct_abstract_declarator pointer
%type <std::vector<std::unique_ptr<AST::ParamDecl>>> parameter_list parameter_type_list
%type <std::unique_ptr<AST::ParamDecl>> parameter_declaration

%type <std::vector<std::pair<std::unique_ptr<AST::TypeNode>, std::unique_ptr<AST::Expr>>>> generic_assoc_list
%type <std::pair<std::unique_ptr<AST::TypeNode>, std::unique_ptr<AST::Expr>>> generic_association

%type <std::vector<std::string>> identifier_list

%start translation_unit

%locations
%%
primary_expression
  : IDENTIFIER { $$ = std::move($1); }
  | constant { $$ = std::move($1); }
  | string { $$ = std::move($1); }
  | '(' expression ')' { $$ = std::move($2); }
  | generic_selection { $$ = std::move($1); }
  ;

constant
  : I_CONSTANT { $$ = std::move($1); }
  | F_CONSTANT { $$ = std::move($1); }
  | ENUMERATION_CONSTANT { $$ = std::move($1); }
  ;

enumeration_constant
  : IDENTIFIER { $$ = std::move($1); }
  ;

string
  : STRING_LITERAL { $$ = std::move($1); }
  | FUNC_NAME { $$ = std::move($1); }
  ;

generic_selection
  : GENERIC '(' assignment_expression ',' generic_assoc_list ')'
    {
      // generic_assoc_list packs pairs of (type, expr); default is handled as {nullptr, expr}
      std::vector<std::pair<std::unique_ptr<AST::TypeNode>, std::unique_ptr<AST::Expr>>> assocs;
      std::unique_ptr<AST::Expr> def;
      for (auto &p : $5) {
        if (p.first) assocs.emplace_back(std::move(p));
        else def = std::move(p.second);
      }
      $$ = ast.makeGenericSelection(std::move($3), std::move(assocs), std::move(def), @1);
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
  | postfix_expression '[' expression ']'       { $$ = ast.makeSubscript(std::move($1), std::move($3), @2); }
  | postfix_expression '(' ')'                  { $$ = ast.makeCall(std::move($1), {}, @2); }
  | postfix_expression '(' argument_expression_list ')'
                                                { $$ = ast.makeCall(std::move($1), std::move($3), @2); }
  | postfix_expression '.' IDENTIFIER           { $$ = ast.makeMember(std::move($1), *$3, false, @2); }
  | postfix_expression PTR_OP IDENTIFIER        { $$ = ast.makeMember(std::move($1), *$3, true,  @2); }
  | postfix_expression INC_OP                   { $$ = ast.makeUnary(AST::UnaryOp::PostInc, std::move($1), @2); }
  | postfix_expression DEC_OP                   { $$ = ast.makeUnary(AST::UnaryOp::PostDec, std::move($1), @2); }
  | '(' type_name ')' '{' initializer_list '}'  { $$ = ast.makeCompoundLiteral(std::move($2), ast.makeInitList(std::move($5), @4), @1); }
  | '(' type_name ')' '{' initializer_list ',' '}' { $$ = ast.makeCompoundLiteral(std::move($2), ast.makeInitList(std::move($5), @4), @1); }
  ;

argument_expression_list
  : assignment_expression                       { $$ = ast.singleton(std::move($1)); }
  | argument_expression_list ',' assignment_expression
                                                { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

unary_expression
  : postfix_expression                           { $$ = std::move($1); }
  | INC_OP unary_expression                      { $$ = ast.makeUnary(AST::UnaryOp::PreInc, std::move($2), @1); }
  | DEC_OP unary_expression                      { $$ = ast.makeUnary(AST::UnaryOp::PreDec, std::move($2), @1); }
  | unary_operator cast_expression               { $$ = ast.makeUnary($1, std::move($2), @1); }
  | SIZEOF unary_expression                      { /* make it a builtin unary if you model it */ $$ = ast.makeUnary(AST::UnaryOp::SizeofExpr /* custom */, std::move($2), @1); }
  | SIZEOF '(' type_name ')'                     { $$ = ast.makeCast(/*sizeof-type as expr*/ std::move($3), nullptr, @1); /* or a dedicated node */ }
  | ALIGNOF '(' type_name ')'                    { $$ = ast.makeCast(/*alignof-type expr*/ std::move($3), nullptr, @1); }
  ;

unary_operator
  : '&' { $$ = AST::UNARY_OPERATOR::ADDRESS_OF; }
  | '*' { $$ = AST::UNARY_OPERATOR::DEREFERENCE; }
  | '+' { $$ = AST::UNARY_OPERATOR::PLUS; }
  | '-' { $$ = AST::UNARY_OPERATOR::MINUS; }
  | '~' { $$ = AST::UNARY_OPERATOR::BITWISE_NOT; }
  | '!' { $$ = AST::UNARY_OPERATOR::LOGICAL_NOT; }
  ;

cast_expression
  : unary_expression                              { $$ = std::move($1); }
  | '(' type_name ')' cast_expression             { $$ = ast.makeCast(std::move($2), std::move($4), @1); }
  ;

multiplicative_expression
  : cast_expression                              { $$ = std::move($1); }
  | multiplicative_expression '*' cast_expression{ $$ = ast.makeBinary(AST::BinOp::Mul, std::move($1), std::move($3), @2); }
  | multiplicative_expression '/' cast_expression{ $$ = ast.makeBinary(AST::BinOp::Div, std::move($1), std::move($3), @2); }
  | multiplicative_expression '%' cast_expression{ $$ = ast.makeBinary(AST::BinOp::Mod, std::move($1), std::move($3), @2); }
  ;

additive_expression
  : multiplicative_expression                    { $$ = std::move($1); }
  | additive_expression '+' multiplicative_expression
                                                { $$ = ast.makeBinary(AST::BinOp::Add, std::move($1), std::move($3), @2); }
  | additive_expression '-' multiplicative_expression
                                                { $$ = ast.makeBinary(AST::BinOp::Sub, std::move($1), std::move($3), @2); }
  ;

shift_expression
  : additive_expression                          { $$ = std::move($1); }
  | shift_expression LEFT_OP additive_expression { $$ = ast.makeBinary(AST::BinOp::Shl, std::move($1), std::move($3), @2); }
  | shift_expression RIGHT_OP additive_expression{ $$ = ast.makeBinary(AST::BinOp::Shr, std::move($1), std::move($3), @2); }
  ;

relational_expression
  : shift_expression                             { $$ = std::move($1); }
  | relational_expression '<' shift_expression   { $$ = ast.makeBinary(AST::BinOp::Lt, std::move($1), std::move($3), @2); }
  | relational_expression '>' shift_expression   { $$ = ast.makeBinary(AST::BinOp::Gt, std::move($1), std::move($3), @2); }
  | relational_expression LE_OP shift_expression { $$ = ast.makeBinary(AST::BinOp::Le, std::move($1), std::move($3), @2); }
  | relational_expression GE_OP shift_expression { $$ = ast.makeBinary(AST::BinOp::Ge, std::move($1), std::move($3), @2); }
  ;

equality_expression
  : relational_expression                        { $$ = std::move($1); }
  | equality_expression EQ_OP relational_expression
                                                { $$ = ast.makeBinary(AST::BinOp::Eq, std::move($1), std::move($3), @2); }
  | equality_expression NE_OP relational_expression
                                                { $$ = ast.makeBinary(AST::BinOp::Ne, std::move($1), std::move($3), @2); }
  ;

and_expression
  : equality_expression                          { $$ = std::move($1); }
  | and_expression '&' equality_expression       { $$ = ast.makeBinary(AST::BinOp::BitAnd, std::move($1), std::move($3), @2); }
  ;

exclusive_or_expression
  : and_expression                               { $$ = std::move($1); }
  | exclusive_or_expression '^' and_expression   { $$ = ast.makeBinary(AST::BinOp::BitXor, std::move($1), std::move($3), @2); }
  ;

inclusive_or_expression
  : exclusive_or_expression                      { $$ = std::move($1); }
  | inclusive_or_expression '|' exclusive_or_expression
                                                { $$ = ast.makeBinary(AST::BinOp::BitOr, std::move($1), std::move($3), @2); }
  ;

logical_and_expression
  : inclusive_or_expression                      { $$ = std::move($1); }
  | logical_and_expression AND_OP inclusive_or_expression
                                                { $$ = ast.makeBinary(AST::BinOp::LogAnd, std::move($1), std::move($3), @2); }
  ;

logical_or_expression
  : logical_and_expression                       { $$ = std::move($1); }
  | logical_or_expression OR_OP logical_and_expression
                                                { $$ = ast.makeBinary(AST::BinOp::LogOr, std::move($1), std::move($3), @2); }
  ;

conditional_expression
  : logical_or_expression                        { $$ = std::move($1); }
  | logical_or_expression '?' expression ':' conditional_expression
                                                { $$ = ast.makeCond(std::move($1), std::move($3), std::move($5), @2); }
  ;

assignment_operator
  : '=' { $$ = AST::AssignOp::Assign; }
  | MUL_ASSIGN { $$ = AST::AssignOp::Mul; }
  | DIV_ASSIGN { $$ = AST::AssignOp::Div; }
  | MOD_ASSIGN { $$ = AST::AssignOp::Mod; }
  | ADD_ASSIGN { $$ = AST::AssignOp::Add; }
  | SUB_ASSIGN { $$ = AST::AssignOp::Sub; }
  | LEFT_ASSIGN { $$ = AST::AssignOp::Shl; }
  | RIGHT_ASSIGN { $$ = AST::AssignOp::Shr; }
  | AND_ASSIGN { $$ = AST::AssignOp::And; }
  | XOR_ASSIGN { $$ = AST::AssignOp::Xor; }
  | OR_ASSIGN { $$ = AST::AssignOp::Or; }
  ;

assignment_expression
  : conditional_expression                        { $$ = std::move($1); }
  | unary_expression assignment_operator assignment_expression
                                                 { $$ = ast.makeAssign($2, std::move($1), std::move($3), @2); }
  ;

expression
  : assignment_expression                         { $$ = std::move($1); }
  | expression ',' assignment_expression          { $$ = ast.makeBinary(AST::BinOp::Comma, std::move($1), std::move($3), @2); }
  ;

constant_expression
  : conditional_expression                        { $$ = std::move($1); }
  ;

declaration
  : declaration_specifiers ';'
      {
        // e.g., "int ;" — not valid in ISO C, but keep node if you want
        $$ = nullptr; /* or create a dummy Decl */
      }
  | declaration_specifiers init_declarator_list ';'
      {
        // Expand list into either a DeclStmt with multiple VarDecls,
        // or if a function with body appears elsewhere (function_definition rule).
        // Here we return a single Decl that wraps many (DeclStmt) or the first, per your AST design.
        // Minimal: return the first Decl; push others via driver/builder outside.
        $$ = std::move($2.front()); // skeleton; adapt to your TU collector
        $2.erase($2.begin());
        for (auto &more : $2) {
          /* ast.enqueueExternal(std::move(more)); */ /* or push into TU later */
        }
      }
  | static_assert_declaration       { $$ = std::move($1); } /* if you model it as a Decl */
  ;

declaration_specifiers
  : type_specifier declaration_specifiers
      { $$ = ast.combineSpecs(ast.makeSpecsFromType(std::move($1)), $2); }
  | type_specifier
      { $$ = ast.makeSpecsFromType(std::move($1)); }
  | type_qualifier declaration_specifiers
      { $$ = ast.combineSpecs(ast.makeSpecsFromType(/*qual to type*/ ast.makeBuiltinType(/*qual-as-type or wrap*/, @1.first_line)), $2); /* or fold into specs.tq */ }
  | type_qualifier
      { $$ = ast.makeSpecsFromType(/*qual*/ ast.makeBuiltinType(/*...*/, @1.first_line)); }
  /*storage wraps*/
  /* | storage_class_specifier declaration_specifiers */
      /* { $$ = ast.combineSpecs(ast.makeSpecsFromType( nullptr), $2); } */
  /*storage*/
  /* | storage_class_specifier */
      /* { $$ = ast.makeSpecsFromType(nullptr); } */
  | function_specifier declaration_specifiers
      { $$ = ast.combineSpecs(ast.makeSpecsFromType(/*fn spec*/ nullptr), $2); }
  | function_specifier
      { $$ = ast.makeSpecsFromType(/*fn spec*/ nullptr); }
  /* | alignment_specifier declaration_specifiers */
      /*align*/
      /* { $$ = ast.combineSpecs(ast.makeSpecsFromType( nullptr), $2); } */
      /*align*/
  /* | alignment_specifier */
      /* { $$ = ast.makeSpecsFromType( nullptr); } */
  ;

init_declarator_list
  : init_declarator                         { $$ = std::vector{ std::move($1) }; }
  | init_declarator_list ',' init_declarator
                                           { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

init_declarator
  : declarator '=' initializer
      {
        // Var or function declarator with initializer (functions can’t init here, so it’s a VarDecl)
        $$ = ast.makeVarDecl(/*specs must be threaded from context — see typical C grammar split*/,
                             $1, /*init*/ std::move($3), @2);
        /* In standard C grammars, specs + list are in the same production; if you keep them split,
           capture 'current specs' in parser state or pass them down via midrule. */
      }
  | declarator
      {
        // No init; could be a function proto or a variable
        $$ = ast.makeVarDecl(/*specs*/, $1, /*init*/ nullptr, @1);
      }
  ;

storage_class_specifier
	: TYPEDEF	{ $$ = AST::TYPE_STORAGE_QUALIFIER::Typedef; }
	| EXTERN	{ $$ = AST::TYPE_STORAGE_QUALIFIER::Extern; }
	| STATIC	{ $$ = AST::TYPE_STORAGE_QUALIFIER::Static; }
	| THREAD_LOCAL { $$ = AST::TYPE_STORAGE_QUALIFIER::Thread_Local; }
	| AUTO  { $$ = AST::TYPE_STORAGE_QUALIFIER::Auto; }
	| REGISTER { $$ = AST::TYPE_STORAGE_QUALIFIER::Register; }
	;

type_specifier
	: VOID { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Void, @1.first_line); }
	| CHAR { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Char, @1.first_line); }
	/* | SHORT { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Short, @1.first_line); } */
	| INT { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Int, @1.first_line); }
	| LONG { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Long, @1.first_line); }
	| FLOAT { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Float, @1.first_line); }
	| DOUBLE { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Double, @1.first_line); }
	/* | SIGNED { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Signed, @1.first_line); } */
	/* | UNSIGNED { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Unsigned, @1.first_line); } */
	| BOOL { $$ = driver.makeBuiltinType(AST::BUILTIN_TYPE::Bool, @1.first_line); }
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
        $$ = ast.makeStructUnion(nullptr, $1, std::move($3), @2);
      }
	| struct_or_union IDENTIFIER '{' struct_declaration_list '}' {
        // named struct/union
        $$ = ast.makeStructUnion(std::move($2), $1, std::move($4), @2);
      }
	| struct_or_union IDENTIFIER {
        // forward declaration
        $$ = ast.makeStructUnion(std::move($2), $1, {}, @1);
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

enumerator
	: enumeration_constant '=' constant_expression
	| enumeration_constant
	;


atomic_type_specifier
	: ATOMIC '(' type_name ')'
	; */

type_qualifier
	: CONST
	| RESTRICT
	| VOLATILE
	| ATOMIC
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
  : pointer direct_declarator           { $$ = ast.wrapPointer($2, @1); /* wrap chain already in $1 or combine both; adjust */ }
  | direct_declarator                   { $$ = std::move($1); }
  ;

direct_declarator
	: IDENTIFIER { $$ = ast.makeIdentDeclarator(*$1, @1); }
	| '(' declarator ')' { $$ = std::move($2); }
	| direct_declarator '[' ']' { $$ = ast.wrapArray($1, /*size*/ nullptr, @2); }
	/* | direct_declarator '[' '*' ']' { $$ = ast.wrapArray($1, nullptr, @2); } */
	/* | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']' { $$ = ast.wrapArray($1, std::move($4), @2); } */
	/* | direct_declarator '[' STATIC assignment_expression ']' { $$ = ast.wrapArray($1, std::move($3), @2); } */
	/* | direct_declarator '[' type_qualifier_list '*' ']' { $$ = ast.wrapArray($1, nullptr, @2); } */
	/* | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']' { $$ = ast.wrapArray($1, std::move($4), @2); } */
	| direct_declarator '[' type_qualifier_list assignment_expression ']' { $$ = ast.wrapArray($1, std::move($4), @2); }
  // unsized
	/* | direct_declarator '[' type_qualifier_list ']' { $$ = ast.wrapArray($1, nullptr, @2); } */
	| direct_declarator '[' assignment_expression ']' { $$ = ast.wrapArray($1, std::move($3), @2); }
  // variadic set in list
	/* | direct_declarator '(' parameter_type_list ')' { $$ = ast.wrapFunction($1, std::move($3), , @2); } */
	| direct_declarator '(' ')' { $$ = ast.wrapFunction($1, {}, /*variadic*/ false, @2); }
	/* K&R parameters — map to ParamDecl with 'int' default or leave TODO */
  /* | direct_declarator '(' identifier_list ')' {  $$ = ast.wrapFunction($1, {TODO}, false, @2); } */

pointer
// fold quals into inner then wrap
  /* : '*' type_qualifier_list pointer     { $$ = $3; $$ = ast.wrapPointer($$, @1); } */
  | '*' type_qualifier_list             { $$ = ast.wrapPointer(AST::Declarator{}, @1); }
  | '*' pointer                         { $$ = ast.wrapPointer($2, @1); }
  | '*'                                 { $$ = ast.wrapPointer(AST::Declarator{}, @1); }
  ;

parameter_type_list
/* mark variadic true in wrapFunction site */
  /* : parameter_list ',' ELLIPSIS         { $$ = std::move($1);  } */
  | parameter_list                      { $$ = std::move($1); }
  ;

parameter_list
  : parameter_declaration               { $$ = std::vector{ std::move($1) }; }
  | parameter_list ',' parameter_declaration
                                        { $1.emplace_back(std::move($3)); $$ = std::move($1); }
  ;

parameter_declaration
  : declaration_specifiers declarator
      { $$ = ast.makeParam($1, $2, @2); }
  | declaration_specifiers abstract_declarator
      { $$ = ast.makeParam($1, $2, @2); }
  | declaration_specifiers
      { $$ = ast.makeParam($1, AST::Declarator{}, @1); }
  ;

type_name : specifier_qualifier_list { $$ = std::move($1.type); }
  /* apply abstract declarator to base type into a TypeNode */
  /* : specifier_qualifier_list abstract_declarator {$$ = ast.applyAbstract($1, $2) std::move($1.type); } */
  ;

type_qualifier_list
	: type_qualifier
	| type_qualifier_list type_qualifier
	;

identifier_list
	: IDENTIFIER
	| identifier_list ',' IDENTIFIER
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
  /* : '{' initializer_list '}'           { $$ = ast.makeInitList(std::move($2), @1); }
  | '{' initializer_list ',' '}'       { $$ = ast.makeInitList(std::move($2), @1); } */
  ;

initializer_list
	: designation initializer
	| initializer { $$ = ast.singleton(std::move($1)); }
	/* | initializer_list ',' designation initializer
	| initializer_list ',' initializer { $1.emplace_back(std::move($3)); $$ = std::move($1); } */
	;

/* You can extend InitListExpr to carry designators if you model them */
designation : designator_list '='     { /* set a pending designator context in builder if needed */ }
  ;

designator_list
  : designator
  | designator_list designator
  ;

designator
  : '[' constant_expression ']'        { /* record array designator */ }
  | '.' IDENTIFIER                     { /* record field designator */ }
  ;

static_assert_declaration
	: STATIC_ASSERT '(' constant_expression ',' STRING_LITERAL ')' ';'
	;

statement
  : labeled_statement               { $$ = std::move($1); }
  | compound_statement              { $$ = std::move($1); }
  | expression_statement            { $$ = std::move($1); }
  | selection_statement             { $$ = std::move($1); }
  | iteration_statement             { $$ = std::move($1); }
  | jump_statement                  { $$ = std::move($1); }
  ;

labeled_statement
  : IDENTIFIER ':' statement        { $$ = ast.makeLabel(*$1, std::move($3), @1); }
  | CASE constant_expression ':' statement
                                   { $$ = ast.makeCase(std::move($2), std::move($4), @1); }
  | DEFAULT ':' statement          { $$ = ast.makeDefault(std::move($3), @1); }
  ;

compound_statement
  : '{' '}'                        { $$ = ast.makeCompoundStmt({}, @1); }
  | '{' block_item_list '}'        { $$ = ast.makeCompoundStmt(std::move($2), @1); }
  ;

block_item_list
  : block_item                     { $$ = ast.singleton(std::move($1)); }
  | block_item_list block_item     { $1.emplace_back(std::move($2)); $$ = std::move($1); }
  ;

block_item
  : declaration                    { $$ = std::make_unique<AST::DeclStmt>(/*wrap*/ std::move($1)); /* or emit multiple if decl expands */ }
  | statement                      { $$ = std::move($1); }
  ;

expression_statement
  : ';'                            { $$ = ast.makeNullStmt(@1); }
  | expression ';'                 { $$ = ast.makeExprStmt(std::move($1), @1); }
  ;

selection_statement
  : IF '(' expression ')' statement ELSE statement
                                   { $$ = ast.makeIf(std::move($3), std::move($5), std::move($7), @1); }
  | IF '(' expression ')' statement
                                   { $$ = ast.makeIf(std::move($3), std::move($5), nullptr, @1); }
  | SWITCH '(' expression ')' statement
                                   { $$ = ast.makeSwitch(std::move($3), std::move($5), @1); }
  ;

iteration_statement
  : WHILE '(' expression ')' statement
                                   { $$ = ast.makeWhile(std::move($3), std::move($5), @1); }
  | DO statement WHILE '(' expression ')' ';'
                                   { $$ = ast.makeDoWhile(std::move($2), std::move($5), @1); }
  | FOR '(' expression_statement expression_statement ')' statement
                                   { $$ = ast.makeFor(std::move($3), /*cond from exprstmt*/ nullptr, /*step*/ nullptr, std::move($6), @1); /* refine: pull expr from exprstmt nodes */ }
  | FOR '(' expression_statement expression_statement expression ')' statement
                                   { $$ = ast.makeFor(std::move($3), /*cond*/ nullptr, std::move($5), std::move($7), @1); }
  | FOR '(' declaration expression_statement ')' statement
                                   { $$ = ast.makeFor(std::make_unique<AST::DeclStmt>(std::move($3)), /*cond*/ nullptr, /*step*/ nullptr, std::move($6), @1); }
  | FOR '(' declaration expression_statement expression ')' statement
                                   { $$ = ast.makeFor(std::make_unique<AST::DeclStmt>(std::move($3)), /*cond*/ nullptr, std::move($5), std::move($7), @1); }
  ;

jump_statement
  /* : GOTO IDENTIFIER ';'            { $$ = ast.makeGoto(*$2, @1); } */
  | CONTINUE ';'                   { $$ = ast.makeContinue(@1); }
  | BREAK ';'                      { $$ = ast.makeBreak(@1); }
  | RETURN ';'                     { $$ = ast.makeReturn(nullptr, @1); }
  | RETURN expression ';'          { $$ = ast.makeReturn(std::move($2), @1); }
  ;

translation_unit
	: external_declaration {
        driver.head_->declarations.push_back($1);
      }
	| translation_unit external_declaration {
        driver.head_->declarations.push_back($2);
      }
	;

external_declaration
  : function_definition        { $$ = std::move($1); }
  | declaration                { $$ = std::move($1); }  /* declaration also returns unique_ptr<AST::Decl> */
  ;

function_definition
  : declaration_specifiers declarator declaration_list compound_statement
    {
      // Build AST::FunctionDecl(specs, declarator, body, locals = $3)
      auto fn = std::make_unique<AST::FunctionDecl>(/* use $1, $2 */);
      fn->set_body(std::move($4));                 // if your body is a Stmt unique_ptr
      fn->set_params(std::move($3));               // vector<unique_ptr<ParamDecl>>
      $$ = std::move(fn);                          // unique_ptr<Decl>
    }
  | declaration_specifiers declarator compound_statement
    {
      auto fn = std::make_unique<AST::FunctionDecl>(/* use $1, $2 */);
      fn->set_body(std::move($3));
      $$ = std::move(fn);                          // unique_ptr<Decl>
    }
  ;

declaration_list
  : declaration
    {
      std::vector<std::unique_ptr<AST::Decl>> v;
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
