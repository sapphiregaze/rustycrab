%{
#include <cstring>
#include <string>
#include <climits>

/* Implementation of yyFlexScanner */
#include "scanner.hpp"
#include "ast.hpp"
#undef  YY_DECL
#define YY_DECL int cAST::cASTScanner::yylex( cAST::cASTParser::semantic_type * const lval, cAST::cASTParser::location_type *loc )

/* typedef to make the returns for the tokens shorter */
using token = cAST::cASTParser::token;

/* define yyterminate as this instead of NULL */
#define yyterminate() return( token::END )

/* msvc2010 requires that we exclude this header file. */
#define YY_NO_UNISTD_H

/* update location on matching */
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

static void comment(void);

static inline int to_int_checked(const char* s, int base) {
  errno = 0;
  long v = std::strtol(s, nullptr, base);
  if (errno == ERANGE || v < INT_MIN || v > INT_MAX) {
    // clamp or report via driver if you prefer
    if (v < INT_MIN) v = INT_MIN;
    if (v > INT_MAX) v = INT_MAX;
  }
  return static_cast<int>(v);
}

%}

%option debug
%option nodefault
%option yyclass="cAST::cASTScanner"
%option noyywrap
%option c++

O   [0-7]
D   [0-9]
NZ  [1-9]
L   [a-zA-Z_]
A   [a-zA-Z_0-9]
H   [a-fA-F0-9]
HP  (0[xX])
E   ([Ee][+-]?{D}+)
P   ([Pp][+-]?{D}+)
FS  (f|F|l|L)
IS  (((u|U)(l|L|ll|LL)?)|((l|L|ll|LL)(u|U)?))
CP  (u|U|L)
SP  (u8|u|U|L)
ES  (\\(['"\?\\abfnrtv]|[0-7]{1,3}|x[a-fA-F0-9]+))
WS  [ \t\v\n\f]

%%

%{ /** Code executed at the beginning of yylex **/
  yylval = lval;
%}

"/*"                                    { comment(); }
"//".*                                    { /* consume //-comment */ }

"auto"					{ return(token::AUTO); }
"break"					{ return(token::BREAK); }
"case"					{ return(token::CASE); }
"char"					{ return(token::CHAR); }
"const"					{ return(token::CONST); }
"continue"				{ return(token::CONTINUE); }
"default"				{ return(token::DEFAULT); }
"do"					{ return(token::DO); }
"double"				{ return(token::DOUBLE); }
"else"					{ return(token::ELSE); }
"enum"					{ return(token::ENUM); }
"extern"				{ return(token::EXTERN); }
"float"					{ return(token::FLOAT); }
"for"					{ return(token::FOR); }
"goto"					{ return(token::GOTO); }
"if"					{ return(token::IF); }
"inline"				{ return(token::INLINE); }
"int"					{ return(token::INT); }
"long"					{ return(token::LONG); }
"register"				{ return(token::REGISTER); }
"restrict"				{ return(token::RESTRICT); }
"return"				{ return(token::RETURN); }
"short"					{ return(token::SHORT); }
"signed"				{ return(token::SIGNED); }
"sizeof"				{ return(token::SIZEOF); }
"static"				{ return(token::STATIC); }
"struct"				{ return(token::STRUCT); }
"switch"				{ return(token::SWITCH); }
"typedef"				{ return(token::TYPEDEF); }
"union"					{ return(token::UNION); }
"unsigned"				{ return(token::UNSIGNED); }
"void"					{ return(token::VOID); }
"volatile"				{ return(token::VOLATILE); }
"while"					{ return(token::WHILE); }
"_Alignas"                              { return token::ALIGNAS; }
"_Alignof"                              { return token::ALIGNOF; }
"_Atomic"                               { return token::ATOMIC; }
"_Bool"                                 { return token::BOOL; }
"_Complex"                              { return token::COMPLEX; }
"_Generic"                              { return token::GENERIC; }
"_Imaginary"                            { return token::IMAGINARY; }
"_Noreturn"                             { return token::NORETURN; }
"_Static_assert"                        { return token::STATIC_ASSERT; }
"_Thread_local"                         { return token::THREAD_LOCAL; }
"__func__"                              { return token::FUNC_NAME; }

{L}{A}* {
    yylval->build<std::string>( yytext );
    return token::IDENTIFIER;
  }

{HP}{H}+{IS}? {
    yylval->build<int>( to_int_checked( yytext, 16 ) );
    return token::I_CONSTANT;
  }
{NZ}{D}*{IS}? {
    yylval->build<int>( to_int_checked( yytext, 10 ) );
    return token::I_CONSTANT;
  }
"0"{O}*{IS}? {
    yylval->build<int>( to_int_checked( YYText(), 8 ) );
    return token::I_CONSTANT;
  }
{CP}?"'"([^'\\\n]|{ES})+"'" {
    char* endptr;
    yylval->build<int>( static_cast<int>( std::strtol( yytext + 1, &endptr, 0 ) ) );
    return token::I_CONSTANT;
  }

{D}+{E}{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }
{D}*"."{D}+{E}?{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }
{D}+"."{E}?{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }
{HP}{H}+{P}{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }
{HP}{H}*"."{H}+{P}{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }
{HP}{H}+"."{P}{FS}? {
    yylval->build<double>( std::stod(yytext) );
    return token::F_CONSTANT;
  }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{
    yylval->build<std::string>( yytext );
    return token::STRING_LITERAL;
  }

"..."					{ return token::ELLIPSIS; }
">>="					{ return token::RIGHT_ASSIGN; }
"<<="					{ return token::LEFT_ASSIGN; }
"+="					{ return token::ADD_ASSIGN; }
"-="					{ return token::SUB_ASSIGN; }
"*="					{ return token::MUL_ASSIGN; }
"/="					{ return token::DIV_ASSIGN; }
"%="					{ return token::MOD_ASSIGN; }
"&="					{ return token::AND_ASSIGN; }
"^="					{ return token::XOR_ASSIGN; }
"|="					{ return token::OR_ASSIGN; }
">>"					{ return token::RIGHT_OP; }
"<<"					{ return token::LEFT_OP; }
"++"					{ return token::INC_OP; }
"--"					{ return token::DEC_OP; }
"->"					{ return token::PTR_OP; }
"&&"					{ return token::AND_OP; }
"||"					{ return token::OR_OP; }
"<="					{ return token::LE_OP; }
">="					{ return token::GE_OP; }
"=="					{ return token::EQ_OP; }
"!="					{ return token::NE_OP; }
";"					{ return ';'; }
("{"|"<%")				{ return '{'; }
("}"|"%>")				{ return '}'; }
","					{ return ','; }
":"					{ return ':'; }
"="					{ return '='; }
"("					{ return '('; }
")"					{ return ')'; }
("["|"<:")				{ return '['; }
("]"|":>")				{ return ']'; }
"."					{ return '.'; }
"&"					{ return '&'; }
"!"					{ return '!'; }
"~"					{ return '~'; }
"-"					{ return '-'; }
"+"					{ return '+'; }
"*"					{ return '*'; }
"/"					{ return '/'; }
"%"					{ return '%'; }
"<"					{ return '<'; }
">"					{ return '>'; }
"^"					{ return '^'; }
"|"					{ return '|'; }
"?"					{ return '?'; }

{WS}+					{ /* whitespace separates tokens */ }
.					{ /* discard bad characters */ }

%%

static void comment(void) {
  return;
}