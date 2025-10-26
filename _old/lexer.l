%option c++
%option reentrant
%option bison-bridge
%option bison-locations
%option noyywrap
%option nodefault
%option nounput noinput
%option yylineno
%option prefix="yy"

%option extra-type="cparser::Driver*"

%e  1019
%p  2807
%n  371
%k  284
%a  1213
%o  1117

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

%{
#include <string>
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "parser.h"
// #include "driver.hpp"

using cparser::Parser;

// #define YY_USER_ACTION    yylloc->columns(yyleng);

// static inline cparser::Driver& DRV(yyscan_t scanner) {
//   return *yyget_extra(scanner);
// }

// extern void yyerror(const char *);  /* prints grammar violation message */

extern int sym_type(const char *);  /* returns type from symbol table */

#define sym_type(identifier) Parser::token::IDENTIFIER /* with no symbol table, fake it */

static void comment(void);
static int check_type(void);
%}

%%
"/*"                                    { comment(); }
"//".*                                    { /* consume //-comment */ }

"auto"					{ return(Parser::token::AUTO); }
"break"					{ return(Parser::token::BREAK); }
"case"					{ return(Parser::token::CASE); }
"char"					{ return(Parser::token::CHAR); }
"const"					{ return(Parser::token::CONST); }
"continue"				{ return(Parser::token::CONTINUE); }
"default"				{ return(Parser::token::DEFAULT); }
"do"					{ return(Parser::token::DO); }
"double"				{ return(Parser::token::DOUBLE); }
"else"					{ return(Parser::token::ELSE); }
"enum"					{ return(Parser::token::ENUM); }
"extern"				{ return(Parser::token::EXTERN); }
"float"					{ return(Parser::token::FLOAT); }
"for"					{ return(Parser::token::FOR); }
"goto"					{ return(Parser::token::GOTO); }
"if"					{ return(Parser::token::IF); }
"inline"				{ return(Parser::token::INLINE); }
"int"					{ return(Parser::token::INT); }
"long"					{ return(Parser::token::LONG); }
"register"				{ return(Parser::token::REGISTER); }
"restrict"				{ return(Parser::token::RESTRICT); }
"return"				{ return(Parser::token::RETURN); }
"short"					{ return(Parser::token::SHORT); }
"signed"				{ return(Parser::token::SIGNED); }
"sizeof"				{ return(Parser::token::SIZEOF); }
"static"				{ return(Parser::token::STATIC); }
"struct"				{ return(Parser::token::STRUCT); }
"switch"				{ return(Parser::token::SWITCH); }
"typedef"				{ return(Parser::token::TYPEDEF); }
"union"					{ return(Parser::token::UNION); }
"unsigned"				{ return(Parser::token::UNSIGNED); }
"void"					{ return(Parser::token::VOID); }
"volatile"				{ return(Parser::token::VOLATILE); }
"while"					{ return(Parser::token::WHILE); }
"_Alignas"                              { return(Parser::token::ALIGNAS); }
"_Alignof"                              { return(Parser::token::ALIGNOF); }
"_Atomic"                               { return(Parser::token::ATOMIC); }
"_Bool"                                 { return(Parser::token::BOOL); }
"_Complex"                              { return(Parser::token::COMPLEX); }
"_Generic"                              { return(Parser::token::GENERIC); }
"_Imaginary"                            { return(Parser::token::IMAGINARY); }
"_Noreturn"                             { return(Parser::token::NORETURN); }
"_Static_assert"                        { return(Parser::token::STATIC_ASSERT); }
"_Thread_local"                         { return(Parser::token::THREAD_LOCAL); }
"__func__"                              { return(Parser::token::FUNC_NAME); }

{L}{A}*					{ return check_type(); }

{HP}{H}+{IS}?				{ return(Parser::token::I_CONSTANT); }
{NZ}{D}*{IS}?				{ return(Parser::token::I_CONSTANT); }
"0"{O}*{IS}?				{ return(Parser::token::I_CONSTANT); }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ return(Parser::token::I_CONSTANT); }

{D}+{E}{FS}?				{ return(Parser::token::F_CONSTANT); }
{D}*"."{D}+{E}?{FS}?			{ return(Parser::token::F_CONSTANT); }
{D}+"."{E}?{FS}?			{ return(Parser::token::F_CONSTANT); }
{HP}{H}+{P}{FS}?			{ return(Parser::token::F_CONSTANT); }
{HP}{H}*"."{H}+{P}{FS}?			{ return(Parser::token::F_CONSTANT); }
{HP}{H}+"."{P}{FS}?			{ return(Parser::token::F_CONSTANT); }

({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ return(Parser::token::STRING_LITERAL); }

"..."					{ return(Parser::token::ELLIPSIS); }
">>="					{ return(Parser::token::RIGHT_ASSIGN); }
"<<="					{ return(Parser::token::LEFT_ASSIGN); }
"+="					{ return(Parser::token::ADD_ASSIGN); }
"-="					{ return(Parser::token::SUB_ASSIGN); }
"*="					{ return(Parser::token::MUL_ASSIGN); }
"/="					{ return(Parser::token::DIV_ASSIGN); }
"%="					{ return(Parser::token::MOD_ASSIGN); }
"&="					{ return(Parser::token::AND_ASSIGN); }
"^="					{ return(Parser::token::XOR_ASSIGN); }
"|="					{ return(Parser::token::OR_ASSIGN); }
">>"					{ return(Parser::token::RIGHT_OP); }
"<<"					{ return(Parser::token::LEFT_OP); }
"++"					{ return(Parser::token::INC_OP); }
"--"					{ return(Parser::token::DEC_OP); }
"->"					{ return(Parser::token::PTR_OP); }
"&&"					{ return(Parser::token::AND_OP); }
"||"					{ return(Parser::token::OR_OP); }
"<="					{ return(Parser::token::LE_OP); }
">="					{ return(Parser::token::GE_OP); }
"=="					{ return(Parser::token::EQ_OP); }
"!="					{ return(Parser::token::NE_OP); }
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

int yywrap(void)        /* called at end of input */
{
    return 1;           /* terminate now */
}

static void comment(void)
{
    int c;

    while ((c = yyinput()) != 0)
        if (c == '*')
        {
            while ((c = yyinput()) == '*')
                ;

            if (c == '/')
                return;

            if (c == 0)
                break;
        }
    // Parser::location_type& loc = cparser::Parser::yyloc;
    // Parser::error(loc, "unterminated comment");
    // yyerror("unterminated comment");
}

static int check_type(void)
{
    switch (sym_type(yytext))
    {
      /* previously defined */
      case Parser::token::TYPEDEF_NAME:
        return Parser::token::TYPEDEF_NAME;

      /* previously defined */
      case Parser::token::ENUMERATION_CONSTANT:
        return Parser::token::ENUMERATION_CONSTANT;

      /* includes undefined */
      default:
        return Parser::token::IDENTIFIER;
    }
}