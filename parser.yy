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

%start translation_unit

%locations
%%
translation_unit: I_CONSTANT translation_unit {
                    driver.add_upper();
                    std::cout << "Parsed integer constant: " << $1 << std::endl;
                  }
                | F_CONSTANT translation_unit {
                    driver.add_upper();
                    std::cout << "Parsed floating constant: " << $1 << std::endl;
                  }
                | STRING_LITERAL translation_unit {
                    driver.add_upper();
                    std::cout << "Parsed string literal: " << $1 << std::endl;
                  }
                | END {
                    std::cout << "Reached end of file." << std::endl;
                  }
                ;
%%

void cAST::cASTParser::error( const location_type &l, const std::string &err_message ) {
  std::cerr << "Error: " << err_message << " at " << l << "\n";
}
