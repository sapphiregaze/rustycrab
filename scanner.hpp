#pragma once

// #ifndef __MCSCANNER_HPP__
// #define __MCSCANNER_HPP__ 1

#if ! defined(yyFlexLexerOnce)
#include <FlexLexer.h>
#endif

#include "parser.tab.hh"
#include "location.hh"

namespace cAST{

class cASTScanner : public yyFlexLexer{
public:

  cASTScanner(std::istream *in) : yyFlexLexer(in) { };

  virtual ~cASTScanner() { };

   //get rid of override virtual function warning
   using FlexLexer::yylex;

   virtual
   int yylex( cAST::cASTParser::semantic_type * const lval, cAST::cASTParser::location_type *location );
   // YY_DECL defined in mc_lexer.l
   // Method body created by flex in mc_lexer.yy.cc

private:
   /* yyval ptr */
   cAST::cASTParser::semantic_type *yylval = nullptr;
};

} /* end namespace cAST */

// #endif /* END __MCSCANNER_HPP__ */
