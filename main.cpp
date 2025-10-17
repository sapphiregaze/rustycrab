#include <iostream>

#ifdef _MSC_VER
#define YY_NO_UNISTD_H
#include <io.h>
#endif

#include "gen/parser.h"
#include "gen/scanner.h"

void lex(std::string);
void parse(std::string);
void yyerror(const char*);

int main(int argc, char* argv[]) {
  if (argc < 2) {
    std::cerr << "usage: " << argv[0] << " <file.c>\n";
    return 1;
  }

  FILE* in = std::fopen(argv[1], "rb");
  if (!in) {
    std::perror("fopen");
    return 1;
  }

  std::string readall = "";
  char buffer[4096];
  size_t n;
  while ((n = std::fread(buffer, 1, sizeof(buffer), in)) > 0) {
    readall.append(buffer, n);
  }

  std::cout << "=== Lexing ===" << std::endl;
  yy_scan_string(readall.c_str());
  yylex();
  std::cout << std::endl << std::endl;

  std::cout << "=== Parsing ===" << std::endl;
  parse(readall);
  std::cout << std::endl;

  return 0;
}
// void lex(std::string string) {
  yy_scan_string(string.c_str());

  int token;
  while ((token = yylex()) != 0) {
    switch (token) {
      break;
    case SEMI:
      std::cout << ";" << std::endl;
      break;
    case EQUAL:
      std::cout << "=" << " ";
      break;
    case PLUS:
      std::cout << "+" << " ";
      break;
    case MINUS:
      std::cout << "-" << " ";
      break;
    case MULT:
      std::cout << "*" << " ";
      break;
    case DIV:
      std::cout << "/" << " ";
      break;
    case INC:
      std::cout << "++" << " ";
      break;
    case DEC:
      std::cout << "--" << " ";
      break;
    case OPEN:
      std::cout << "(" << " ";
      break;
    case CLOSE:
      std::cout << ")" << " ";
      break;
    case FLOAT:
      std::cout << yylval.fval << " ";
      break;
    case IDENTIFIER:
      std::cout << yylval.sval << " ";
      break;
    default:
      std::cout << "!ERROR!" << std::endl;
    }
  }
}

void parse(std::string string) {
  yy_scan_string(string.c_str());
  yyparse();
}

void yyerror(const char *s) { std::cerr << s << std::endl; }
