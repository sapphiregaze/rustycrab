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
extern int yylex(Parser::semantic_type* yylval,
                 Parser::location_type* yylloc,
                 yyscan_t scanner,
                 Driver& driver);
extern int yylex_init_extra(void* user_defined, yyscan_t* scanner);
extern int yylex_destroy(yyscan_t scanner);
extern void yyset_in(FILE* in_str, yyscan_t scanner);

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

  Driver driver;
  driver.setFile(argv[1]);
  yyscan_t scanner;
  if (yylex_init_extra(&driver, &scanner)) {
    std::cerr << "Failed to initialize scanner\n";
    return 1;
  }
  yyset_in(in, scanner);

  Parser parser(scanner, driver);
  Parser::location_type loc;
  int rc = parser.parse();

  std::fclose(in);
  if (rc != 0) {
    std::cerr << "Parsing failed with code " << rc << "\n";
    return rc;
  }
  yylex_destroy(scanner);

  if (driver.tu) {
    print(*driver.tu, std::cout);
  } else {
    std::cerr << "No translation unit generated\n";
  }

  return 0;
}