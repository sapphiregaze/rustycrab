#include <iostream>
#include <cstdio>
#include "ast.h"
#include "driver.hpp"

#ifdef _MSC_VER
#define YY_NO_UNISTD_H
#include <io.h>
#endif

#include "gen/parser.h"
#include "gen/scanner.h"

using namespace cAST;

extern int yylex(cparser::Parser::semantic_type*,
                 cparser::Parser::location_type*,
                 yyscan_t scanner,
                 cparser::Driver& driver);

extern int yylex_destroy(yyscan_t scanner);
extern int yylex_init_extra(void* user_defined, yyscan_t* scanner);
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

  cparser::Driver driver;
  driver.setFile(argv[1]);

  yyscan_t scanner;
  if (yylex_init_extra(&driver, &scanner)) {
    std::cerr << "Failed to initialize scanner\n";
    std::fclose(in);
    return 1;
  }
  yyset_in(in, scanner);

  cparser::Parser parser(driver);
  parser.set_debug_level(0);

  int rc = parser.parse(scanner, driver);

  std::fclose(in);
  yylex_destroy(scanner);

  if (rc != 0) {
    std::cerr << "Parsing failed with code " << rc << "\n";
    return rc;
  }

  if (driver.tu) {
    print(*driver.tu, std::cout);
  } else {
    std::cerr << "No translation unit generated\n";
  }

  return 0;
}
