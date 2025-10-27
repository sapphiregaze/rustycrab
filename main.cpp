#include <iostream>
#include <cstdlib>
#include <cstring>

#include "driver.hpp"

int main( const int argc, const char **argv ) {
  if (argc != 2) {
    return (EXIT_FAILURE);
  }

  if (std::strncmp(argv[1], "-h", 2) == 0) {
    std::cout << "just give a filename to count from a file\n";
    std::cout << "use -h to get this menu\n";
    return (EXIT_SUCCESS);
  }

  cAST::Driver driver;
  driver.parse(argv[1]);

  if (driver.had_error()) {
    driver.dump_errors(std::cerr);
    return 2;
  } else {
    std::cout << "Parse successful!" << std::endl;
  }

  driver.dump_ast(std::cout);

  return (EXIT_SUCCESS);
}