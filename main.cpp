#include <iostream>
#include <cstdlib>
#include <cstring>

#include "driver.hpp"

int main( const int argc, const char **argv ) {
  /** check for the right # of arguments **/
  if (argc != 2) {
    /** exit with failure condition **/
    return (EXIT_FAILURE);
  }

  if (std::strncmp(argv[1], "-h", 2) == 0) {
    /** simple help menu **/
    std::cout << "just give a filename to count from a file\n";
    std::cout << "use -h to get this menu\n";
    return (EXIT_SUCCESS);
  }

  cAST::Driver driver;

  driver.parse(argv[1]);

  if (driver.had_error()) {
    driver.dump_diagnostics(std::cerr);
    return 2;
  }

  driver.dump_ast(std::cout);
  // driver.print(std::cout) << "\n";

  return (EXIT_SUCCESS);
}