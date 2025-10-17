cmake_minimum_required(VERSION 3.21)
project(FlexBisonTemplate)

include(scripts/flex-bison.cmake)
fetch_flex_bison()

add_flex_target(run_flex ${CMAKE_SOURCE_DIR}/lexer.l OUTPUT ${CMAKE_SOURCE_DIR}/gen/scanner.h)
add_bison_target(run_bison ${CMAKE_SOURCE_DIR}/parser.y OUTPUT ${CMAKE_SOURCE_DIR}/gen/parser.h)

add_executable(rc
  main.cpp
  ast.cpp
)
add_dependencies(rc run_flex run_bison)
