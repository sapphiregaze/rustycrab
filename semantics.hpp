#pragma once
#include <string>
#include <iostream>
#include <ostream>
#include <vector>
#include <memory>
#include <unordered_map>
#include <algorithm>
#include <cassert>

#include "ast.hpp"

class SymbolTable {
  public:
    SymbolTable();
    ~SymbolTable();

    void pushScope();
    void popScope();

    Symbol* DeclareObject(
      const std::string& name,
      Type* type,
      SourceRange loc,
      bool is_definition=false,
      bool is_param=false
    );

    Symbol* DeclareFunction(
      const std::string& name,
      Type* type,
      SourceRange loc,
      bool is_definition=false
      // bool is_param=false
    );

    Symbol* LookupObjectOrFunction(Scope* start, const std::string& name);

    Scope* cur();

    bool insert(std::unique_ptr<Symbol> symbol);
    Symbol* lookup(const std::string& name);

  private:
    std::vector<std::unique_ptr<Scope>> scopes;
};

struct Scope {
  Scope* parent = nullptr;

  std::unordered_map<std::string, std::unique_ptr<Symbol>> objectsAndFuncs;
  // not implementing
  // std::unordered_map<std::string, std::unique_ptr<Symbol>> tags;   // struct/union/enum tags
};

enum class SymbolKind {
  Object,
  Function,

  // not implementing
  // TypedefName,
  // Tag, (struct enum union)
  // Label (goto)
};

struct SourceRange {
  std::string filename;
  int start_line;
  int start_column;
  int end_line;
  int end_column;
};

struct Type; 
// Your type system node: pointer, array, function type, struct type, etc.
// You either already have this,
// or you'll need to build it from declarators.

struct Symbol {
  std::string name;
  SymbolKind kind;
  Type* type;
  SourceRange loc;

  bool is_definition = false;
  bool is_parameter = false;

  // not implementing
  // bool is_static = false;
  // bool is_extern = false;
  // bool is_member = false; // struct/union field

  cAST::ASTNode* ast_node = nullptr;
};
