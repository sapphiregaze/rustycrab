#include "semantics.hpp"

std::unique_ptr<Scope> current; // global for ownership of root
std::vector<std::unique_ptr<Scope>> scopeOwned; // owns all nested scopes
std::vector<Scope*> scopeStack; // lexical stack

SymbolTable::SymbolTable() {
  current = std::make_unique<Scope>();
  scopeStack.push_back(current.get());
}

void SymbolTable::pushScope() {
  auto child = std::make_unique<Scope>();
  child->parent = scopeStack.back();
  scopeOwned.push_back(std::move(child));
  scopeStack.push_back(scopeOwned.back().get());
}

void SymbolTable::popScope() {
  scopeStack.pop_back();
}

Scope* SymbolTable::cur() {
  return scopeStack.back();
}

// Insert helpers:
Symbol* SymbolTable::DeclareObject(
  const std::string& name,
  Type* type,
  SourceRange loc,
  bool is_definition=false,
  bool is_param=false
) {
  auto sym = std::make_unique<Symbol>();
  sym->name = name;
  sym->kind = SymbolKind::Object;
  sym->type = type;
  sym->loc = loc;
  sym->is_definition = is_definition;
  sym->is_parameter = is_param;

  auto* raw = sym.get();

  // check redeclaration rules *in this scope only*
  auto& bucket = cur()->objectsAndFuncs;
  if (bucket.find(name) != bucket.end()) {
      // emit "redefinition of 'name'" error
  } else {
    bucket[name] = std::move(sym);
  }

  return raw;
}

Symbol* SymbolTable::DeclareFunction(
  const std::string& name,
  Type* type,
  SourceRange loc,
  bool is_definition=false
) {
  auto sym = std::make_unique<Symbol>();
  sym->name = name;
  sym->kind = SymbolKind::Function;
  sym->type = type;
  sym->loc = loc;
  sym->is_definition = is_definition;
  // sym->is_parameter = is_param;

  auto* raw = sym.get();

  // check redeclaration rules *in this scope only*
  auto& bucket = cur()->objectsAndFuncs;
  if (bucket.find(name) != bucket.end()) {
      // emit "redefinition of 'name'" error
  } else {
    bucket[name] = std::move(sym);
  }

  return raw;
}

Symbol* SymbolTable::LookupObjectOrFunction(Scope* start, const std::string& name) {
  for (Scope* s = start; s != nullptr; s = s->parent) {
    auto it = s->objectsAndFuncs.find(name);
    if (it != s->objectsAndFuncs.end()) {
      return it->second.get();
    }
  }
  return nullptr;
}
