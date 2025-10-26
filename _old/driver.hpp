#pragma once

#include "ast.h"
#include <string>
#include <memory>
#include "gen/parser.h"

namespace cparser {
  struct Driver {
    std::string file;
    std::unique_ptr<cAST::TranslationUnit> tu;

    Driver(): file(""), tu(nullptr) {}
    ~Driver() = default;

    void setFile(const std::string& f) {
      file = f;
    }

    static cAST::SourceRange toRange(const Parser::location_type& loc, const std::string& f) {
      cAST::SourceRange r;
      r.begin.file = f; r.end.file = f;
      r.begin.line = loc.begin.line;
      r.begin.column = loc.begin.column;
      r.end.line = loc.end.line;
      r.end.column = loc.end.column;
      return r;
    }
    template<class T>
    static void setLoc(
      std::unique_ptr<T>& n,
      const Parser::location_type& loc,
      const std::string& f
    ) {
      if(n) n->loc = toRange(loc, f);
    }
  };
}