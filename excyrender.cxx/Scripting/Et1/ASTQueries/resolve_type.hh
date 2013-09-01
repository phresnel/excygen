// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef RESOLVE_TYPE_HH_INCLUDED_20130820
#define RESOLVE_TYPE_HH_INCLUDED_20130820

#include "../AST.hh"
#include <map>

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

    AST::Typeinfo resolve_type(shared_ptr<AST::ASTNode> ast);
    AST::Typeinfo resolve_type(AST::ASTNode const &ast);

    AST::Typeinfo resolve_type(shared_ptr<AST::ASTNode> ast, std::map<string,AST::Typeinfo> const &symbols);
    AST::Typeinfo resolve_type(AST::ASTNode const &ast, std::map<string,AST::Typeinfo> const &symbols);


    string resolve_type_raw(shared_ptr<AST::ASTNode> ast);
    string resolve_type_raw(AST::ASTNode const &ast);
    string resolve_type_raw(shared_ptr<AST::ASTNode> ast, std::map<string,AST::Typeinfo> const &symbols);
    string resolve_type_raw(AST::ASTNode const &ast, std::map<string,AST::Typeinfo> const &symbols);
} } } }

#endif // RESOLVE_TYPE_HH_INCLUDED_20130820
