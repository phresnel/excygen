// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef FIND_BINDING_NAMES_HH_INCLUDED_20130820
#define FIND_BINDING_NAMES_HH_INCLUDED_20130820

#include "../AST.hh"
#include <set>

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

    std::set<std::string> find_binding_names (shared_ptr<AST::ASTNode>, bool cross_bindings=true);
    std::set<std::string> find_binding_names (AST::ASTNode const &, bool cross_bindings=true);

} } } }

#endif // FIND_BINDING_NAMES_HH_INCLUDED_20130820
