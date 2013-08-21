// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef LIFT_IDENTIFIERS_TO_CALLS_HH_INCLUDED_20130820
#define LIFT_IDENTIFIERS_TO_CALLS_HH_INCLUDED_20130820

#include "../AST.hh"
#include <set>

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

    void lift_identifiers_to_calls(shared_ptr<AST::ASTNode> ast, std::set<std::string> const &which, bool cross_bindings=true);
    void lift_identifiers_to_calls(AST::ASTNode &ast, std::set<std::string> const &which, bool cross_bindings=true);

} } } }

#endif // LAMBDA_LIFT_HH_INCLUDED_20130820
