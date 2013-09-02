// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef EQUAL_HH_INCLUDED_20130902
#define EQUAL_HH_INCLUDED_20130902

#include "../AST.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

    bool equal(string const &lhs_code, string const &rhs_code);
    bool equal(AST::ASTNode const &lhs, AST::ASTNode const &rhs);

} } } }

#endif // EQUAL_HH_INCLUDED_20130902
