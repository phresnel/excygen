// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef HAS_UNRESOLVED_HH_INCLUDED_20130901
#define HAS_UNRESOLVED_HH_INCLUDED_20130901

#include "../AST.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTQueries {

    bool has_unresolved (shared_ptr<AST::ASTNode>);
    bool has_unresolved (AST::ASTNode const &);

} } } }

#endif // HAS_UNRESOLVED_HH_INCLUDED_20130901
