// (C) 2013 Sebastian Mach (1983), this file is published under the terms of the
// GNU General Public License, Version 3 (a.k.a. GPLv3).
// See COPYING in the root-folder of the excygen project folder.
#ifndef UNNEST_LETINS_HH_INCLUDED_20130910
#define UNNEST_LETINS_HH_INCLUDED_20130910

#include "../AST.hh"

namespace excyrender { namespace Nature { namespace Et1 { namespace ASTPasses {

    void unnest_letins(shared_ptr<AST::ASTNode> ast);

} } } }

#endif // UNNEST_LETINS_HH_INCLUDED_20130910
